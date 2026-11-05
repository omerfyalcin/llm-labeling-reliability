# A small DuckDB cache for LLM labels, so that re-rendering the slides never
# repeats (or changes) a run that has already been done.
#
# Each row is one label for one tweet, from one run of one configuration. A
# configuration is everything that is sent to the API: model, system prompt,
# parameters, provider settings, and the output type. The cache key depends only
# on these and on the tweet text, not on the R code that produced them.

library(DBI)

cache_path <- "llm_cache.duckdb"

cache_connect <- function() {
  con <- dbConnect(duckdb::duckdb(), cache_path)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS labels (
      key VARCHAR, run INTEGER, tweet_id VARCHAR, label VARCHAR,
      model VARCHAR, system_prompt VARCHAR, settings VARCHAR, input VARCHAR,
      created_at TIMESTAMP DEFAULT current_timestamp,
      PRIMARY KEY (key, run)
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS samples (
      sample VARCHAR, position INTEGER, tweet_id VARCHAR,
      PRIMARY KEY (sample, position)
    )
  ")
  con
}

# Everything about a request except the tweet itself
chat_settings <- function(chat, type) {
  provider <- chat$get_provider()
  jsonlite::toJSON(
    list(params = provider@params, api_args = provider@extra_args, type = type@properties$label@values),
    auto_unbox = TRUE
  )
}

cache_key <- function(chat, type, inputs) {
  config <- paste(chat$get_model(), chat$get_system_prompt(), chat_settings(chat, type), sep = "\n")
  vapply(inputs, \(input) digest::digest(paste(config, input, sep = "\n"), algo = "sha256"), character(1), USE.NAMES = FALSE)
}

cache_read <- function(chat, type, tweets, runs) {
  con <- cache_connect()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  keys <- tibble::tibble(key = cache_key(chat, type, tweets$input), id = tweets$id_str)
  dbWriteTable(con, "wanted", keys, temporary = TRUE, overwrite = TRUE)
  dbGetQuery(con, "SELECT w.id, l.run, l.label FROM labels l JOIN wanted w USING (key)") |>
    tibble::as_tibble() |>
    dplyr::filter(run %in% runs)
}

cache_write <- function(chat, type, tweets, run, labels) {
  con <- cache_connect()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  rows <- data.frame(
    key = cache_key(chat, type, tweets$input), run = run, tweet_id = tweets$id_str,
    label = labels, model = chat$get_model(), system_prompt = chat$get_system_prompt(),
    settings = as.character(chat_settings(chat, type)), input = tweets$input
  )
  rows <- rows[!is.na(rows$label), ] # failed requests are retried next time
  dbAppendTable(con, "labels", rows)
}

# Label each tweet in `runs` separate runs, calling the API only for the
# tweet-run pairs that are not in the cache yet.
cached_labels <- function(chat, type, tweets, runs = 1:5) {
  for (run in runs) {
    done <- cache_read(chat, type, tweets, run)
    todo <- tweets[!tweets$id_str %in% done$id, ]
    if (nrow(todo) > 0) {
      result <- parallel_chat_structured(chat, as.list(todo$input), type)
      cache_write(chat, type, todo, run, as.character(result$label))
    }
  }
  cache_read(chat, type, tweets, runs) |>
    dplyr::arrange(run, match(id, tweets$id_str))
}

# Draw a random sample once and store its tweet IDs; later calls return the
# stored sample, even if the data or the sampling code changes.
cached_sample <- function(name, draw) {
  con <- cache_connect()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  ids <- dbGetQuery(con, "SELECT tweet_id FROM samples WHERE sample = ? ORDER BY position", params = list(name))$tweet_id
  if (length(ids) == 0) {
    ids <- draw()
    dbAppendTable(con, "samples", data.frame(sample = name, position = seq_along(ids), tweet_id = ids))
  }
  ids
}

# The model's probability of each label on one call, read from the first token
# of the label. ellmer doesn't expose logprobs, so the request is built by ellmer
# (same model, prompt, settings, and output type as the runs) and sent with
# logprobs added.
# `first_tokens`: for labels whose first token is also the start of another label,
# the token that starts them (e.g. NATIONAL is N + ATIONAL, while NON-POLITICAL
# starts with NON, so N counts for NATIONAL: c(NATIONAL = "N")).
label_probs <- function(chat, type, input, first_tokens = NULL) {
  turns <- c(chat$get_turns(include_system_prompt = TRUE), list(UserTurn(list(ContentText(input)))))
  req <- ellmer:::chat_request(chat$get_provider(), stream = FALSE, turns = turns, type = type) |>
    httr2::req_body_json_modify(logprobs = TRUE, top_logprobs = 10) |>
    httr2::req_retry(max_tries = 10) # OpenRouter rate-limits this model (HTTP 429)
  tokens <- httr2::resp_body_json(httr2::req_perform(req))$choices[[1]]$logprobs$content
  text <- vapply(tokens, \(t) t$token, character(1))
  before <- c("", head(Reduce(paste0, text, accumulate = TRUE), -1))
  i <- which(grepl('"label"\\s*:\\s*"$', before))[1]
  if (is.na(i)) stop("Could not find the label token in the response")
  labels <- type@properties$label@values
  alternatives <- tokens[[i]]$top_logprobs
  probs <- vapply(labels, \(label) {
    # An alternative counts for a label if the label starts with it, and no other label does
    # (or if it is the label's first token in `first_tokens`)
    matches <- Filter(\(a) {
      (sum(startsWith(labels, a$token)) == 1 && startsWith(label, a$token)) ||
        identical(a$token, unname(first_tokens[label]))
    }, alternatives)
    sum(exp(vapply(matches, \(a) a$logprob, numeric(1))))
  }, numeric(1))
  tibble::tibble(label = labels, p = probs)
}

# Label probabilities for each tweet, from one call per tweet and configuration.
cached_probs <- function(chat, type, tweets, first_tokens = NULL) {
  con <- cache_connect()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS probs (
      key VARCHAR, tweet_id VARCHAR, label VARCHAR, p DOUBLE,
      created_at TIMESTAMP DEFAULT current_timestamp,
      PRIMARY KEY (key, label)
    )
  ")
  keys <- cache_key(chat, type, tweets$input)
  for (j in seq_along(keys)) {
    done <- dbGetQuery(con, "SELECT count(*) AS n FROM probs WHERE key = ?", params = list(keys[j]))$n
    if (done == 0) {
      probs <- label_probs(chat, type, tweets$input[j], first_tokens)
      dbAppendTable(con, "probs", data.frame(key = keys[j], tweet_id = tweets$id_str[j], label = probs$label, p = probs$p))
    }
  }
  wanted <- tibble::tibble(key = keys, id = tweets$id_str)
  dbWriteTable(con, "wanted_probs", wanted, temporary = TRUE, overwrite = TRUE)
  dbGetQuery(con, "SELECT w.id, p.label, p.p FROM probs p JOIN wanted_probs w USING (key)") |>
    tibble::as_tibble()
}
