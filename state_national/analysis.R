# Do legislators from smaller states talk about national matters more?
#
# 200 tweets from each of the 50 states, labeled once with the final configuration
# from slides.qmd (specific prompt + top-k = 1, Qwen3 on DeepInfra via OpenRouter),
# regressed on the state's 2020 population.
#
# Run from the project root: Rscript state_national/analysis.R
# Nothing outside state_national/ is written; labels are cached in
# state_national/llm_cache.duckdb, using the helpers from llm_cache.R.

library(dplyr)
library(readr)
library(ggplot2)
library(ellmer)

readRenviron(".env") # sets OPENROUTER_API_KEY
source("llm_cache.R")
cache_path <- "state_national/llm_cache.duckdb" # a separate cache, so llm_cache.duckdb is untouched

out_dir <- "state_national/results"
dir.create(out_dir, showWarnings = FALSE)

n_per_state <- 200

# Data --------------------------------------------------------------------

# tweets_clean from analyze.R (the filtered 5-year data), saved once since the CSV is slow to read
tweets_rds <- "state_national/tweets_clean.rds"
if (!file.exists(tweets_rds)) {
  source("analyze.R")
  saveRDS(tweets_clean, tweets_rds)
}
tweets_clean <- readRDS(tweets_rds)

format_tweet <- function(text, quoted) {
  if_else(
    is.na(quoted),
    paste0("Tweet: ", text),
    paste0("Tweet: ", text, "\n\nQuoted tweet: ", quoted)
  )
}

sample_ids <- cached_sample(paste0("by-state-", n_per_state), \() {
  set.seed(2026)
  slice_sample(tweets_clean, n = n_per_state, by = legislator_state)$id_str
})

sampled <- tweets_clean |>
  filter(id_str %in% sample_ids) |>
  distinct(id_str, .keep_all = TRUE) |> # a few tweets appear twice in the data
  mutate(input = format_tweet(full_text, quoted_status.full_text))
stopifnot(nrow(sampled) == 50 * n_per_state)

# July 1, 2020 Census Bureau estimates (Vintage 2020), downloaded from
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/totals/nst-est2020-alldata.csv
population <- read_csv("state_national/download/nst-est2020-alldata.csv", show_col_types = FALSE) |>
  filter(SUMLEV == "040", NAME != "District of Columbia", NAME != "Puerto Rico") |>
  transmute(legislator_state = gsub(" ", "_", NAME), population = POPESTIMATE2020)
stopifnot(nrow(population) == 50, setequal(population$legislator_state, sampled$legislator_state))

# Labels: the final configuration from slides.qmd ----------------------------

specific_prompt <- "Classify this tweet from a U.S. state legislator as
NATIONAL, STATE, LOCAL, NON-POLITICAL, or UNCLEAR.

Rules for borderline cases:
1. The legislator's own district, constituents, or campaign for their seat is LOCAL,
   even when the tweet mentions their seat in the state legislature.
2. When a tweet mixes levels, choose the level of the main actor or decision, not
   the place: a state agency working in one county is STATE; a presidential
   election result in one precinct is NATIONAL.
3. Holidays, commemorations, greetings, jokes, and scenery are NON-POLITICAL
   unless the tweet makes a policy or political point.
4. If the meaning depends on something you cannot see (a photo, a link, or an
   earlier tweet in a thread), choose UNCLEAR."

type <- type_object(
  label = type_enum(c("NATIONAL", "STATE", "LOCAL", "NON-POLITICAL", "UNCLEAR"))
)

chat_topk <- chat_openrouter(
  system_prompt = specific_prompt,
  model = "qwen/qwen3-235b-a22b-2507",
  params = params(temperature = 1, top_k = 1),
  api_args = list(provider = list(
    order = list("DeepInfra"), allow_fallbacks = FALSE, require_parameters = TRUE
  ))
)

# In chunks, so that one failed request (which also crashes httr2's progress bar)
# loses only its chunk: finished chunks are already in the cache when we retry
chunks <- split(sampled, ceiling(seq_len(nrow(sampled)) / 500))
for (attempt in 1:5) {
  for (chunk in chunks) {
    tryCatch(cached_labels(chat_topk, type, chunk, runs = 1), error = \(e) message(conditionMessage(e)))
  }
}
labels <- cached_labels(chat_topk, type, sampled, runs = 1)
stopifnot(nrow(labels) == nrow(sampled)) # failed requests are retried on the next run

coded <- sampled |>
  inner_join(select(labels, id_str = id, label), by = "id_str") |>
  left_join(population, by = "legislator_state") |>
  mutate(
    national = as.integer(label == "NATIONAL"),
    political = label %in% c("NATIONAL", "STATE", "LOCAL"),
    log10_pop = log10(population)
  )

write_csv(
  select(coded, id_str, created_at, user.screen_name, legislator_state, population, label, full_text),
  file.path(out_dir, "labels.csv")
)

# Regressions -------------------------------------------------------------

# Tweet-level linear probability models; standard errors clustered by state,
# since population only varies between states
clustered <- function(model) lmtest::coeftest(model, vcov = sandwich::vcovCL(model, cluster = ~legislator_state))

models <- list(
  "All tweets: P(NATIONAL) ~ log10(population)" =
    lm(national ~ log10_pop, data = coded),
  "Political tweets only (NATIONAL/STATE/LOCAL): P(NATIONAL) ~ log10(population)" =
    lm(national ~ log10_pop, data = filter(coded, political)),
  "All tweets, logit: NATIONAL ~ log10(population)" =
    glm(national ~ log10_pop, family = binomial, data = coded)
)

by_state <- coded |>
  summarise(
    n = n(),
    n_political = sum(political),
    n_national = sum(national),
    national_share = mean(national),
    national_share_political = n_national / n_political,
    n_legislators = n_distinct(user.id_str),
    .by = c(legislator_state, population)
  ) |>
  arrange(population)
write_csv(by_state, file.path(out_dir, "by_state.csv"))

# Rank correlation at the state level, as a check that doesn't assume linearity
spearman <- cor.test(by_state$national_share, by_state$population, method = "spearman", exact = FALSE)

sink(file.path(out_dir, "regressions.txt"))
cat("Label counts\n")
print(table(coded$label))
for (name in names(models)) {
  cat("\n", name, "\n", sep = "")
  print(clustered(models[[name]]))
}
cat("\nState level: Spearman correlation of NATIONAL share and population\n")
print(spearman)
sink()

# Plot --------------------------------------------------------------------

p <- ggplot(by_state, aes(population, national_share)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#003262", fill = "#B8C0CC") +
  geom_text(aes(label = gsub("_", " ", legislator_state)), size = 3, color = "#333333",
            check_overlap = TRUE, vjust = -0.6) +
  geom_point(color = "#003262", size = 2) +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "State population, 2020 (log scale)",
    y = paste0("Tweets labeled NATIONAL (of ", n_per_state, ")"),
    title = "Do legislators from smaller states tweet about national matters more?"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
ggsave(file.path(out_dir, "national_by_population.png"), p, width = 9, height = 5.5, dpi = 200, bg = "white")

cat(readLines(file.path(out_dir, "regressions.txt")), sep = "\n")
