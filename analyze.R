

library(dplyr)
library(readr)

# cols_only() drops the unnamed row-index column written by write.csv().
# Tweet and user IDs exceed double precision (2^53), so read them as strings.
tweets <- read_csv(
  "milli.twts_sample.csv",
  name_repair = "unique_quiet",
  col_types = cols_only(
    created_at                 = col_datetime(format = "%a %b %d %H:%M:%S %z %Y"),
    id_str                     = col_character(),
    full_text                  = col_character(),
    user.id_str                = col_character(),
    user.screen_name           = col_character(),
    user.name                  = col_character(),
    favorite_count             = col_integer(),
    retweet_count              = col_integer(),
    retweeted_status.full_text = col_character(),
    quoted_status.full_text    = col_character(),
    legislator_state           = col_character()
  )
)

tweets_clean <- tweets |>
  # IDs that were saved in scientific notation (e.g. "1.236776654481e+18") lost their digits
  filter(!grepl("e[+]", id_str), !grepl("e[+]", user.id_str)) |>
  filter(!startsWith(full_text, "RT @")) |>
  # Replies
  filter(!startsWith(full_text, "@")) |>
  # Tweets consisting of nothing but one or more URLs
  filter(!grepl("^\\s*(https?://\\S+\\s*)+$", full_text)) |>
  # Last 5 years of data, counted back from the most recent tweet
  filter(created_at > seq(max(created_at), by = "-5 years", length.out = 2)[2]) |>
  arrange(created_at)
