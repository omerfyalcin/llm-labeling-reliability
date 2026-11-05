# Labeling Text with LLMs: How to Make LLM Measurement Reliable

Source for a 45-minute lesson for graduate students on using large language models to label and classify text. The full lesson has four components (first API call, reliability, validity, scaling up). These slides cover the second: **reliability**, i.e. whether an LLM gives the same label to the same document every time.

The running example classifies tweets from U.S. state legislators as NATIONAL, STATE, LOCAL, NON-POLITICAL, or UNCLEAR. Each tweet is labeled 5 times, and three configurations are compared on held-out tweets:

1. a simple prompt
2. a specific prompt with codebook-style rules for borderline cases, written after reading the tweets the model labeled inconsistently
3. the specific prompt with top-k = 1

## Files

- `slides.qmd`: the slides (Quarto, reveal.js)
- `llm_cache.R`: caches every LLM label and token probability in DuckDB, so re-rendering never repeats or changes a run
- `analyze.R`: reads and filters the tweet data
- `styles.css`: slide and handout styles
- `state_national/`: a side analysis of whether legislators from smaller states tweet about national matters more (see its README)

## Data

The tweet data is **not included**: platform terms do not allow redistributing tweet text. The LLM caches (`*.duckdb`) and tweet-level label files are left out for the same reason, since they contain tweet text. As a result, `slides.qmd` will not render from this repository alone; the code is shared so the workflow can be read and adapted.

Included: state-level results (`state_national/results/by_state.csv`, `regressions.txt`, the plot) and the Census Bureau population estimates used in the side analysis.

## Adapting it

Requires Quarto and R with `dplyr`, `tidyr`, `purrr`, `ggplot2`, `readr`, `scales`, `knitr`, `tibble`, `ellmer`, `DBI`, `duckdb`, `httr2`, `jsonlite`, `lmtest`, and `sandwich`, plus an [OpenRouter](https://openrouter.ai) API key in a `.env` file:

```
OPENROUTER_API_KEY=...
```

`llm_cache.R` works with any `ellmer` chat and any data frame with `id_str` and `input` columns. The slides themselves refer to specific tweets from the original sample (the examples and the themes), so those parts need to be replaced for new data.

## Author

Omer Yalcin
