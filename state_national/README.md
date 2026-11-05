# Do legislators from smaller states talk about national matters more?

Side analysis for the second research question in `slides.qmd`. Nothing outside this folder is changed.

- `analysis.R`: run from the project root with `Rscript state_national/analysis.R`
- Sample: 200 tweets per state from `tweets_clean` (`analyze.R`, the filtered 5-year data), seed 2026, stored in `llm_cache.duckdb` here
- Labels: the final configuration from the slides (specific prompt + top-k = 1, `qwen/qwen3-235b-a22b-2507` on DeepInfra via OpenRouter), one run per tweet, cached in `llm_cache.duckdb` here
- Population: Census Bureau July 1, 2020 estimates (`download/nst-est2020-alldata.csv`)
- `results/`: `labels.csv` (tweet-level), `by_state.csv`, `regressions.txt`, `national_by_population.png`
