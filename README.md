# Final-Project

TO414 final group project -> NBA draft lottery predictor

Predicts whether a college player goes in the first round of the NBA draft, in R.

Features come from his final college season (plus simple multi-season features). Three sources:

| Source | What we take | URL pattern |
|---|---|---|
| Barttorvik | every D1 player-season, advanced stats | `barttorvik.com/getadvstats.php?year=YYYY&csv=1` |
| Barttorvik | team ratings (competition level) | `barttorvik.com/YYYY_team_results.csv` |
| Basketball-Reference | draft results (pick, player, college) | `basketball-reference.com/draft/NBA_YYYY.html` |

## Quick start for teammates

```r
# 1. Clone the repo, open it in RStudio (File > Open Project > the .Rproj, or set the
#    working directory to the repo folder). Then, once:
install.packages(c("dplyr", "readr", "stringr", "tidyr", "purrr", "httr", "rvest",
                   "ggplot2", "knitr", "rmarkdown"))

# 2. Build the dataset (downloads ~5 min the first time; cached after that)
source("build_dataset.R")
build_dataset(start = 2010, end = 2025, out_dir = "data")

# 3. Run the baseline: open logreg_baseline.Rmd and click Knit, or
rmarkdown::render("logreg_baseline.Rmd")
```

`data/players.csv` is committed to the repo, so you can also skip step 2 and
model straight away. Everyone must use the committed `players.csv` so the five
models are compared on identical data. Rebuild only if `build_dataset.R` or
`manual_matches.csv` changes, then commit the new `players.csv`.

Check the console after step 2: the three sanity-check players must say `OK`,
the "Label verification" block must say `OK`, and the summary line should read
roughly `12,000 rows, ~370 first-rounders` across 14 draft classes
(2020 and 2021 are excluded as COVID years).

## Files

```
build_dataset.R        download + match + build players.csv
logreg_baseline.Rmd    leave-one-draft-out logistic regression (knit to HTML)
tests/                 offline fixture to check the pipeline without network
data/                  created by build_dataset.R (raw/ cache, players.csv, match_report.txt, manual_matches.csv)
results/               created by the Rmd
```

## Setup

```r
install.packages(c("dplyr", "readr", "stringr", "tidyr", "purrr", "httr", "rvest",
                   "ggplot2", "knitr", "rmarkdown"))
```

## 1. Build the dataset

In RStudio, with the project folder as the working directory:

```r
source("build_dataset.R")
build_dataset(start = 2010, end = 2025, out_dir = "data")
```

Takes a few minutes (Basketball-Reference is rate-limited, so there's a 4 s
pause per draft page). Downloads are cached in `data/raw/`; rerun with
`skip_download = TRUE` to rebuild from cache (e.g. after editing `manual_matches.csv`).

Outputs in `data/`:

- **`players.csv`** — one row per player, from his **last** college season.
  Target columns: `first_round` (0/1), `drafted`, `pick_number`.
- **`match_report.txt`** — match rate, picks that couldn't be matched, and a
  cross-check of B-Ref pick numbers against Barttorvik's own `pick` column
  (should be ~0 disagreements; if not, a match or the column mapping is wrong).
- **`manual_matches.csv`** — one row per unmatched/ambiguous pick. Fill in
  `bt_pid` (the Barttorvik player id; the report prints suggestions), then rerun
  with `skip_download = TRUE`. Keep this file in the shared repo: it's the team's fix list.

### How the multi-season problem is handled

Each player is one row, built from his final season. Earlier seasons are folded in:

- `seasons_played`, `has_prior_season`
- `prev_bpm`, `prev_usg`, `prev_mp`, `prev_TS_per` and `delta_*` (final minus previous)
- `career_FT_pct`, `career_TP_pct` with attempt counts (`career_FTA`, `career_TPA`)

Freshmen have NA in `prev_*`/`delta_*`; that's expected and `has_prior_season` flags it.

### Negative pool

Players in their final season with ≥ 400 minutes (`min_minutes`), excluding the
most recent season in the data (many of those players are still in school).
Drafted players are always included.

### Sanity checks printed by the script

- Known players (Zion Williamson 2019, Anthony Edwards 2020, Paolo Banchero 2022)
  must show the right team, class, and `pick` — confirms the Barttorvik column mapping.
- `draft_class_mismatch` should be ~0: a drafted player whose last Barttorvik
  season isn't his draft year (usually a sit-out or a bad match).

## 2. Baseline logistic regression

Open `logreg_baseline.Rmd` in RStudio and **Knit**, or:

```r
rmarkdown::render("logreg_baseline.Rmd")
```

- Leave-one-draft-out cross-validation (never random row splits).
- Within-season z-scoring of each stat (era adjustment); set `season_z: false` in params to turn off.
- Missing values → median + a `_missing` flag column.
- Balanced class weights in `glm`.
- Reports PR-AUC, ROC-AUC, log loss, top-n precision per draft, coefficients,
  and the biggest misses in both directions.

Change the feature set by editing the `features` vector in the `load` chunk.

Outputs in `results/`: `logreg_per_year.csv`, `logreg_coefficients.csv`,
`logreg_predictions.csv` (every player with his predicted probability and rank in class).

## 3. For the other four models

Use **exactly the same** `players.csv`, the same leave-one-draft-out loop, and
the same metrics. Copy the loop from the Rmd; swap the model (`class::knn`,
`nnet::nnet`, `randomForest::randomForest`, `xgboost::xgboost`). KNN and the ANN
need the scaling; the tree models don't (and xgboost takes NAs directly).

## Expected first result

Coefficient signs to check: `class_num` negative (younger = more valuable),
`bpm` and `height_in` positive. If they come out wrong, it's almost always a
join problem — look at `match_report.txt` before touching the model.

## Testing without network

```r
source("tests/make_fixture.R"); make_fixture("tests/fixture")
build_dataset(2017, 2020, out_dir = "tests/fixture", skip_download = TRUE)
rmarkdown::render("logreg_baseline.Rmd",
                  params = list(data = "tests/fixture/players.csv", results_dir = "tests/fixture/results"))
```
