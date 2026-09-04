# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

Replication package for Problem Set 1 ("Predicting Income") of *Big Data and Machine Learning para Economía Aplicada* (MECA 4107, Universidad de los Andes). The goal is to build and evaluate models of individual labor income for Bogotá using the 2018 GEIH household survey, structured around three required analyses (each becomes its own slide deck and, eventually, its own analysis code):

1. **Age–income profile** — unconditional `log(w) = β1 + β2*Age + β3*Age² + u`, then a conditional version adding `totalHoursWorked` and `relab`. Requires a bootstrap CI for the implied peak age. Stubbed out in `script/04_age_labor_income.r` (currently empty).
2. **Gender income gap** — unconditional `log(w) = β1 + β2*Female + u`, then conditional specifications with a self-chosen (and defensible) control set, recovering the gender coefficient via Frisch–Waugh–Lovell with both analytical and bootstrap SEs.
3. **Income prediction** — validation-set approach (train on `subset` chunks 1–7, validate on 8–10), comparing the Section 1/2 models against ≥5 additional specifications by validation RMSE, then LOOCV vs. validation performance and a variable-importance analysis for the best model.

The data-scraping, data-cleaning and data-description pipeline (`script/01_Web_Scrapping.r` through `03_Data_Description.r`, plus a `LaTex/` writeup pulling in their output) is implemented; Sections 1–3 above are the modeling work to be added on top of `data/geih_clean.rds`.

## Pipeline / architecture

Scripts in `script/` are numbered and meant to run in order from the repo root (each reads/writes paths like `"data/..."` relative to the working directory):

- **`01_Web_Scrapping.r`** — scrapes 10 HTML table chunks from `https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_{1..10}.html`, tags each row with a `subset` column (1–10) identifying its source chunk, and saves the raw combined data to `data/geih_scrap.rds`. **`subset` is the train/validation split key for Section 3** (chunks 1–7 = train, 8–10 = validation, per the assignment).
- **`02_Data_Cleaning.r`** — reads `geih_scrap.rds` and produces `data/geih_clean.rds`:
  - Filters to the analysis sample: `age >= 18 & ocu == 1` (employed adults, per the assignment's §3.1 sample definition). Note: `dsi == 0` looks similar but is **not** equivalent — it let through ~6k economically inactive respondents in an earlier iteration of this script.
  - Drops variables that are unneeded or 100% missing after filtering.
  - Builds a balance table (`balance_table`, exported to `output/tables/balance_table.tex`) comparing rows with vs. without `y_total_m`. **Documented finding:** missingness in `y_total_m` is not random — it's concentrated among `relab` 6/7 (unpaid family / other-household workers, ~100% missing) and, to a lesser extent, self-employed/informal workers (see `relab_pct`/`formal_pct`). This is relevant context for how Sections 1–3 justify their own missing-income handling.
  - Exports `geih_clean.rds` with missing values left as-is (not imputed or dropped) — each downstream analysis should make and justify its own call on `y_total_m` missingness/zeros, per the assignment's §3.2.
- **`03_Data_Description.r`** — reads `geih_clean.rds` and produces every table in `output/tables/*.tex` and figure in `output/figures/*.png` used by the writeup:
  - All statistics are survey-weighted by `fex_c` (GEIH's person-level expansion factor) via the `srvyr` package: a design object `geih_svy <- geih_clean %>% as_survey_design(weights = fex_c)` feeds two generic helpers, `describe_continuous()` (mean/sd/quantiles via `survey_mean()`/`survey_sd()`/`survey_quantile()`, plus hand-rolled weighted skewness/kurtosis since `srvyr` has no weighted-moment estimator; N/min/max stay unweighted) and `describe_categorical()` (weighted population totals via `survey_total()`). Histograms and scatter plots use the same weight through ggplot2's `weight` aesthetic (weighted density, weighted group means, WLS trend lines).
  - **Gotcha:** inside `describe_continuous()`, the `.fns` list passed to `across()` must be written *literally inline* in the `summarise()` call, not pre-assigned to a variable (e.g. `stat_funs <- list(...)`) and referenced by name. `srvyr`'s `summarise()` re-quotes the call before evaluating it against the design's data, and a custom function referencing a bare column symbol (like the `fex_c` weight, inside the weighted-skewness/kurtosis helpers) loses access to that column when the list comes from a variable — `survey_mean()`/`survey_sd()`/`survey_quantile()`/`unweighted()` are unaffected since `srvyr` special-cases those.
  - Tables are exported with `kableExtra::kbl(..., format = "latex", booktabs = TRUE)`, then post-processed to swap kableExtra's `[!h]` placement hint for a hard `[H]` (`sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", ...)`, matching the same fix in `02_Data_Cleaning.r`) — otherwise these small tables float past their own section in the compiled writeup, landing among later figures/sections instead. Requires `\usepackage{float}` in the including `.tex`.
- **`LaTex/main.tex`** — the writeup: pulls every table/figure from `output/` via `\input{../output/tables/*.tex}` / `\includegraphics{../output/figures/*.png}`, so it stays in sync automatically when `02`/`03` are rerun — no content is duplicated into the `.tex` file itself. Compile with `pdflatex` from inside `LaTex/` (twice, for the TOC/cross-references).
- **`00_Master_File.r`** — currently just a comment noting the intent to skip re-scraping if `data/geih_scrap.rds` already exists; not yet a working orchestrator.

No automated test suite; scripts are run directly and their console/table output is inspected manually.

## Commands

- Run a script (from the repo root, i.e. this `Problem Set 1: Predicting Income/` directory): `Rscript script/02_Data_Cleaning.r`
- Dependencies are managed inline via `pacman::p_load(...)` at the top of each script, which installs any missing CRAN package automatically — no separate install step.
- Compile the writeup: `cd LaTex && pdflatex -interaction=nonstopmode main.tex && pdflatex -interaction=nonstopmode main.tex` (two passes; run after `02`/`03` so the `.tex`/figure inputs are current). Clean up `main.aux`/`.log`/`.out`/`.toc` afterward — the `.gitignore` excludes `*.pdf` but not these, so they're easy to accidentally commit.

## Key variables

- Outcome: `y_total_m` — total monthly labor income (salaried + self-employment).
- Sample filter: `age >= 18 & ocu == 1`.
- `fex_c` — person-level expansion factor (GEIH is a complex survey, not a simple random sample); used as the weight for every descriptive statistic in `03_Data_Description.r`.
- `relab` — employment relationship (1 private employee, 2 government employee, 3 domestic worker, 4 self-employed, 5 employer, 6 unpaid family worker, 7 unpaid worker in another household's business, 8 day laborer, 9 other).
- `maxEducLevel` — 1 none, 2 preschool, 3 primary incomplete, 4 primary complete, 5 secondary incomplete, 6 secondary complete, 7 tertiary.
- `sex` — 0/1, coding inferred (not from the codebook) from gender-skewed occupations (`oficio`): housemaids (54) are almost all `sex == 0`, drivers (98) almost all `sex == 1`.
- `subset` — 1–10, the scraped-page chunk a row came from; doubles as the Section 3 train (1–7) / validation (8–10) split.

## Conventions

- Each script opens with a header block (Project/Script/Authors/Purpose) and a `# Layout:` comment listing its numbered sections; within a numbered section (`## 4. ...`), steps are sub-lettered (`# a.`, `# b.`, ...).
- Tables destined for the writeup are exported with `kableExtra::kbl(..., format = "latex", booktabs = TRUE)` to `output/tables/*.tex` (forced to `[H]` placement, see above); figures belong in `output/figures/`.
- The assignment recommends following the tidyverse style guide.
- `presentation/` holds slide-deck sources, but `.gitignore` excludes `*.pdf`/`*.html` repo-wide, so rendered decks aren't committed here — they're submitted separately on Bloque Neón as `age_equipo_XX.pdf`, `gap_equipo_XX.pdf`, `pred_equipo_XX.pdf`.
