# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

Replication package for Problem Set 1 ("Predicting Income") of *Big Data and Machine Learning para Economía Aplicada* (MECA 4107, Universidad de los Andes). The goal is to build and evaluate models of individual labor income for Bogotá using the 2018 GEIH household survey, structured around three required analyses (each becomes its own slide deck and, eventually, its own analysis code):

1. **Age–income profile** — unconditional `log(w) = β1 + β2*Age + β3*Age² + u`, then a conditional version adding `totalHoursWorked` and `relab`. Requires a bootstrap CI for the implied peak age.
2. **Gender income gap** — unconditional `log(w) = β1 + β2*Female + u`, then conditional specifications with a self-chosen (and defensible) control set, recovering the gender coefficient via Frisch–Waugh–Lovell with both analytical and bootstrap SEs.
3. **Income prediction** — validation-set approach (train on `subset` chunks 1–7, validate on 8–10), comparing the Section 1/2 models against ≥5 additional specifications by validation RMSE, then LOOCV vs. validation performance and a variable-importance analysis for the best model.

As of this writing, only the data-scraping and data-cleaning pipeline is implemented (`script/01_Web_Scrapping.r`, `script/02_Data_Cleaning.r`); the three sections above are the work to be added on top of `data/geih_clean.rds`.

## Pipeline / architecture

Scripts in `script/` are numbered and meant to run in order from the repo root (each reads/writes paths like `"data/..."` relative to the working directory):

- **`01_Web_Scrapping.r`** — scrapes 10 HTML table chunks from `https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_{1..10}.html`, tags each row with a `subset` column (1–10) identifying its source chunk, and saves the raw combined data to `data/geih_scrap.rds`. **`subset` is the train/validation split key for Section 3** (chunks 1–7 = train, 8–10 = validation, per the assignment).
- **`02_Data_Cleaning.r`** — reads `geih_scrap.rds` and produces `data/geih_clean.rds`:
  - Filters to the analysis sample: `age >= 18 & ocu == 1` (employed adults, per the assignment's §3.1 sample definition). Note: `dsi == 0` looks similar but is **not** equivalent — it let through ~6k economically inactive respondents in an earlier iteration of this script.
  - Drops variables that are unneeded or 100% missing after filtering.
  - Builds a balance table (`balance_table`, exported to `output/tables/balance_table.tex` via `kableExtra::kbl(..., format = "latex")`) comparing rows with vs. without `y_total_m`. **Documented finding:** missingness in `y_total_m` is not random — it's concentrated among `relab` 6/7 (unpaid family / other-household workers, ~100% missing) and, to a lesser extent, self-employed/informal workers. This is relevant context for how Sections 1–3 justify their own missing-income handling.
  - Exports `geih_clean.rds` with missing values left as-is (not imputed or dropped) — each downstream analysis should make and justify its own call on `y_total_m` missingness/zeros, per the assignment's §3.2.
- **`03_Data_Description.r`** — currently a stub (loads `tidyverse` only); intended to hold descriptive statistics/exploratory figures for the analysis sample.
- **`00_Master_File.r`** — currently just a comment noting the intent to skip re-scraping if `data/geih_scrap.rds` already exists; not yet a working orchestrator.

No automated test suite; scripts are run directly and their console/table output is inspected manually.

## Commands

- Run a script (from the repo root, i.e. this `Problem Set 1: Predicting Income/` directory): `Rscript script/02_Data_Cleaning.r`
- Dependencies are managed inline via `pacman::p_load(...)` at the top of each script, which installs any missing CRAN package automatically — no separate install step.

## Key variables

- Outcome: `y_total_m` — total monthly labor income (salaried + self-employment).
- Sample filter: `age >= 18 & ocu == 1`.
- `relab` — employment relationship (1 private employee, 2 government employee, 3 domestic worker, 4 self-employed, 5 employer, 6 unpaid family worker, 7 unpaid worker in another household's business, 8 day laborer, 9 other).
- `maxEducLevel` — 1 none, 2 preschool, 3 primary incomplete, 4 primary complete, 5 secondary incomplete, 6 secondary complete, 7 tertiary.
- `subset` — 1–10, the scraped-page chunk a row came from; doubles as the Section 3 train (1–7) / validation (8–10) split.

## Conventions

- Each script opens with a header block (Project/Script/Authors/Purpose) and a `# Layout:` comment listing its numbered sections; within a numbered section (`## 4. ...`), steps are sub-lettered (`# a.`, `# b.`, ...).
- Tables destined for the writeup are exported with `kableExtra::kbl(..., format = "latex", booktabs = TRUE)` to `output/tables/*.tex`; figures belong in `output/figures/`.
- The assignment recommends following the tidyverse style guide.
- `presentation/` holds slide-deck sources, but `.gitignore` excludes `*.pdf`/`*.html` repo-wide, so rendered decks aren't committed here — they're submitted separately on Bloque Neón as `age_equipo_XX.pdf`, `gap_equipo_XX.pdf`, `pred_equipo_XX.pdf`.
