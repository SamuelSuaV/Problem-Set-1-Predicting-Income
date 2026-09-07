###############################################################################
# Project Name:      Predicting Income
# Script Name:       00_Master_File.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Runs the whole pipeline (scraping, cleaning, description
#                    and Sections 1-3) with a single call.
###############################################################################

# Layout:
# 1. Configuration
# 2. Working directory and output folders
# 3. Pipeline definition
# 4. Step runner
# 5. Execute

################################################################################

# Usage (run from the repo root, the "Problem Set 1 - Predicting Income/" dir):
#
#   Rscript script/00_Master_File.r            # run every step
#   Rscript script/00_Master_File.r 04 05      # run only steps 04 and 05
#   FORCE_SCRAPE=1 Rscript script/00_Master_File.r   # re-run the web scraping
#
# Step 01 (scraping) is skipped if data/geih_scrap.rds already exists so we
# don't hit the GEIH pages every time. Use FORCE_SCRAPE=1 or delete the file
# to scrape again.

################################################################################


## 1. Configuration

# Only re-scrape if asked to, or if the raw file isn't there.
force_scrape <- tolower(Sys.getenv("FORCE_SCRAPE", "false")) %in%
  c("1", "true", "yes", "y")

# Optional args: step numbers ("01", "4", ...) to run just some of the steps.
# If nothing is passed we run everything.
requested_steps <- commandArgs(trailingOnly = TRUE)

# Each script loads its own packages with pacman, but pacman itself has to be
# installed first.
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}


## 2. Working directory and output folders

# Every script uses paths relative to the repo root ("data/...", "output/...").
if (!file.exists("script/01_Web_Scrapping.r")) {
  stop(
    "Run this from the repo root:  Rscript script/00_Master_File.r",
    call. = FALSE
  )
}

for (d in c("data", "output/tables", "output/figures", "output/models")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


## 3. Pipeline definition

# id      -> numeric prefix of the script (used to pick a subset of steps)
# script  -> path from the repo root
# label   -> description printed to the console
# skip    -> optional function; if it returns TRUE the step is skipped
pipeline <- list(
  list(
    id = "01", script = "script/01_Web_Scrapping.r",
    label = "Web scraping (GEIH 2018 sample -> data/geih_scrap.rds)",
    skip = function() {
      if (force_scrape) return(FALSE)
      file.exists("data/geih_scrap.rds")
    }
  ),
  list(
    id = "02", script = "script/02_Data_Cleaning.r",
    label = "Data cleaning (-> data/geih_clean.rds, balance table)"
  ),
  list(
    id = "03", script = "script/03_Data_Description.r",
    label = "Data description (descriptive tables and figures)"
  ),
  list(
    id = "04", script = "script/04_age_labor_income.r",
    label = "Section 1: age-income profile (bootstrap peak age)"
  ),
  list(
    id = "05", script = "script/05_gender_gap.r",
    label = "Section 2: gender income gap (FWL, bootstrap SE)"
  ),
  list(
    id = "06", script = "script/06_income_prediction_train.r",
    label = "Section 3: train Section 1/2 models on subset 1-7"
  ),
  list(
    id = "07", script = "script/07_income_prediction_validation.r",
    label = "Section 3: validation RMSE, LOOCV, variable importance"
  ),
  list(
    id = "08", script = "script/08_income_imputation_comparison.r",
    label = "Section 3: complete-case vs. PMM-imputed comparison"
  )
)


## 4. Step runner

# Turn a step token ("4", "04", "step04") into its two-digit id.
norm_id <- function(x) sprintf("%02d", as.integer(gsub("\\D", "", x)))

selected_ids <- if (length(requested_steps) > 0) {
  vapply(requested_steps, norm_id, character(1))
} else {
  vapply(pipeline, function(s) s$id, character(1))
}

# Each script reads the .rds it needs from disk, so we source it in its own
# environment to keep names from leaking between steps.
run_step <- function(step) {
  bar <- strrep("=", 74)
  message("\n", bar)
  message(sprintf(
    "[%s]  Step %s  %s", format(Sys.time(), "%H:%M:%S"), step$id, step$label
  ))
  message(bar)

  t0 <- Sys.time()
  ok <- tryCatch({
    source(step$script, local = new.env(parent = globalenv()), echo = FALSE)
    TRUE
  }, error = function(e) {
    message("\n*** Step ", step$id, " FAILED: ", conditionMessage(e))
    FALSE
  })
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  if (!ok) {
    stop(
      sprintf("Pipeline stopped at step %s after %s s.", step$id, dt),
      call. = FALSE
    )
  }
  message(sprintf("--- step %s done (%s s)", step$id, dt))
}


## 5. Execute

t_start <- Sys.time()
ran <- character(0)
skipped <- character(0)

for (step in pipeline) {
  if (!step$id %in% selected_ids) next

  if (!is.null(step$skip) && isTRUE(step$skip())) {
    message(sprintf(
      "\n--- step %s skipped (%s)", step$id, step$label
    ))
    skipped <- c(skipped, step$id)
    next
  }

  run_step(step)
  ran <- c(ran, step$id)
}

total <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 2)
message("\n", strrep("=", 74))
message(sprintf("Pipeline finished in %s min.", total))
message("  ran:     ", if (length(ran)) paste(ran, collapse = ", ") else "(none)")
message("  skipped: ",
        if (length(skipped)) paste(skipped, collapse = ", ") else "(none)")
message(strrep("=", 74))

################################End of script###################################
