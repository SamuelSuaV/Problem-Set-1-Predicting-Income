###############################################################################
# Project Name:      Predicting Income
# Script Name:       06_Income_Prediction_Train.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Re-estimate the five models from 04_Age_Labor_Income.r and
#                    05_Gender_Gap.r on the Section 3 training split (subset
#                    1-7) only, and compare their coefficients against the
#                    full-sample versions. The fitted training models are
#                    saved for out-of-sample prediction on subset 8-10 in a
#                    later script.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data and build model variables
# 3. Train / full-sample split
# 4. Re-estimate the five models on the training split and on the full sample
# 5. Save fitted training models for out-of-sample prediction
# 6. Full-sample vs. train-only coefficient comparison table

################################################################################

# Input:  Clean analysis dataframe from 02_Data_Cleaning.r
# Output: Fitted training models (output/models/section3_train_models.rds) and
#         a full-sample vs. train coefficient comparison table
#         (output/tables/section3_train_vs_full.tex)

################################################################################


## 1. Load libraries

library(pacman)
p_load(
    tidyverse,
    broom,
    kableExtra)


## 2. Load data and build model variables

# Same analysis sample and derived variables as 04_Age_Labor_Income.r and
# 05_Gender_Gap.r: employed adults (already enforced in 02_Data_Cleaning.r)
# with strictly positive labour income, since log(y_total_m) is undefined at 0
# and NA for missing income.
clean_data <- readRDS("data/geih_clean.rds") |>
    filter(y_total_m > 0) |>
    mutate(
        age2    = age^2,
        log_inc = log(y_total_m),
        female  = as.numeric(sex == 0)
    )


## 3. Train / full-sample split

# Section 3 of the problem set validates on scraped chunks the model never
# saw: subset 1-7 is the training split, subset 8-10 is held out for
# out-of-sample validation in a later script. Every model below is fit twice
# - on train_data and on the full clean_data - so we can see how much each
# coefficient moves once the validation chunks (~30% of the sample) are
# withheld from estimation.
train_data <- clean_data |> filter(subset %in% 1:7)


## 4. Re-estimate the five models on the training split and on the full sample

# Same five specifications as 04_Age_Labor_Income.r (model1, model2) and
# 05_Gender_Gap.r (gap_uncond, gap_hk, gap_hours), weighted by fex_c for the
# same reason given there: the GEIH is a complex survey, not a simple random
# sample.

# a. Age-income profile (04_Age_Labor_Income.r)
model1_train <- lm(log_inc ~ age + age2, data = train_data, weights = fex_c)
model1_full  <- lm(log_inc ~ age + age2, data = clean_data, weights = fex_c)

model2_train <- lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                    data = train_data, weights = fex_c)
model2_full  <- lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                    data = clean_data, weights = fex_c)

# b. Gender gap (05_Gender_Gap.r)
gap_uncond_train <- lm(log_inc ~ female, data = train_data, weights = fex_c)
gap_uncond_full  <- lm(log_inc ~ female, data = clean_data, weights = fex_c)

gap_hk_train <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                    data = train_data, weights = fex_c)
gap_hk_full  <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                    data = clean_data, weights = fex_c)

gap_hours_train <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                         totalHoursWorked,
                       data = train_data, weights = fex_c)
gap_hours_full  <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                         totalHoursWorked,
                       data = clean_data, weights = fex_c)

# c. Quick check that nothing degenerated when 30% of the sample was dropped
# (e.g. a relab or maxEducLevel category with too few training observations).
summary(model1_train)
summary(model2_train)
summary(gap_uncond_train)
summary(gap_hk_train)
summary(gap_hours_train)


## 5. Save fitted training models for out-of-sample prediction

# A named list, not five loose objects, so a later validation script can loop
# over it (predict() each model against the subset 8-10 data) instead of
# hard-coding five separate calls.
train_models <- list(
    model1     = model1_train,
    model2     = model2_train,
    gap_uncond = gap_uncond_train,
    gap_hk     = gap_hk_train,
    gap_hours  = gap_hours_train
)

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
saveRDS(train_models, "output/models/section3_train_models.rds")


## 6. Full-sample vs. train-only coefficient comparison table

# a. One row per coefficient, one pair of columns per sample (train vs. full),
# grouped by model so the two model families don't run together.
compare_model <- function(model_train, model_full) {
    full_join(
        tidy(model_train) |> select(term, estimate, std.error),
        tidy(model_full)  |> select(term, estimate, std.error),
        by = "term",
        suffix = c("_train", "_full")
    )
}

age_models <- bind_rows(
    compare_model(model1_train, model1_full) |> mutate(model = "Unconditional"),
    compare_model(model2_train, model2_full) |> mutate(model = "Conditional")
)

gender_models <- bind_rows(
    compare_model(gap_uncond_train, gap_uncond_full) |> mutate(model = "Unconditional"),
    compare_model(gap_hk_train, gap_hk_full) |> mutate(model = "HK controls"),
    compare_model(gap_hours_train, gap_hours_full) |> mutate(model = "HK + hours")
)

comparison_table <- bind_rows(
    age_models    |> mutate(family = "Age-income profile (04)"),
    gender_models |> mutate(family = "Gender gap (05)")
) |>
    relocate(family, model, term)

view(comparison_table)

# b. Export to LaTeX, same [!h] -> [H] fix as 02_Data_Cleaning.r and
# 04_Age_Labor_Income.r, so the table holds its place in the compiled
# write-up (requires \usepackage{float}).
force_float_h <- function(x) {
    sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

comparison_table_tex <- comparison_table |>
    rename(
        Family = family,
        Model = model,
        Term = term,
        `Train (1-7)` = estimate_train,
        `SE (train)` = std.error_train,
        `Full sample` = estimate_full,
        `SE (full)` = std.error_full
    ) |>
    kbl(
        format = "latex", booktabs = TRUE, digits = 4,
        caption = "Section 3 training-split coefficients vs. full-sample coefficients",
        label = "section3_train_vs_full"
    ) |>
    kable_styling(latex_options = c("hold_position", "scale_down")) |>
    collapse_rows(columns = 1:2, latex_hline = "major") |>
    as.character() |>
    force_float_h()

writeLines(comparison_table_tex, "output/tables/section3_train_vs_full.tex")
