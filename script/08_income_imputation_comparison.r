###############################################################################
# Project Name:      Predicting Income
# Script Name:       08_Income_Imputation_Comparison.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Compare Section 3's ten income-prediction specifications
#                    under two training regimes - complete cases only (as in
#                    07_income_prediction_validation.r) vs. a training set
#                    where missing y_total_m is imputed via predictive mean
#                    matching (PMM) - to gauge how much the missing-income
#                    selection problem documented in 02_Data_Cleaning.r's
#                    balance table actually affects prediction performance.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data and build model variables
# 3. The ten specifications, as a function of a training data frame
# 4. Baseline: complete-case training (same population as 07)
# 5. PMM imputation of missing log(y_total_m) among paid workers
# 6. Refit the ten specifications on each of the m=5 imputed training sets
# 7. Comparison table: complete-case vs. PMM-imputed, export to LaTeX
# 8. Diagnostic: observed vs. imputed log-income density

################################################################################

# Input:  Clean analysis dataframe from 02_Data_Cleaning.r
# Output: Comparison table (output/tables/section3_imputation_comparison.tex)
#         and diagnostic figure
#         (output/figures/section3_imputation_density.png)

################################################################################


## 1. Load libraries

library(pacman)
p_load(
    tidyverse,
    broom,
    kableExtra,
    mice)


## 2. Load data and build model variables

# Same derived variables as 07_income_prediction_validation.r. relab 6-7
# (unpaid family / other-household workers) are dropped up front: in this
# training split every single relab-6/7 row has missing/non-positive income
# (165 of 165, verified separately) - their "missingness" is not a data
# problem to fix, it is the correct value for someone who by definition
# receives no monetary wage, so they are excluded from both training regimes
# compared below rather than treated as imputable.
clean_data <- readRDS("data/geih_clean.rds") |>
    filter(!(relab %in% c(6, 7))) |>
    mutate(
        age2    = age^2,
        female  = as.numeric(sex == 0),
        subsidio_familiar = case_when(
            p6585s3 == 1        ~ "Sí",
            p6585s3 %in% c(2, 9) ~ "No",
            TRUE                 ~ "No aplica"
        ) |> factor(levels = c("No", "Sí", "No aplica")),
        recibe_transferencias = as.numeric(p7505 == 1),
        # log_inc is NA whenever income is missing OR non-positive (the same
        # criterion every other script in this project uses before taking a
        # log) - exactly the set PMM imputes below.
        log_inc = if_else(!is.na(y_total_m) & y_total_m > 0, log(y_total_m), NA_real_)
    )

train_all       <- clean_data |> filter(subset %in% 1:7)
validation_data <- clean_data |> filter(subset %in% 8:10, !is.na(log_inc))
train_complete  <- train_all  |> filter(!is.na(log_inc))

cat(sprintf(
    "Train rows: %d complete-case / %d total (%d missing income to impute)\n",
    nrow(train_complete), nrow(train_all), sum(is.na(train_all$log_inc))
))


## 3. The ten specifications, as a function of a training data frame

# Identical formulas to 06_income_prediction_train.r (model1, model2,
# gap_uncond, gap_hk, gap_hours) and 07_income_prediction_validation.r
# (model3-model7), wrapped in a function so they can be re-fit on
# train_complete and on each imputed training set without duplicating code
# five more times.
fit_all_models <- function(data) {
    list(
        model1 = lm(log_inc ~ age + age2,
                    data = data, weights = fex_c),
        model2 = lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                    data = data, weights = fex_c),
        gap_uncond = lm(log_inc ~ female,
                         data = data, weights = fex_c),
        gap_hk = lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                    data = data, weights = fex_c),
        gap_hours = lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                          totalHoursWorked,
                        data = data, weights = fex_c),
        model3 = lm(log_inc ~ age + age2 + college + formal + factor(sizeFirm) +
                      formal:college,
                    data = data, weights = fex_c),
        model4 = lm(log_inc ~ age + age2 + college + factor(sizeFirm) +
                      subsidio_familiar + subsidio_familiar:college,
                    data = data, weights = fex_c),
        model5 = lm(log_inc ~ age + age2 + college + cuentaPropia +
                      cuentaPropia:college,
                    data = data, weights = fex_c),
        model6 = lm(log_inc ~ age + age2 + female + totalHoursWorked +
                      I(totalHoursWorked^2) + recibe_transferencias +
                      recibe_transferencias:totalHoursWorked,
                    data = data, weights = fex_c),
        model7 = lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                      female:factor(maxEducLevel),
                    data = data, weights = fex_c)
    )
}

model_labels <- c(
    model1     = "Age: unconditional (04)",
    model2     = "Age: conditional (04)",
    gap_uncond = "Gender: unconditional (05)",
    gap_hk     = "Gender: HK controls (05)",
    gap_hours  = "Gender: HK + hours (05)",
    model3     = "Formal x firm size x college",
    model4     = "Subsidio familiar x college",
    model5     = "Cuenta propia x college",
    model6     = "Horas^2 x transferencias",
    model7     = "Género x nivel educativo"
)

# Validation RMSE, AIC/BIC and leave-one-out CV (leverage shortcut, see
# 07_income_prediction_validation.r) for a fitted set of ten models -
# identical logic to 07, wrapped in a function since it is now applied six
# times (once per training regime: complete-case, and each of 5 imputations).
evaluate_models <- function(models) {
    validation_rmse <- map_dbl(models, function(model) {
        pred <- predict(model, newdata = validation_data)
        sqrt(mean((validation_data$log_inc - pred)^2, na.rm = TRUE))
    })

    loocv_rmse <- map_dbl(models, function(model) {
        h <- hatvalues(model)
        e <- residuals(model)
        # A factor level with a single training observation (model2's
        # factor(relab) == 8, n = 1) gives that row h_ii = 1 exactly:
        # removing it would eliminate its whole category, so its
        # leave-one-out residual is a 0/0 that is mathematically undefined,
        # not just numerically unstable - excluded rather than left to
        # floating-point rounding (which can turn 0/0 into Inf or an
        # arbitrary finite value depending on rounding direction).
        valid <- (1 - h) > 1e-8
        sqrt(mean((e[valid] / (1 - h[valid]))^2))
    })

    fit_stats <- models |>
        map(glance) |>
        list_rbind(names_to = "model") |>
        select(model, AIC, BIC)

    tibble(
        model            = names(models),
        `Validation RMSE` = validation_rmse[names(models)],
        `LOOCV RMSE`      = loocv_rmse[names(models)]
    ) |>
        left_join(fit_stats, by = "model")
}


## 4. Baseline: complete-case training (same population as 07)

baseline_results <- evaluate_models(fit_all_models(train_complete))


## 5. PMM imputation of missing log(y_total_m) among paid workers

# Predictive mean matching: fit a linear model of log_inc on the predictors
# below using only rows with observed income, rank those rows by how close
# their predicted value is to each missing row's predicted value, and impute
# every missing row with the actual observed log_inc of one of its 5 nearest
# matches (mice's default donor pool size) - never a model-based point
# prediction, always a real value from someone similar. This mirrors how
# statistical agencies (e.g. the US Census Bureau's CPS) impute missing
# income via hot-deck/PMM. m = 5 independent random donor draws guard the
# comparison below against depending on a single draw's luck; maxit = 1
# because log_inc is the only incomplete variable here, so there is nothing
# for the chained-equations algorithm to iterate against. The imputation
# model is unweighted (fex_c only enters the substantive models re-fit in
# section 6) - it exists purely to find plausible donors, not to estimate a
# population parameter.
impute_vars <- c("log_inc", "age", "age2", "sex", "maxEducLevel", "relab",
                  "formal", "totalHoursWorked", "sizeFirm", "cuentaPropia")

impute_df <- train_all |>
    select(all_of(impute_vars)) |>
    mutate(across(c(sex, maxEducLevel, relab, formal, sizeFirm, cuentaPropia), factor))

set.seed(123)
imp <- mice(impute_df, m = 5, method = "pmm", maxit = 1, printFlag = FALSE)


## 6. Refit the ten specifications on each of the m=5 imputed training sets

# complete(imp, i) preserves train_all's row order, so its log_inc column
# can be substituted back into train_all directly - every other column
# (fex_c, college, subsidio_familiar, ...) needed by fit_all_models() is
# already there and untouched by the imputation.
imputed_results <- map_dfr(1:5, function(i) {
    train_imputed_i <- train_all |> mutate(log_inc = complete(imp, i)$log_inc)
    evaluate_models(fit_all_models(train_imputed_i))
}) |>
    group_by(model) |>
    summarise(across(c(`Validation RMSE`, `LOOCV RMSE`, AIC, BIC), mean), .groups = "drop")


## 7. Comparison table: complete-case vs. PMM-imputed, export to LaTeX

comparison_table <- baseline_results |>
    rename_with(~ paste0(., " (completos)"), -model) |>
    left_join(
        imputed_results |> rename_with(~ paste0(., " (imputado)"), -model),
        by = "model"
    ) |>
    mutate(Model = model_labels[model]) |>
    select(
        Model,
        `Validation RMSE (completos)`, `Validation RMSE (imputado)`,
        `LOOCV RMSE (completos)`, `LOOCV RMSE (imputado)`,
        `AIC (completos)`, `AIC (imputado)`,
        `BIC (completos)`, `BIC (imputado)`
    ) |>
    arrange(`Validation RMSE (completos)`)

view(comparison_table)

force_float_h <- function(x) {
    sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

n_imputed <- sum(is.na(train_all$log_inc))
comparison_table_tex <- comparison_table |>
    kbl(
        format = "latex", booktabs = TRUE, digits = 4,
        caption = sprintf(
            "Section 3: complete-case (n = %d) vs. PMM-imputed (n = %d, m = 5) training, by validation RMSE, leave-one-out CV RMSE, AIC and BIC",
            nrow(train_complete), nrow(train_all)
        ),
        label = "section3_imputation_comparison"
    ) |>
    kable_styling(latex_options = c("hold_position", "scale_down")) |>
    as.character() |>
    force_float_h()

writeLines(comparison_table_tex, "output/tables/section3_imputation_comparison.tex")


## 8. Diagnostic: observed vs. imputed log-income density

# Pools the imputed values from all 5 draws (each missing row contributes 5
# points, one per imputation) against the single observed distribution, to
# check the PMM draws land in a plausible range rather than piling up at
# implausible values.
imputed_values <- map_dfr(1:5, function(i) {
    completed <- complete(imp, i)
    tibble(log_inc = completed$log_inc[is.na(train_all$log_inc)])
})

density_data <- bind_rows(
    tibble(log_inc = train_all$log_inc[!is.na(train_all$log_inc)], source = "Observado"),
    imputed_values |> mutate(source = "Imputado (PMM)")
)

density_plot <- ggplot(density_data, aes(x = log_inc, fill = source)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Observado" = "#4daad5", "Imputado (PMM)" = "#6c0a8a")) +
    labs(
        title = "Distribución de log-ingreso: observado vs. imputado (PMM)",
        subtitle = paste0(n_imputed, " observaciones imputadas, 5 sorteos de donante cada una"),
        x = "log(y_total_m)",
        y = "Densidad",
        fill = NULL
    ) +
    theme_minimal()

ggsave("output/figures/section3_imputation_density.png", density_plot, width = 8, height = 5)
density_plot
