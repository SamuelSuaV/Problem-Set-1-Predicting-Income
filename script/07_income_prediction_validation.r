###############################################################################
# Project Name:      Predicting Income
# Script Name:       07_Income_Prediction_Validation.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Add five additional income-prediction specifications to
#                    the five models from 04_Age_Labor_Income.r and
#                    05_Gender_Gap.r (trained in 06_income_prediction_train.r),
#                    compare all ten by validation-set RMSE, AIC, BIC and an
#                    exact leave-one-out CV via the leverage shortcut, and
#                    plot the variable importance of the best model.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data and build model variables (train / validation split)
# 3. Five additional specifications (economic justification in comments)
# 4. Combine with the Section 1/2 models from 06_income_prediction_train.r
# 5. Validation-set RMSE
# 6. In-sample fit: AIC and BIC
# 7. Leave-one-out CV via the leverage shortcut
# 8. Model comparison table and export to LaTeX
# 9. Variable importance for the best model (standardized |beta|) and plot

################################################################################

# Input:  Clean analysis dataframe from 02_Data_Cleaning.r and the training
#         models saved by 06_income_prediction_train.r
# Output: Model comparison table (output/tables/section3_model_comparison.tex)
#         and variable-importance figure
#         (output/figures/section3_variable_importance.png)

################################################################################


## 1. Load libraries

library(pacman)
p_load(
    tidyverse,
    broom,
    kableExtra)


## 2. Load data and build model variables (train / validation split)

# Same analysis sample as 04_Age_Labor_Income.r, 05_Gender_Gap.r and
# 06_income_prediction_train.r: employed adults with strictly positive labour
# income. Two extra derived variables feed the additional specifications in
# section 3 below.
clean_data <- readRDS("data/geih_clean.rds") |>
    filter(y_total_m > 0) |>
    mutate(
        age2    = age^2,
        log_inc = log(y_total_m),
        female  = as.numeric(sex == 0),
        # p6585s3: "¿el mes pasado recibió subsidio familiar?" (1 = sí,
        # 2/9 = no/no sabe). Only asked of dependent workers, so ~32% of the
        # sample is NA - kept as its own "No aplica" category (not dropped)
        # so those rows still enter every model, and because the missingness
        # itself proxies for a non-salaried employment relationship.
        subsidio_familiar = case_when(
            p6585s3 == 1        ~ "Sí",
            p6585s3 %in% c(2, 9) ~ "No",
            TRUE                 ~ "No aplica"
        ) |> factor(levels = c("No", "Sí", "No aplica")),
        # p7505: recibió dinero de otros hogares/personas/instituciones en los
        # últimos 12 meses (1 = sí, 2 = no); no missing values.
        recibe_transferencias = as.numeric(p7505 == 1)
    )

# Section 3's train (1-7) / validation (8-10) split, same key used in
# 06_income_prediction_train.r.
train_data      <- clean_data |> filter(subset %in% 1:7)
validation_data <- clean_data |> filter(subset %in% 8:10)


## 3. Five additional specifications (economic justification in comments)

# All weighted by fex_c, for the same reason as every regression in this
# project: the GEIH is a complex survey, not a simple random sample.

# a. Formal-sector / firm-size wage premium, interacted with a college degree.
# Formal jobs bundle legal protections and access to better-capitalised
# employers (formal wage premium); larger firms pay more for the same worker
# (efficiency wages / internal labour markets). The interaction tests whether
# a college credential is screened/rewarded more strongly once a job is
# formal, where employers can verify it and pay structures are more rigid.
model3 <- lm(log_inc ~ age + age2 + college + formal + factor(sizeFirm) +
               formal:college,
             data = train_data, weights = fex_c)

# b. Non-wage benefits bundle. The family subsidy (subsidio familiar / cajas
# de compensación) is a mandated fringe benefit tied to a formal, salaried
# employment relationship - "good jobs" bundle higher pay and this kind of
# benefit rather than trading one for the other. The interaction tests
# whether that benefit's association with pay is larger for college-educated
# workers, who tend to hold higher-tier jobs within the formal sector.
# NOTE: factor(maxEducLevel) is deliberately left out here - in this dataset
# `college` is exactly `maxEducLevel == 6` (not level 7, despite the name),
# so including both is perfectly collinear and lm() aliases a coefficient to
# NA. maxEducLevel's gradient is already covered by gap_hk/gap_hours (05) and
# model7 below, so college alone is the education control here.
model4 <- lm(log_inc ~ age + age2 + college + factor(sizeFirm) +
               subsidio_familiar + subsidio_familiar:college,
             data = train_data, weights = fex_c)

# c. Self-employment penalty, interacted with a college degree. Own-account
# work is, on average, a subsistence-income penalty (liquidity constraints,
# no employer-provided benefits), but college-educated own-account workers
# (independent professionals/consultants) are a different population from
# subsistence self-employment, so the penalty should be smaller - or
# reversed - for them.
model5 <- lm(log_inc ~ age + age2 + college + cuentaPropia +
               cuentaPropia:college,
             data = train_data, weights = fex_c)

# d. Labour-supply / reservation-wage effect of external transfers. Standard
# labour-supply theory: non-labour income (remittances, help from other
# households) raises the reservation wage and can dampen labour-supply
# intensity (an income effect on hours). totalHoursWorked enters with a
# quadratic to allow diminishing (or reversing) returns to extra hours - most
# salaried jobs have a flat/overtime-capped hours-pay relationship - and its
# interaction with recibe_transferencias tests whether the hours-income
# relationship is flatter for workers cushioned by outside transfers.
model6 <- lm(log_inc ~ age + age2 + female + totalHoursWorked +
               I(totalHoursWorked^2) + recibe_transferencias +
               recibe_transferencias:totalHoursWorked,
             data = train_data, weights = fex_c)

# e. Gender gap in returns to education. 05_Gender_Gap.r's gap_hk holds
# education fixed as a single level shift; this specification lets each
# maxEducLevel category carry its own gender gap, testing whether returns to
# schooling diverge by sex (e.g. a "glass ceiling" that widens with
# credentials, or a motherhood penalty concentrated among more educated
# women).
model7 <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
               female:factor(maxEducLevel),
             data = train_data, weights = fex_c)


## 4. Combine with the Section 1/2 models from 06_income_prediction_train.r

section12_models <- readRDS("output/models/section3_train_models.rds")

all_models <- c(
    section12_models,
    list(model3 = model3, model4 = model4, model5 = model5,
         model6 = model6, model7 = model7)
)

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


## 5. Validation-set RMSE

# predict() on subset 8-10, which none of the ten models saw during fitting.
# RMSE is computed on the log(y_total_m) scale every model is estimated on,
# so it is directly comparable across all ten specifications - converting
# back to pesos would need a retransformation correction (e.g. Duan's
# smearing estimator), which is out of scope here. Factor levels were
# checked beforehand: validation_data has no category (relab, sizeFirm,
# maxEducLevel, ...) absent from train_data, so predict() cannot fail on an
# unseen level here.
validation_rmse <- map_dbl(all_models, function(model) {
    pred <- predict(model, newdata = validation_data)
    sqrt(mean((validation_data$log_inc - pred)^2, na.rm = TRUE))
})


## 6. In-sample fit: AIC and BIC

# From the same training fit used for validation prediction, so AIC, BIC and
# validation RMSE all describe the same fitted object. Because every model is
# fit by weighted least squares (weights = fex_c), AIC/BIC use the
# weighted-Gaussian log-likelihood implied by those weights - internally
# consistent across all ten models since they all carry the same weights.
fit_stats <- all_models |>
    map(glance) |>
    list_rbind(names_to = "model") |>
    select(model, AIC, BIC)


## 7. Leave-one-out CV via the leverage shortcut

# Exact LOOCV would refit every model once per left-out training observation
# (~10,000 refits each). For least squares - ordinary or weighted, as used
# here - that is unnecessary: leaving out observation i and refitting gives
# exactly the same residual as
#     e_(i) = e_i / (1 - h_ii)
# where e_i is that observation's residual from the single fit on the full
# training data, and h_ii is its leverage - the i-th diagonal of the hat
# matrix, i.e. how much weight the fit puts on its own value when smoothing.
# hatvalues() already accounts for the fex_c weights each model was fit with,
# so squaring and averaging e_(i) across observations reproduces the RMSE
# that n separate leave-one-out regressions would give, from one regression
# instead of n.
loocv_rmse <- function(model) {
    h <- hatvalues(model)
    e <- residuals(model)
    # A factor level with a single training observation (model2's
    # factor(relab) == 8, n = 1) gives that row h_ii = 1 exactly: removing it
    # would eliminate its whole category, so its leave-one-out residual is a
    # 0/0 that is mathematically undefined, not just numerically unstable.
    # Excluded rather than left to floating-point rounding, which can turn
    # 0/0 into an arbitrary finite value or Inf depending on rounding direction.
    valid <- (1 - h) > 1e-8
    sqrt(mean((e[valid] / (1 - h[valid]))^2))
}

loocv_rmse_vals <- map_dbl(all_models, loocv_rmse)


## 8. Model comparison table and export to LaTeX

comparison_table <- tibble(
    model            = names(all_models),
    Model            = model_labels[names(all_models)],
    `LOOCV RMSE`     = loocv_rmse_vals[names(all_models)],
    `Validation RMSE` = validation_rmse[names(all_models)]
) |>
    left_join(fit_stats, by = "model") |>
    select(Model, `LOOCV RMSE`, `Validation RMSE`, AIC, BIC) |>
    arrange(`Validation RMSE`)

view(comparison_table)

force_float_h <- function(x) {
    sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

comparison_table_tex <- comparison_table |>
    kbl(
        format = "latex", booktabs = TRUE, digits = 4,
        caption = "Section 3: model comparison by leave-one-out CV RMSE (leverage shortcut), validation RMSE, AIC and BIC",
        label = "section3_model_comparison"
    ) |>
    kable_styling(latex_options = c("hold_position", "scale_down")) |>
    as.character() |>
    force_float_h()

writeLines(comparison_table_tex, "output/tables/section3_model_comparison.tex")


## 9. Variable importance for the best model (standardized |beta|) and plot

# a. Best model = lowest validation RMSE, the criterion the problem set asks
# the ten specifications to be ranked by.
best_model_name <- names(which.min(validation_rmse))
best_model <- all_models[[best_model_name]]

# b. Standardized coefficients (beta * sd(x) / sd(y)) put continuous
# variables, dummies and factor levels measured on different scales (years,
# hours, 0/1 indicators) on the same footing, so their magnitudes are
# comparable within the plot.
model_matrix <- model.matrix(best_model)[, -1, drop = FALSE]  # drop intercept
x_sd <- apply(model_matrix, 2, sd)
y_sd <- sd(train_data$log_inc)

std_betas <- tidy(best_model) |>
    filter(term != "(Intercept)") |>
    mutate(std_beta = estimate * x_sd[term] / y_sd)

importance_plot <- ggplot(
    std_betas,
    aes(x = reorder(term, abs(std_beta)), y = std_beta, fill = std_beta > 0)
) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c(`TRUE` = "#4daad5", `FALSE` = "#6c0a8a"), guide = "none") +
    labs(
        title = paste("Variable importance:", model_labels[[best_model_name]]),
        subtitle = "Standardized coefficients, ranked by |beta| (best model by validation RMSE)",
        x = NULL,
        y = "Standardized coefficient"
    ) +
    theme_minimal()

ggsave("output/figures/section3_variable_importance.png", importance_plot,
       width = 8, height = 5)
importance_plot
