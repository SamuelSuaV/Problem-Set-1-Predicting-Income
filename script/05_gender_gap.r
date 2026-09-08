###############################################################################
# Project Name:      Predicting Income
# Script Name:       05_Gender_Gap.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Estimate the unconditional and conditional gender labour
#                    income gap, recover the gender coefficient via the
#                    Frisch-Waugh-Lovell decomposition with analytical and
#                    bootstrap standard errors, and compare the predicted
#                    age-income profiles of men and women.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data and build model variables
# 3. Gender gap regressions (unconditional and conditional)
# 4. FWL decomposition of the conditional gender gap
# 5. Bootstrap standard error for the FWL coefficient
# 6. Predicted age-income profiles by sex
# 7. Regression table and export to LaTeX
# 8. Age-income profile by sex plot

################################################################################

# Input:  Clean analysis dataframe from 02_Data_Cleaning.r
# Output: Regression table (output/tables/gender_gap_regression.tex) and
#         age-income profile by sex figure
#         (output/figures/age_income_profile_sex.png)

################################################################################


## 1. Load libraries

library(pacman)
p_load(
    tidyverse,
    boot,
    broom,
    kableExtra,
    fixest,
    sandwich)


## 2. Load data and build model variables

clean_data <- readRDS("data/geih_clean.rds")

# Same analysis sample as 04_Age_Labor_Income.r: employed adults with strictly
# positive labour income (zeros and missings cannot enter a log outcome).
# female is built from the codebook coding of sex (=1 male, =0 female).
# TEMPORAL: el filtro de maxEducLevel ya quedo en 02_Data_Cleaning.r, pero ese
# script todavia no se ha vuelto a correr, asi que geih_clean.rds aun trae esa
# fila. Borrar esta condicion cuando 02 se corra de nuevo.
clean_data <- clean_data |>
  filter(y_total_m > 0, !is.na(maxEducLevel)) |>
  mutate(
    age2    = age^2,
    log_inc = log(y_total_m),
    female  = as.numeric(sex == 0),
    id_hogar = paste(directorio, secuencia_p, sep = "_" )
  )


## 3. Gender gap regressions (unconditional and conditional)

# All regressions are weighted by fex_c, the person-level expansion factor, for
# the same reason as in 04_Age_Labor_Income.r: the GEIH is a complex survey, so
# the gap we report is a statement about Bogota's workers, not about the
# respondents we happen to observe. Standard errors are
# heteroskedasticity-robust, for the same reason and after the same
# deliberation as in 04_Age_Labor_Income.r: clustering (by household or by
# occupation) was considered and rejected, because neither is the level at
# which the thought experiment assigns sex.

# a. Unconditional gap: the raw difference in mean log income between women and
# men. It bundles together every channel (education, hours, occupation, ...).
gap_uncond <- feols(log_inc ~ female,
                    data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(gap_uncond)


# b. Conditional gap, human capital controls. Age and education are
# predetermined with respect to sex, so they are confounders rather than
# mechanisms: this is our preferred specification and the one used for the FWL
# decomposition in section 4.
gap_hk <- feols(log_inc ~ female + age + age2 + factor(maxEducLevel),
                data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(gap_hk)

# c. Conditional gap, adding hours worked. The mandated outcome (y_total_m) is
# *monthly* income, which scales mechanically with hours, and women work fewer
# paid hours on average. Holding hours fixed therefore moves the estimate
# towards an hourly-pay comparison. Hours are chosen by the worker and are
# themselves shaped by sex, so this is a post-treatment control: it answers a
# different question than (b), it does not "improve" on it.
gap_hours <- feols(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                     totalHoursWorked,
                   data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(gap_hours)

# d. Gap in percentage terms: with a log outcome, exp(beta) - 1 converts the
# coefficient into the proportional income difference between women and men.
gap_pct <- function(model) (exp(coef(model)["female"]) - 1) * 100

gap_pct(gap_uncond)
gap_pct(gap_hk)
gap_pct(gap_hours)




#### FWL

#stage 1: regress income on the controls
stage1_FWL <- feols(log_inc ~ age + age2 + factor(maxEducLevel),
                    data = clean_data, weights = ~fex_c)
residuals_stage1 <- resid(stage1_FWL)

#stage 2: regress female on controls
stage2_FWL <- feols(female ~ age + age2 + factor(maxEducLevel),
                    data = clean_data, weights = ~fex_c)
residuals_stage2 <- resid(stage2_FWL)

# Both sets of residuals go back into clean_data as columns: stage 3 needs a
# data frame where feols can find fex_c in order to weight the regression. The
# rows line up because the analysis sample has no missing values left, so no
# observation was dropped in stages 1 and 2.
clean_data <- clean_data |>
  mutate(res_income = residuals_stage1, res_female = residuals_stage2)


#stage 3: regress residuals of stage 1 on residuals of stage 2
stage3_FWL <- feols(res_income ~ res_female,
                    data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(stage3_FWL)

# The point estimate matches the full regression exactly: this is what the
# problem set asks us to explain. Stages 1 and 2 strip out everything the
# controls account for, so stage 3 identifies the gender coefficient off the
# variation in female that is orthogonal to age and education - precisely the
# variation the full regression uses.
c(full = unname(coef(gap_hk)["female"]),
  fwl  = unname(coef(stage3_FWL)["res_female"]))

#degrees of fredom for the full model
df_full_model <- gap_hk %>%
  degrees_freedom("resid")

# A note on the standard errors: stage 3 reports 0.013203 while the full
# regression reports 0.013206. They differ because feols only sees two
# parameters in stage 3 (df = n - 2 = 14,761) and cannot know that stages 1
# and 2 already spent degrees of freedom on the controls (df = n - k = 14,754).
# Rescaling the stage-3 standard error by sqrt(14761/14754) = 1.000237 recovers
# the full-regression one exactly. We report the full regression's standard
# error, which is the correct one; the 0.02% gap is a bookkeeping artefact of
# running FWL by hand, not a difference between the two estimators.

 #degrees of fredom for the FWL model
  df_FWL_model <- stage3_FWL %>%
  degrees_freedom("resid")


#standard errors
## stage3_FWL
se_FWL <- se(stage3_FWL)["res_female"]

## full model
se_full_model <- se(gap_hk)["female"]

## rescaling stage 3 by the ratio of degrees of freedom recovers the full
## regression's standard error exactly
se_FWL_corrected <- se_FWL * sqrt(df_FWL_model / df_full_model)

c(fwl_raw        = unname(se_FWL),
  fwl_corrected  = unname(se_FWL_corrected),
  full_model     = unname(se_full_model))


## 5. Bootstrap standard error for the gender coefficient

# The problem set asks for both analytical and bootstrap standard errors. The
# analytical one is the robust SE above; this is the bootstrap counterpart.
#
# Resampling is at the individual level, matching those robust standard errors:
# the ordinary pairs bootstrap is asymptotically equivalent to the White
# variance estimator, so the two should land on the same number.
#
# We refit the full specification rather than redoing the three FWL stages in
# every replicate: section 4 showed both give numerically identical gender
# coefficients, so the extra two regressions per replicate would only cost time.

B <- 5000 # number of bootstrap samples

# vcovBS does not accept fixest objects, so the same specification is refitted
# with lm() purely as a vehicle for the bootstrap. The coefficients are
# identical to gap_hk; only the class of the object differs.
gap_hk_lm <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                data = clean_data, weights = fex_c)

set.seed(123) # for reproducibility
vcov_boot <- vcovBS(gap_hk_lm, R = B)
se_boot   <- sqrt(vcov_boot["female", "female"])

# Analytical and bootstrap standard errors side by side.
c(analytical = unname(se_full_model),
  bootstrap  = unname(se_boot))


## 6. Predicted age-income profiles by sex

# a. The specifications above give men and women the same age profile shifted
# vertically by the gender dummy, so both would peak at exactly the same age
# and comparing their peaks would be vacuous. Interacting female with age and
# age2 lets each sex have its own curvature, which is what the problem set
# asks us to compare.
gap_interact <- feols(
  log_inc ~ female + age + age2 + female:age + female:age2 +
    factor(maxEducLevel),
  data = clean_data, weights = ~fex_c, vcov = "hetero"
)
summary(gap_interact)

# b. Implied peak age by sex. For men (female = 0) the interactions drop out
# and the usual formula applies. For women (female = 1) they switch on, so the
# linear and quadratic terms become the sums below.
peak_age_by_sex <- function(model) {
  b <- coef(model)
  c(
    men   = unname(-b["age"] / (2 * b["age2"])),
    women = unname(-(b["age"] + b["female:age"]) /
                     (2 * (b["age2"] + b["female:age2"])))
  )
}

peak_age_by_sex(gap_interact)

# c. Confidence intervals for the two peak ages. Back to boot() by hand rather
# than vcovBS: a peak age is a ratio of coefficients, and the sandwich-style
# functions only return variance matrices for the coefficients themselves.
#
# We also bootstrap the difference between the two peaks. That is the quantity
# that actually answers whether men and women peak at different ages: the
# individual interaction coefficients are not significant on their own, but the
# peak ages are a non-linear combination of them, so their difference has to be
# tested directly rather than read off those t-statistics.
peak_sex_stat <- function(data, index) {
  model <- feols(
    log_inc ~ female + age + age2 + female:age + female:age2 +
      factor(maxEducLevel),
    data = data[index, ], weights = ~fex_c
  )
  peaks <- peak_age_by_sex(model)
  c(peaks, difference = unname(peaks["men"] - peaks["women"]))
}

set.seed(123) # for reproducibility
boot_peak_sex <- boot(clean_data, peak_sex_stat, R = B)
boot_peak_sex

boot.ci(boot_peak_sex, type = "perc", index = 1)  # men
boot.ci(boot_peak_sex, type = "perc", index = 2)  # women
boot.ci(boot_peak_sex, type = "perc", index = 3)  # difference


## 7. Regression table and export to LaTeX

# The problem set asks for a table comparing the unconditional and conditional
# gaps with analytical and bootstrap standard errors and a measure of in-sample
# fit. Since the object of interest is the gender coefficient rather than the
# whole coefficient vector, the table carries one row per specification.

# a. Bootstrap standard errors for every specification, not just the preferred
# one. Same route as in section 5: refit with lm() because vcovBS does not take
# fixest objects.
se_boot_of <- function(formula) {
  m <- lm(formula, data = clean_data, weights = fex_c)
  set.seed(123)
  sqrt(vcovBS(m, R = B)["female", "female"])
}

f_uncond <- log_inc ~ female
f_hk     <- log_inc ~ female + age + age2 + factor(maxEducLevel)
f_hours  <- log_inc ~ female + age + age2 + factor(maxEducLevel) +
  totalHoursWorked

# b. One row per specification.
gap_table <- tibble(
  Specification = c(
    "(1) Unconditional",
    "(2) + age, age2, education",
    "(3) + hours worked"
  ),
  Female    = c(coef(gap_uncond)["female"], coef(gap_hk)["female"],
                coef(gap_hours)["female"]),
  SE_rob    = c(se(gap_uncond)["female"], se(gap_hk)["female"],
                se(gap_hours)["female"]),
  SE_boot   = c(se_boot_of(f_uncond), se_boot_of(f_hk), se_boot_of(f_hours)),
  Gap_pct   = c(gap_pct(gap_uncond), gap_pct(gap_hk), gap_pct(gap_hours)),
  R2        = c(r2(gap_uncond, "r2"), r2(gap_hk, "r2"), r2(gap_hours, "r2")),
  N         = c(nobs(gap_uncond), nobs(gap_hk), nobs(gap_hours))
)

gap_table

# c. Implied peak ages by sex, with their bootstrap confidence intervals.
ci_of <- function(i) boot.ci(boot_peak_sex, type = "perc", index = i)$percent[4:5]

peak_table <- tibble(
  Group    = c("Men", "Women", "Difference (men - women)"),
  Peak_age = boot_peak_sex$t0,
  CI_lower = c(ci_of(1)[1], ci_of(2)[1], ci_of(3)[1]),
  CI_upper = c(ci_of(1)[2], ci_of(2)[2], ci_of(3)[2])
)

peak_table

# d. Export both to LaTeX, same pattern as 02 and 04: kbl with booktabs, and
# [!h] forced to [H] so the tables cannot float out of their own section.
force_float_h <- function(x) {
  sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

gap_table_tex <- gap_table |>
  rename(
    `Female` = Female, `SE (robust)` = SE_rob,
    `SE (bootstrap)` = SE_boot, `Gap (\\%)` = Gap_pct, `R$^2$` = R2
  ) |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 4, escape = FALSE,
    caption = "Gender labour income gap: unconditional and conditional",
    label = "gender_gap"
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  as.character() |>
  force_float_h()

writeLines(gap_table_tex, "output/tables/gender_gap_regression.tex")

peak_table_tex <- peak_table |>
  rename(`Peak age` = Peak_age, `CI lower` = CI_lower, `CI upper` = CI_upper) |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 2,
    caption = "Implied peak age by sex, with 95\\% bootstrap confidence intervals",
    label = "peak_age_sex"
  ) |>
  kable_styling(latex_options = c("hold_position")) |>
  as.character() |>
  force_float_h()

writeLines(peak_table_tex, "output/tables/gender_peak_age_by_sex.tex")


## 8. Age-income profile by sex plot

# a. Age grid over the observed range, one copy per sex. Education is held at
# its modal category: as in 04, with no age-education interaction in the model
# this only shifts both curves vertically - it changes neither their shape nor
# the peak ages, so the comparison is unaffected by the choice.
ref_educ <- clean_data |> count(maxEducLevel) |> slice_max(n, n = 1) |>
  pull(maxEducLevel)

# The grid stops at 70 rather than at the sample maximum of 91: the 99th
# percentile of age is 71, so past that the curves are fitted on about 1% of the
# observations. Plotting to 91 would hand a third of the chart's width to that
# 1% and let the parabola's extrapolated dive dominate the picture.
age_max_plot <- 70
age_grid <- seq(min(clean_data$age), age_max_plot, by = 1)

profiles_sex <- expand_grid(age = age_grid, female = c(0, 1)) |>
  mutate(age2 = age^2, maxEducLevel = ref_educ)

profiles_sex$log_inc_pred <- predict(gap_interact, newdata = profiles_sex)

profiles_sex <- profiles_sex |>
  mutate(sexo = if_else(female == 1, "Women", "Men"))

# b. Plot both profiles with their peak ages marked.
# lab_hjust pushes each peak label away from the other so they cannot overlap
# when the two peaks sit close together.
peaks_sex <- tibble(
  sexo     = c("Men", "Women"),
  peak_age = c(boot_peak_sex$t0[1], boot_peak_sex$t0[2])
) |>
  mutate(lab_hjust = if_else(peak_age == min(peak_age), 1.1, -0.1))

# The outcome is in logs, which nobody can read off an axis, so the breaks sit
# at round peso amounts (doubling, the natural spacing on a log scale) and are
# labelled in pesos. The curve is unchanged; only the axis becomes legible.
peso_breaks <- c(6e5, 8e5, 1e6, 1.5e6, 2e6, 3e6)
peso_labels <- c("$600K", "$800K", "$1.0M", "$1.5M", "$2.0M", "$3.0M")

# Series are labelled on the curves themselves, so identity never depends on
# matching a colour back to a legend.
series_labels_sex <- profiles_sex |>
  group_by(sexo) |>
  slice_max(age, n = 1) |>
  ungroup()

y_top <- max(profiles_sex$log_inc_pred)

# The band between the curves is the gender gap at each age, so shading it
# turns the widening gap into something you can see rather than infer.
gap_band <- profiles_sex |>
  select(age, sexo, log_inc_pred) |>
  pivot_wider(names_from = sexo, values_from = log_inc_pred)

age_profile_sex_plot <- ggplot(profiles_sex,
                               aes(x = age, y = log_inc_pred, color = sexo)) +
  geom_ribbon(
    data = gap_band, aes(x = age, ymin = Women, ymax = Men),
    inherit.aes = FALSE, fill = "grey55", alpha = 0.15
  ) +
  geom_vline(
    data = peaks_sex, aes(xintercept = peak_age, color = sexo),
    linetype = "dashed", linewidth = 0.5, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.9) +
  geom_text(
    data = peaks_sex,
    aes(x = peak_age, y = y_top, label = sprintf("%.1f", peak_age),
        hjust = lab_hjust),
    vjust = -1.2, size = 3.4, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(
    data = series_labels_sex, aes(label = sexo),
    hjust = -0.2, size = 3.8, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Sex",
    values = c(Men = "#235da3", Women = "#ad0d5d")
  ) +
  scale_y_continuous(breaks = log(peso_breaks), labels = peso_labels,
                     expand = expansion(mult = c(0.05, 0.14))) +
  scale_x_continuous(breaks = seq(20, 70, by = 10)) +
  coord_cartesian(xlim = c(min(age_grid), max(age_grid) + 6), clip = "off") +
  labs(
    title = "Women's earnings peak almost five years earlier than men's",
    x = "Age",
    y = NULL,
    caption = paste0(
      "Predicted monthly labour income, education held at its modal level. ",
      "The shaded band is the gender gap,\nwhich widens from 19% at age 25 to ",
      "38% at age 55. Dashed lines mark each profile's implied peak age.\n",
      "Vertical axis on a log scale; ages shown to 70, the sample's 99th ",
      "percentile. GEIH 2018, Bogota: employed\nadults 18+ with positive ",
      "labour income (n = 14,763), weighted by fex_c."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position       = "none",
    panel.grid.minor      = element_blank(),
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_line(color = "grey92", linewidth = 0.4),
    plot.title            = element_text(face = "bold", size = 14,
                                         margin = margin(b = 14)),
    plot.title.position   = "plot",
    plot.caption          = element_text(color = "grey45", hjust = 0, size = 8),
    plot.caption.position = "plot",
    plot.margin           = margin(12, 28, 10, 10)
  )

ggsave("output/figures/age_income_profile_sex.png", age_profile_sex_plot,
       width = 8, height = 5, dpi = 300)
age_profile_sex_plot

