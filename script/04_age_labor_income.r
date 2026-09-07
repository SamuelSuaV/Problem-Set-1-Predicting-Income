###############################################################################
# Project Name:      Predicting Income
# Script Name:       04_Age_Labor_Income.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Estimate the unconditional and conditional age-labour
#                    income profiles, the implied peak age of each one and its
#                    bootstrap confidence interval.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data and build model variables
# 3. Age-income profile regressions
# 4. Implied peak age
# 5. Bootstrap confidence intervals for the peak age
# 6. Regression table and export to LaTeX
# 7. Age-income profile plot

################################################################################

# Input:  Clean analysis dataframe from 02_Data_Cleaning.r
# Output: Regression table (output/tables/age_income_regression.tex) and
#         age-income profile figure (output/figures/age_income_profile.png)

################################################################################


## 1. Load libraries
#install.packages("pacman")
library(pacman)
p_load(
    tidyverse,
    boot,
    broom,
    kableExtra,
    fixest)

## 2. Load data and build model variables

clean_data <- readRDS("data/geih_clean.rds")

# id_hogar identifies a household: directorio is the dwelling and secuencia_p
# the household within it, so only the pair is unique. It is the level we
# cluster standard errors on, because 70% of the analysis sample shares a
# household with someone else in it.
clean_data <- clean_data |>
    filter(y_total_m > 0) |>
    mutate(
      age2     = age^2,
      log_inc  = log(y_total_m),  #creamos una variable de logaritmo del ingreso para poder hacer la regresión
      id_hogar = paste(directorio, secuencia_p, sep = "_")
    )

# relab labels (duplicated from 02_Data_Cleaning.r: every script runs on its
# own, so the vector defined there is not available here).
relab_labels <- c(
  "1" = "Obrero o empleado de empresa particular",
  "2" = "Obrero o empleado del gobierno",
  "3" = "Empleado doméstico",
  "4" = "Trabajador por cuenta propia",
  "5" = "Patrón o empleador",
  "6" = "Trabajador familiar sin remuneración",
  "7" = "Trabajador sin remuneración en empresas o negocios de otros hogares",
  "8" = "Jornalero o peón",
  "9" = "Otro"
)

clean_data |>
  count(relab) |>
  mutate(relab_label = relab_labels[as.character(relab)])

## 3. Age-income profile regressions

# Every regression below is weighted by fex_c, the person-level expansion
# factor: the GEIH is a complex survey, not a simple random sample, so the
# profile we report is a statement about Bogota's workers and not about the
# 14,764 respondents we happen to observe. This also keeps the section
# consistent with the weighted descriptives in 03_Data_Description.r.
# In practice the choice is innocuous here: dropping the weights moves the
# implied peak age by 0.11 years (model 1) and 0.02 years (model 2), an order
# of magnitude less than the width of their bootstrap confidence intervals.

# Standard errors are clustered at the household level (id_hogar): the GEIH
# samples dwellings and interviews every member of the household, so the
# household - not the person - is the sampling unit, and 70% of the analysis
# sample shares a household with someone else in it. Clustering leaves the
# coefficients untouched and only affects inference. The problem set does not
# ask for this; it is our own decision.
#
# Decomposing the increase over the classical (iid) standard error of beta_age:
#   classical 0.00325 -> robust (HC) 0.00401 -> clustered by household 0.00419
# so most of it (x1.24) is heteroskedasticity, not clustering: households are
# tiny here (1.7 people on average), which caps how much clustering can matter
# even though within-household correlation is fairly strong (rho ~ 0.13).
#
# Robustness note: clustering by occupation (oficio) instead would double the
# standard errors (0.00910, x2.27), because those clusters average 187 people -
# a weak within-occupation correlation (rho ~ 0.02) compounded over many pairs.
# We do not adopt it: occupations are not a sampling unit, so that correlation
# reflects an omitted variable rather than the survey design. Either way no
# qualitative conclusion changes (beta_age keeps t ~ 9.5).

# a. Unconditional profile
model1 <- feols(log_inc ~ age + age2,
                data = clean_data, weights = ~fex_c, vcov = ~id_hogar)
summary(model1) # revisamos el resumen del modelo

# b. Conditional profile: adds total hours worked and employment type, and no
# other controls (as required by the problem set).
model2 <- feols(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                data = clean_data, weights = ~fex_c, vcov = ~id_hogar)
summary(model2) # revisamos el resumen del modelo


## 4. Implied peak age

# The profile is a parabola, so the predicted income peaks where its slope is
# zero: age* = -beta_age / (2 * beta_age2).

# a. Unconditional profile
peak_age1 <- -coef(model1)["age"] / (2 * coef(model1)["age2"])

# b. Conditional profile
peak_age2 <- -coef(model2)["age"] / (2 * coef(model2)["age2"])


## 5. Bootstrap confidence intervals for the peak age

B <- 5000 # number of bootstrap samples

# Each replicate refits the same weighted specification as in section 3, so the
# bootstrap distribution is centred on the estimates we actually report.
#
# This is a CLUSTER bootstrap: the resampling unit is the household, not the
# person. Resampling people one by one would treat the sample as 14,764
# independent draws when 70% of it shares a household, and would understate the
# uncertainty exactly the way unclustered standard errors do (see section 3).
# So each replicate draws 8,806 households with replacement and keeps every
# member of the households drawn - a household drawn twice contributes its
# members twice.

# filas_por_hogar maps each household to the row positions of its members.
# Building it once and indexing into it per replicate is much faster than
# joining the household draw back onto clean_data 3,000 times.
filas_por_hogar <- split(seq_len(nrow(clean_data)), clean_data$id_hogar)

# boot() resamples the rows of whatever it is given, so we hand it one row per
# household: `index` then indexes households, and unlist() expands the drawn
# households back into the row positions of their members.
hogares <- tibble(id_hogar = names(filas_por_hogar))

# a. Unconditional profile
peak_age_stat1 <- function(data, index) {
  filas <- unlist(filas_por_hogar[index], use.names = FALSE)
  model <- feols(log_inc ~ age + age2,
                 data = clean_data[filas, ], weights = ~fex_c)
  unname(-coef(model)["age"] / (2 * coef(model)["age2"]))
}

set.seed(123) # for reproducibility
boot_peak_age1 <- boot(hogares, peak_age_stat1, R = B)
boot_peak_age1          # original, bias y std. error bootstrap
sd(boot_peak_age1$t)    # sd bootstrap (equivalente al "std. error" de arriba)
boot.ci(boot_peak_age1, type = "perc")  # IC 95%


# b. Conditional profile
peak_age_stat2 <- function(data, index) {
  filas <- unlist(filas_por_hogar[index], use.names = FALSE)
  model <- feols(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                 data = clean_data[filas, ], weights = ~fex_c)
  unname(-coef(model)["age"] / (2 * coef(model)["age2"]))
}

set.seed(123) # for reproducibility
boot_peak_age2 <- boot(hogares, peak_age_stat2, R = B)
boot_peak_age2
sd(boot_peak_age2$t)
boot.ci(boot_peak_age2, type = "perc")  # IC 95%


## 6. Regression table and export to LaTeX

# a. One row per coefficient, one pair of columns per specification. Terms that
# only appear in the conditional model are left as NA on the model 1 side.
table_regression <- full_join(
  tidy(model1) |> select(term, estimate, std.error),
  tidy(model2) |> select(term, estimate, std.error),
  by = "term",
  suffix = c("_model1", "_model2")
)

# b. Extra rows the coefficient table does not carry: peak age, its bootstrap
# CI and the in-sample fit measure required by the problem set.
extra_rows <- tibble(
  term = c("Peak age", "Peak age CI lower", "Peak age CI upper", "R-squared"),
  estimate_model1 = c(
    peak_age1,
    boot.ci(boot_peak_age1, type = "perc")$percent[4],
    boot.ci(boot_peak_age1, type = "perc")$percent[5],
    glance(model1)$r.squared
  ),
  std.error_model1 = NA_real_,
  estimate_model2 = c(
    peak_age2,
    boot.ci(boot_peak_age2, type = "perc")$percent[4],
    boot.ci(boot_peak_age2, type = "perc")$percent[5],
    glance(model2)$r.squared
  ),
  std.error_model2 = NA_real_
)

table_regression <- bind_rows(table_regression, extra_rows)
view(table_regression)

# c. Export to LaTeX, same pattern as 02_Data_Cleaning.r: kbl with booktabs,
# forcing [H] instead of kableExtra's [!h] so the table cannot float past its
# own section in the compiled write-up (requires \usepackage{float}).
force_float_h <- function(x) {
  sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

table_regression_tex <- table_regression |>
  rename(
    Term = term,
    `Model 1` = estimate_model1,
    `SE (Model 1)` = std.error_model1,
    `Model 2` = estimate_model2,
    `SE (Model 2)` = std.error_model2
  ) |>
  kbl(
    # NOTE: with digits = 4 the SE of age2 prints as 0.0000 (its real value is
    # ~0.0000376, too small for 4 decimals) - revisit before using this table
    # in the slides (more decimals for that row only, or scientific notation).
    format = "latex", booktabs = TRUE, digits = 4,
    caption = "Age-income profile: unconditional vs. conditional",
    label = "age_income"
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  as.character() |>
  force_float_h()

writeLines(table_regression_tex, "output/tables/age_income_regression.tex")


## 7. Age-income profile plot

# a. Age grid over the range observed in the sample.
age_grid <- seq(min(clean_data$age), max(clean_data$age), by = 1)

# b. Unconditional profile: model1 only depends on age, nothing else to hold
# fixed.
profile1 <- tibble(age = age_grid, age2 = age_grid^2)
profile1$log_inc_pred <- predict(model1, newdata = profile1)

# c. Conditional profile: model2 also depends on totalHoursWorked and relab, so
# we hold them at a "reference worker" (average hours, most frequent relab
# category) and vary age only. Since the model has no interactions with age,
# this choice only shifts the curve vertically: it changes neither its shape
# nor the peak age, so the comparison below is robust to it.
ref_hours <- mean(clean_data$totalHoursWorked, na.rm = TRUE)
ref_relab <- 1  # Obrero o empleado de empresa particular (most common category)

profile2 <- tibble(
  age = age_grid,
  age2 = age_grid^2,
  totalHoursWorked = ref_hours,
  relab = ref_relab
)
profile2$log_inc_pred <- predict(model2, newdata = profile2)

# d. Both profiles in one data frame so they can be plotted side by side.
profiles <- bind_rows(
  profile1 |> mutate(model = "Unconditional"),
  profile2 |> mutate(model = "Conditional")
)

# e. Plot both curves, marking each specification's peak age. Only the shape
# and the peak age are comparable across curves: their vertical position
# depends on the reference worker chosen above.
peaks <- tibble(
  model = c("Unconditional", "Conditional"),
  peak_age = c(unname(peak_age1), unname(peak_age2))
)

age_profile_plot <- ggplot(profiles, aes(x = age, y = log_inc_pred, color = model)) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = peaks, aes(xintercept = peak_age, color = model),
    linetype = "dashed", show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Specification",
    values = c(Unconditional = "#6c0a8a", Conditional = "#4daad5")
  ) +
  labs(
    title = "Age-income profile: unconditional vs. conditional",
    subtitle = "Dashed lines mark the implied peak age of each specification",
    x = "Age",
    y = "Predicted log(total monthly income)"
  ) +
  theme_minimal()

ggsave("output/figures/age_income_profile.png", age_profile_plot, width = 8, height = 5)
age_profile_plot
