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
    kableExtra)


## 2. Load data and build model variables

clean_data <- readRDS("data/geih_clean.rds")

# Same analysis sample as 04_Age_Labor_Income.r: employed adults with strictly
# positive labour income (zeros and missings cannot enter a log outcome).
# female is built from the codebook coding of sex (=1 male, =0 female); the
# problem set asks for the coefficient on Female, not on male.
clean_data <- clean_data |>
  filter(y_total_m > 0) |>
  mutate(
    age2    = age^2,
    log_inc = log(y_total_m),
    female  = as.numeric(sex == 0)
  )


## 3. Gender gap regressions (unconditional and conditional)

# All regressions are weighted by fex_c, the person-level expansion factor, for
# the same reason as in 04_Age_Labor_Income.r: the GEIH is a complex survey, so
# the gap we report is a statement about Bogota's workers, not about the
# respondents we happen to observe.

# a. Unconditional gap: the raw difference in mean log income between women and
# men. It bundles together every channel (education, hours, occupation, ...).
gap_uncond <- lm(log_inc ~ female, data = clean_data, weights = fex_c)
summary(gap_uncond)

# b. Conditional gap, human capital controls. Age and education are
# predetermined with respect to sex, so they are confounders rather than
# mechanisms: this is our preferred specification and the one used for the FWL
# decomposition in section 4.
gap_hk <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
             data = clean_data, weights = fex_c)
summary(gap_hk)

# c. Conditional gap, adding hours worked. The mandated outcome (y_total_m) is
# *monthly* income, which scales mechanically with hours, and women work fewer
# paid hours on average. Holding hours fixed therefore moves the estimate
# towards an hourly-pay comparison. Hours are chosen by the worker and are
# themselves shaped by sex, so this is a post-treatment control: it answers a
# different question than (b), it does not "improve" on it.
gap_hours <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                  totalHoursWorked,
                data = clean_data, weights = fex_c)
summary(gap_hours)

# d. Gap in percentage terms: with a log outcome, exp(beta) - 1 converts the
# coefficient into the proportional income difference between women and men.
gap_pct <- function(model) (exp(coef(model)["female"]) - 1) * 100

gap_pct(gap_uncond)
gap_pct(gap_hk)
gap_pct(gap_hours)
