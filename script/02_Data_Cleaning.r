###############################################################################
# Project Name:      Predicting Income
# Script Name:       02_Data_Cleaning.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Clean the scraped GEIH 2018 sample: keep employed adults,
#                    select the variables of interest and handle missing values.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data
# 3. Filter and select variables
# 4. Handle missing values
# 6. Handle outliers
# 5. Save clean data

################################################################################

# Input:  Raw scraped dataframe from 01_Web_Scrapping.r
# Output: Clean analysis dataframe

################################################################################


## 1. Load libraries

library(pacman)
p_load(
  tidyverse,  # Data manipulation.
  srvyr,      # Survey design and analysis.
  broom,      # Tidy statistical test output.
  kableExtra  # Export tables to LaTeX.
)

# Why use srvyr?
# Uses sandwich estimators to scale uncertainty back to the actual sample size.


## 2. Load data
geih_scrap <- readRDS("data/geih_scrap.rds")

## 3. Initial filter, renaming and selection

# Restricted_sample: only employed adults (age >= 18 and employment status = 1).
# Removed unecessary variables
geih_clean <- geih_scrap %>%
  filter(age >= 18, ocu == 1) %>%
  select(-fweight, -fex_dpto, -depto, -clase)


## 4. Handle missing values

# Characterization of missings
# a. Percentage of missing values by variable
missing_summary <- geih_clean %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

missing_summary

# Delete columns with 100% missing values
cols_to_remove <- names(missing_summary)[missing_summary == 100]
geih_clean <- geih_clean %>% select(-all_of(cols_to_remove))

# The removed columns are:
# p550 - Net income from harvest in last 12 monts
# p7301 - Worked or looked for job earlier
# p7350 - In your last job you where (uneployed)
# p7422 - Income earned from work in last month (for unemployed)?
# p7422s1 - How much (replying to p7422)
# y_gananciaNetaAgro_m - Net income from agric. activities in last 12 months

# Now we extract the missings in vars of special interest
vars_of_interest <- c(
  "age", "maxEducLevel", "y_total_m", "sex"
)

missing_vars <- missing_summary[vars_of_interest]

# b. Separate rows with vs. without a missing y_total_m
geih_clean <- geih_clean %>%
  mutate(missing_income = factor(
    if_else(is.na(y_total_m), "Missing", "Non-missing"),
    levels = c("Non-missing", "Missing")
  ))

geih_missing    <- geih_clean %>% filter(missing_income == "Missing")
geih_nonmissing <- geih_clean %>% filter(missing_income == "Non-missing")

# c. Summary statistics for age, by group
age_summary <- geih_clean %>%
  group_by(missing_income) %>%
  summarise(
    n      = n(),
    mean   = mean(age, na.rm = TRUE),
    sd     = sd(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    min    = min(age, na.rm = TRUE),
    max    = max(age, na.rm = TRUE),
    .groups = "drop"
  )

age_summary

# d. Percentage breakdowns by group (sex, education level)
pct_by_group <- function(data, var) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    count(missing_income, .data[[var]]) %>%
    group_by(missing_income) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()
}

sex_pct  <- pct_by_group(geih_clean, "sex")          # Sex distribution
educ_pct <- pct_by_group(geih_clean, "maxEducLevel") # Education level

sex_pct
educ_pct

# e. T-test comparing the missing vs. non-missing groups
# For age we compare means directly; for categorical variables we compare,
# for each category, the share of the group that falls in it (a dummy
# t-test), which is equivalent to a two-sample test of proportions.
safe_ttest <- function(formula, data) {
  tt <- tryCatch(t.test(formula, data = data), error = function(e) NULL)
  if (is.null(tt)) {
    return(tibble(
      estimate1 = NA_real_, estimate2 = NA_real_,
      estimate = NA_real_, statistic = NA_real_, p.value = NA_real_
    ))
  }
  broom::tidy(tt) %>% select(estimate1, estimate2, estimate, statistic, p.value)
}

ttest_categorical <- function(data, var) {
  levels_var <- data %>%
    filter(!is.na(.data[[var]])) %>%
    distinct(.data[[var]]) %>%
    pull() %>%
    sort()

  map_df(levels_var, function(lvl) {
    d <- data %>% mutate(dummy = as.numeric(.data[[var]] == lvl))
    safe_ttest(dummy ~ missing_income, d) %>%
      mutate(variable = var, category = as.character(lvl), .before = 1)
  })
}

ttest_age <- safe_ttest(age ~ missing_income, geih_clean) %>%
  mutate(variable = "age", category = NA_character_, .before = 1)

balance_table <- bind_rows(
  ttest_age,
  ttest_categorical(geih_clean, "sex"),
  ttest_categorical(geih_clean, "maxEducLevel"),
  ttest_categorical(geih_clean, "relab"),
  ttest_categorical(geih_clean, "formal")
) %>%
  rename(
    mean_nonmissing = estimate1, mean_missing = estimate2, diff = estimate
  ) %>%
  mutate(across(
    c(mean_nonmissing, mean_missing, diff, statistic, p.value), ~ round(., 4)
  ))

# f. Final table: means/percentages for each group, their difference and the
# t-test p-value (row "age" reports means in years; all other rows report
# proportions, i.e. category shares expressed on a 0-1 scale).
balance_table

# g. Label sex and education levels, and collapse variable/category into a
# single identifying column (the variable's own name when it has no
# categories, e.g. "age"; its category's name otherwise, e.g. "Female").
# Sex coding inferred from gender-skewed occupations (oficio): housemaids
# (54) are almost all sex == 0, drivers (98) almost all sex == 1.
sex_labels <- c("0" = "Female", "1" = "Male")

# Source: maxEducLevel codebook (Categorias.pdf)
educ_labels <- c(
  "1" = "None",
  "2" = "Preschool",
  "3" = "Primary incomplete (1-4)",
  "4" = "Primary complete (5)",
  "5" = "Secondary incomplete (6-10)",
  "6" = "Secondary complete (11)",
  "7" = "Tertiary",
  "9" = "N/A"
)

# Source: relab codebook (Categorias.pdf)
relab_labels <- c(
  "1" = "Private employee",
  "2" = "Government employee",
  "3" = "Domestic worker",
  "4" = "Self-employed",
  "5" = "Employer",
  "6" = "Unpaid family worker",
  "7" = "Unpaid worker (other household)",
  "8" = "Day laborer",
  "9" = "Other"
)

formal_labels <- c("0" = "Informal", "1" = "Formal")

balance_table <- balance_table %>%
  mutate(variable = case_when(
    variable == "age"          ~ "Age",
    variable == "sex"          ~ sex_labels[category],
    variable == "maxEducLevel" ~ educ_labels[category],
    variable == "relab"        ~ relab_labels[category],
    variable == "formal"       ~ formal_labels[category],
    TRUE                       ~ category
  )) %>%
  select(-category)

balance_table

# h. Export the balance table to LaTeX
balance_table_tex <- balance_table %>%
  rename(
    Variable = variable,
    `Non-missing` = mean_nonmissing, `Missing` = mean_missing,
    `Diff.` = diff, `t-stat` = statistic, `p-value` = p.value
  ) %>%
  kbl(
    format = "latex", booktabs = TRUE, digits = 4,
    caption = "Balance table: missing vs. non-missing income",
    label = "balance"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

writeLines(as.character(balance_table_tex), "output/tables/balance_table.tex")

# i. Characterizing the missing-income group by employment relationship
# With the sample restricted to occupied adults (ocu == 1), missing y_total_m
# is concentrated among workers without a fixed wage: unpaid family/other-
# household workers (relab 6-7) are almost never in the non-missing group,
# and self-employed, employer and informal workers are all over-represented
# among the missing (see balance_table rows above).
relab_pct  <- pct_by_group(geih_clean, "relab")
formal_pct <- pct_by_group(geih_clean, "formal")

relab_pct
formal_pct

## 5. Save clean data

# geih_clean is exported as-is: missing values (e.g. y_total_m) are left
# untouched here, not dropped or imputed.
saveRDS(geih_clean, file = "data/geih_clean.rds")

################################End of script###################################
