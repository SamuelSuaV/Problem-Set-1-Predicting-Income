###############################################################################
# Project Name:      Predicting Income
# Script Name:       03_Data_Description.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Descriptive statistics and exploratory plots for the
#                    clean GEIH 2018 analysis sample.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data and build labels/groups
# 3. Helper functions for descriptive tables
# 4. General descriptive statistics
# 5. Descriptive statistics by group (sex, age range, formality)
# 6. Income distribution histograms by group
# 7. Income vs. age scatter plots by group
# 8. Export tables to LaTeX

################################################################################

# Input:  Clean analysis dataframe from 02_Data_Cleaning.r
# Output: Summary statistics tables (output/tables/*.tex) and income
#         distribution figures (output/figures/*.png)

################################################################################


## 1. Load libraries

library(pacman)

p_load(
  tidyverse, # Manipulación de datos y gráficos.
  moments,   # Skewness y kurtosis.
  scales,    # Paletas de color y formato de ejes.
  kableExtra # Export tables to LaTeX.
)


## 2. Load data and build labels/groups

geih_clean <- readRDS("data/geih_clean.rds")

# Variables de interés para la caracterización: no muchas, pero cubren lo
# demográfico (edad, género, educación), lo laboral (informalidad, horas
# trabajadas) y el resultado de interés (ingreso).
continuous_vars <- c("age", "hoursWorkUsual", "y_total_m")
categorical_vars <- c("sex", "maxEducLevel", "formal")

var_labels <- c(
  age            = "Age",
  hoursWorkUsual = "Usual weekly hours worked",
  y_total_m      = "Total monthly income"
)

# Labels reused from 02_Data_Cleaning.r (same codebook/coding criteria).
sex_labels <- c("0" = "Female", "1" = "Male")

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

formal_labels <- c("0" = "Informal", "1" = "Formal")

category_labels <- list(
  sex          = sex_labels,
  maxEducLevel = educ_labels,
  formal       = formal_labels
)

# Four age ranges based on the sample's quartiles, so groups are comparable
# in size (18-28, 29-38, 39-50, 51-94).
age_breaks <- c(18, 28, 38, 50, 94)
age_group_labels <- c("18-28", "29-38", "39-50", "51-94")

geih_clean <- geih_clean %>%
  mutate(
    age_group = cut(
      age,
      breaks = age_breaks, labels = age_group_labels, include.lowest = TRUE
    )
  )


## 3. Helper functions for descriptive tables

# a. Classic descriptive statistics for continuous variables, optionally
# by group. Includes skewness and (non-excess) kurtosis: kurtosis == 3
# corresponds to a normal distribution.
describe_continuous <- function(data, vars, group_var = NULL) {
  data %>%
    { if (!is.null(group_var)) group_by(., across(all_of(group_var))) else . } %>%
    summarise(
      across(
        all_of(vars),
        list(
          n        = ~ sum(!is.na(.)),
          mean     = ~ mean(., na.rm = TRUE),
          sd       = ~ sd(., na.rm = TRUE),
          min      = ~ min(., na.rm = TRUE),
          p25      = ~ quantile(., 0.25, na.rm = TRUE),
          median   = ~ median(., na.rm = TRUE),
          p75      = ~ quantile(., 0.75, na.rm = TRUE),
          max      = ~ max(., na.rm = TRUE),
          skewness = ~ moments::skewness(., na.rm = TRUE),
          kurtosis = ~ moments::kurtosis(., na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      -any_of(group_var), names_to = c("variable", ".value"), names_sep = "__"
    ) %>%
    mutate(
      variable = recode(variable, !!!var_labels),
      across(c(mean, sd, min, p25, median, p75, max, skewness, kurtosis), ~ round(., 2))
    )
}

# b. Frequency table (n, %) for categorical variables, optionally by group.
describe_categorical <- function(data, vars, group_var = NULL) {
  map_df(vars, function(v) {
    d <- data %>%
      filter(!is.na(.data[[v]])) %>%
      mutate(category = as.character(.data[[v]]))

    d <- if (!is.null(group_var)) {
      d %>% count(across(all_of(group_var)), category) %>%
        group_by(across(all_of(group_var)))
    } else {
      d %>% count(category)
    }

    d %>%
      mutate(pct = round(n / sum(n) * 100, 2)) %>%
      ungroup() %>%
      mutate(
        variable = v,
        category = if (v %in% names(category_labels)) category_labels[[v]][category] else category,
        .before = 1
      )
  })
}


## 4. General descriptive statistics

general_continuous  <- describe_continuous(geih_clean, continuous_vars)
general_categorical <- describe_categorical(geih_clean, categorical_vars)

general_continuous
general_categorical


## 5. Descriptive statistics by group

# a. By sex
by_sex_continuous  <- describe_continuous(geih_clean, continuous_vars, "sex") %>%
  mutate(sex = sex_labels[as.character(sex)], .after = sex)
by_sex_categorical <- describe_categorical(geih_clean, c("maxEducLevel", "formal"), "sex") %>%
  mutate(sex = sex_labels[as.character(sex)])

by_sex_continuous
by_sex_categorical

# b. By age range
by_age_continuous  <- describe_continuous(
  geih_clean, setdiff(continuous_vars, "age"), "age_group"
)
by_age_categorical <- describe_categorical(
  geih_clean, c("sex", "maxEducLevel", "formal"), "age_group"
)

by_age_continuous
by_age_categorical

# c. By formality
by_formal_continuous  <- describe_continuous(geih_clean, continuous_vars, "formal") %>%
  mutate(formal = formal_labels[as.character(formal)], .after = formal)
by_formal_categorical <- describe_categorical(geih_clean, c("sex", "maxEducLevel"), "formal") %>%
  mutate(formal = formal_labels[as.character(formal)])

by_formal_continuous
by_formal_categorical


## 6. Income distribution histograms by group

# Overlays each group's log-income distribution (semi-transparent) on top of
# the original/overall sample distribution ("Total"), with a dashed vertical
# line at each group's mean.
plot_income_dist <- function(data, group_var, group_labels = NULL, title, filename) {
  d <- data %>%
    filter(!is.na(y_total_m), !is.na(.data[[group_var]])) %>%
    mutate(log_income = log(y_total_m))

  d <- if (!is.null(group_labels)) {
    d %>% mutate(group = unname(group_labels[as.character(.data[[group_var]])]))
  } else {
    d %>% mutate(group = as.character(.data[[group_var]]))
  }

  group_levels <- c("Total", sort(unique(d$group)))
  plot_data <- bind_rows(d, mutate(d, group = "Total")) %>%
    mutate(group = factor(group, levels = group_levels))

  group_means <- plot_data %>%
    group_by(group) %>%
    summarise(mean_log = mean(log_income, na.rm = TRUE), .groups = "drop")

  palette <- c("Total" = "grey40", setNames(
    scales::hue_pal()(length(group_levels) - 1), setdiff(group_levels, "Total")
  ))

  p <- ggplot(plot_data, aes(x = log_income, fill = group)) +
    geom_histogram(
      aes(y = after_stat(density)), position = "identity",
      alpha = 0.45, binwidth = 0.25, color = NA
    ) +
    geom_vline(
      data = group_means, aes(xintercept = mean_log, color = group),
      linetype = "dashed", linewidth = 0.8
    ) +
    scale_fill_manual(name = "Group", values = palette) +
    scale_color_manual(name = "Group", values = palette) +
    labs(
      title = title, x = "Log total monthly income", y = "Density"
    ) +
    theme_minimal()

  ggsave(filename, p, width = 8, height = 5)
  p
}

income_by_sex <- plot_income_dist(
  geih_clean, "sex", sex_labels,
  title = "Income distribution by sex",
  filename = "output/figures/income_dist_sex.png"
)

income_by_age <- plot_income_dist(
  geih_clean, "age_group", NULL,
  title = "Income distribution by age range",
  filename = "output/figures/income_dist_age.png"
)

income_by_formal <- plot_income_dist(
  geih_clean, "formal", formal_labels,
  title = "Income distribution by formality",
  filename = "output/figures/income_dist_formal.png"
)

income_by_sex
income_by_age
income_by_formal


## 7. Income vs. age scatter plots by group

# Scatter of log-income against age, colored by group, with a fitted straight
# line (OLS, no intercept restrictions) run separately for each group.
plot_income_age_scatter <- function(data, group_var, group_labels = NULL, title, filename) {
  d <- data %>%
    filter(!is.na(y_total_m), !is.na(age), !is.na(.data[[group_var]])) %>%
    mutate(log_income = log(y_total_m))

  d <- if (!is.null(group_labels)) {
    d %>% mutate(group = unname(group_labels[as.character(.data[[group_var]])]))
  } else {
    d %>% mutate(group = as.character(.data[[group_var]]))
  }

  group_levels <- sort(unique(d$group))
  d <- d %>% mutate(group = factor(group, levels = group_levels))

  palette <- setNames(scales::hue_pal()(length(group_levels)), group_levels)

  p <- ggplot(d, aes(x = age, y = log_income, color = group)) +
    geom_point(alpha = 0.15, size = 0.8) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    scale_color_manual(name = "Group", values = palette) +
    labs(title = title, x = "Age", y = "Log total monthly income") +
    theme_minimal()

  ggsave(filename, p, width = 8, height = 5)
  p
}

scatter_income_age_sex <- plot_income_age_scatter(
  geih_clean, "sex", sex_labels,
  title = "Income vs. age by sex",
  filename = "output/figures/scatter_income_age_sex.png"
)

scatter_income_age_educ <- plot_income_age_scatter(
  geih_clean, "maxEducLevel", educ_labels,
  title = "Income vs. age by education level",
  filename = "output/figures/scatter_income_age_educ.png"
)

scatter_income_age_formal <- plot_income_age_scatter(
  geih_clean, "formal", formal_labels,
  title = "Income vs. age by formality",
  filename = "output/figures/scatter_income_age_formal.png"
)

scatter_income_age_sex
scatter_income_age_educ
scatter_income_age_formal


## 8. Export tables to LaTeX

# Nicer headers for the LaTeX tables (falls back to the original name for
# any column not listed here).
header_map <- c(
  sex          = "Sex",
  age_group    = "Age range",
  formal       = "Formality",
  variable     = "Variable",
  category     = "Category",
  n            = "N",
  mean         = "Mean",
  sd           = "SD",
  min          = "Min",
  p25          = "P25",
  median       = "Median",
  p75          = "P75",
  max          = "Max",
  skewness     = "Skewness",
  kurtosis     = "Kurtosis",
  pct          = "%"
)

export_table_tex <- function(tbl, filename, caption, label) {
  col_names <- unname(ifelse(names(tbl) %in% names(header_map),
    header_map[names(tbl)], names(tbl)
  ))
  is_long <- nrow(tbl) > 20

  # scale_down and longtable are mutually exclusive in kableExtra.
  latex_options <- if (is_long) {
    c("hold_position", "repeat_header")
  } else {
    c("hold_position", "scale_down")
  }

  tbl_tex <- tbl %>%
    kbl(
      format = "latex", booktabs = TRUE, digits = 2, col.names = col_names,
      caption = caption, label = label, longtable = is_long
    ) %>%
    kable_styling(latex_options = latex_options)

  writeLines(as.character(tbl_tex), filename)
}

export_table_tex(
  general_continuous, "output/tables/general_continuous.tex",
  "General descriptive statistics: continuous variables", "general-continuous"
)
export_table_tex(
  general_categorical, "output/tables/general_categorical.tex",
  "General descriptive statistics: categorical variables", "general-categorical"
)

export_table_tex(
  by_sex_continuous, "output/tables/by_sex_continuous.tex",
  "Descriptive statistics by sex: continuous variables", "by-sex-continuous"
)
export_table_tex(
  by_sex_categorical, "output/tables/by_sex_categorical.tex",
  "Descriptive statistics by sex: categorical variables", "by-sex-categorical"
)

export_table_tex(
  by_age_continuous, "output/tables/by_age_continuous.tex",
  "Descriptive statistics by age range: continuous variables", "by-age-continuous"
)
export_table_tex(
  by_age_categorical, "output/tables/by_age_categorical.tex",
  "Descriptive statistics by age range: categorical variables", "by-age-categorical"
)

export_table_tex(
  by_formal_continuous, "output/tables/by_formal_continuous.tex",
  "Descriptive statistics by formality: continuous variables", "by-formal-continuous"
)
export_table_tex(
  by_formal_categorical, "output/tables/by_formal_categorical.tex",
  "Descriptive statistics by formality: categorical variables", "by-formal-categorical"
)

################################End of script###################################

