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
  tidyverse,  # Manipulación de datos y gráficos.
  scales,     # Paletas de color y formato de ejes.
  kableExtra, # Export tables to LaTeX.
  srvyr       # Survey design and analysis (weighted descriptives via fex_c).
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

# GEIH is a complex survey, not a simple random sample: fex_c (the person-
# level expansion factor) says how many population individuals each
# respondent represents. All descriptive tables below are computed on this
# survey design so they're representative of the population, not just this
# sample; the histograms and scatter plots in sections 6-7 use fex_c
# directly as a plotting weight for the same reason.
geih_svy <- geih_clean %>% as_survey_design(weights = fex_c)


## 3. Helper functions for descriptive tables

# Weighted (via fex_c) skewness/kurtosis: moments::skewness()/kurtosis()
# don't take weights, so these reimplement them (moment-based, i.e.
# kurtosis == 3 for a normal distribution) on weighted central moments.
weighted_skewness <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]; w <- w[keep]
  }
  mu <- sum(w * x) / sum(w)
  m2 <- sum(w * (x - mu)^2) / sum(w)
  m3 <- sum(w * (x - mu)^3) / sum(w)
  m3 / m2^1.5
}

weighted_kurtosis <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]; w <- w[keep]
  }
  mu <- sum(w * x) / sum(w)
  m2 <- sum(w * (x - mu)^2) / sum(w)
  m4 <- sum(w * (x - mu)^4) / sum(w)
  m4 / m2^2
}

# a. Weighted classic descriptive statistics for continuous variables,
# optionally by group (design must be a tbl_svy, e.g. geih_svy). n, min and
# max are reported unweighted (sample size and range aren't population
# quantities); mean, sd, quantiles, skewness and kurtosis are weighted by
# fex_c so they're representative of the population. Skewness/(non-excess)
# kurtosis -- kurtosis == 3 corresponds to a normal distribution -- are
# included only when moments = TRUE (used for the by-group tables in
# section 5, not the general table in section 4).
describe_continuous <- function(design, vars, group_var = NULL, moments = TRUE) {
  round_vars <- c("mean", "sd", "min", "p25", "median", "p75", "max")
  if (moments) round_vars <- c(round_vars, "skewness", "kurtosis")

  # The .fns list for across() has to be written literally inline here
  # rather than built in a `stat_funs <- list(...)` variable beforehand:
  # srvyr's summarise() re-quotes the summarise() call before evaluating it
  # against the design's underlying data, and a custom function referencing
  # a bare column symbol (fex_c, for the weighted moments below) inside a
  # formula stored in a variable loses access to that column ("object
  # 'fex_c' not found") -- survey_mean()/survey_sd()/survey_quantile()/
  # unweighted() are unaffected since srvyr handles those specially.
  design %>%
    { if (!is.null(group_var)) group_by(., across(all_of(group_var))) else . } %>%
    summarise(
      across(all_of(vars), c(
        list(
          n      = ~ unweighted(sum(!is.na(.x))),
          mean   = ~ survey_mean(.x, na.rm = TRUE, vartype = NULL),
          sd     = ~ survey_sd(.x, na.rm = TRUE),
          p25    = ~ unname(survey_quantile(.x, 0.25, na.rm = TRUE, vartype = NULL)[[1]]),
          median = ~ survey_median(.x, na.rm = TRUE, vartype = NULL),
          p75    = ~ unname(survey_quantile(.x, 0.75, na.rm = TRUE, vartype = NULL)[[1]]),
          min    = ~ unweighted(min(.x, na.rm = TRUE)),
          max    = ~ unweighted(max(.x, na.rm = TRUE))
        ),
        if (moments) list(
          skewness = ~ weighted_skewness(.x, fex_c),
          kurtosis = ~ weighted_kurtosis(.x, fex_c)
        )
      ), .names = "{.col}__{.fn}"),
      .groups = "drop"
    ) %>%
    pivot_longer(
      -any_of(group_var), names_to = c("variable", ".value"), names_sep = "__"
    ) %>%
    mutate(
      variable = recode(variable, !!!var_labels),
      across(all_of(round_vars), ~ round(., 2))
    )
}

# b. Weighted frequency table for categorical variables, optionally by
# group (design must be a tbl_svy). n is the weighted population-total
# estimate for the category (via fex_c, rounded to a whole number) and pct
# its share of the weighted group total.
describe_categorical <- function(design, vars, group_var = NULL) {
  map_df(vars, function(v) {
    d <- design %>% filter(!is.na(.data[[v]]))

    d <- if (!is.null(group_var)) {
      d %>% group_by(across(all_of(group_var)), category = as.character(.data[[v]]))
    } else {
      d %>% group_by(category = as.character(.data[[v]]))
    }

    d %>%
      summarise(n = survey_total(vartype = NULL), .groups = "drop_last") %>%
      mutate(pct = round(n / sum(n) * 100, 2), n = round(n)) %>%
      ungroup() %>%
      mutate(
        variable = v,
        category = if (v %in% names(category_labels)) category_labels[[v]][category] else category,
        .before = 1
      )
  })
}


## 4. General descriptive statistics

general_continuous  <- describe_continuous(geih_svy, continuous_vars, moments = FALSE)
general_categorical <- describe_categorical(geih_svy, categorical_vars)

general_continuous
general_categorical


## 5. Descriptive statistics by group

# The outcome variable (y_total_m) is the only continuous variable compared
# across groups here, together with skewness/kurtosis (left out of the
# general table above); the categorical comparison is restricted to
# formality (informal/formal).

# a. By sex
by_sex_continuous  <- describe_continuous(geih_svy, "y_total_m", "sex") %>%
  mutate(sex = sex_labels[as.character(sex)], .after = sex)
by_sex_categorical <- describe_categorical(geih_svy, "formal", "sex") %>%
  mutate(sex = sex_labels[as.character(sex)])

by_sex_continuous
by_sex_categorical

# b. By age range
by_age_continuous  <- describe_continuous(geih_svy, "y_total_m", "age_group")
by_age_categorical <- describe_categorical(geih_svy, c("sex", "formal"), "age_group")

by_age_continuous
by_age_categorical

# c. By formality
by_formal_continuous  <- describe_continuous(geih_svy, "y_total_m", "formal") %>%
  mutate(formal = formal_labels[as.character(formal)], .after = formal)
by_formal_categorical <- describe_categorical(geih_svy, "sex", "formal") %>%
  mutate(formal = formal_labels[as.character(formal)])

by_formal_continuous
by_formal_categorical


## 6. Income distribution histograms by group

# Overlays each group's log-income distribution (semi-transparent) on top of
# the original/overall sample distribution ("Total"), with a dashed vertical
# line at each group's mean. Both the histogram (via the weight aesthetic)
# and the means are weighted by fex_c, consistent with the tables above.
# The x-axis is zoomed to the 0.1%-99.9% quantile range of log-income: with
# the full range (log ~4 to ~18), a handful of extreme outliers stretch the
# axis and flatten the histogram, hiding the shape of the bulk of the
# distribution. Bins are still computed on the full data (via
# coord_cartesian, not scale limits), so this only zooms the view -- it
# doesn't drop observations or distort the density.
income_xlim <- quantile(
  log(geih_clean$y_total_m), probs = c(0.001, 0.999), na.rm = TRUE
)

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
    summarise(
      mean_log = weighted.mean(log_income, w = fex_c, na.rm = TRUE), .groups = "drop"
    )

  palette <- c("Total" = "grey40", setNames(
    scales::hue_pal()(length(group_levels) - 1), setdiff(group_levels, "Total")
  ))

  p <- ggplot(plot_data, aes(x = log_income, fill = group)) +
    geom_histogram(
      aes(y = after_stat(density), weight = fex_c), position = "identity",
      alpha = 0.45, binwidth = 0.15, color = NA
    ) +
    geom_vline(
      data = group_means, aes(xintercept = mean_log, color = group),
      linetype = "dashed", linewidth = 0.8
    ) +
    coord_cartesian(xlim = income_xlim) +
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
# line (weighted least squares, via fex_c as the regression weight) run
# separately for each group.
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
    geom_smooth(aes(weight = fex_c), method = "lm", se = FALSE, linewidth = 1) +
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
    kable_styling(latex_options = latex_options) %>%
    as.character()

  # kableExtra's hold_position only sets [!h] (a placement hint), which lets
  # LaTeX float these small tables past their own section into wherever the
  # next figures (forced to [H]) land, interleaving sections in the output.
  # Force [H] (requires \usepackage{float} in the including .tex) so every
  # table renders exactly where it appears in the source.
  tbl_tex <- sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", tbl_tex)

  writeLines(tbl_tex, filename)
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

