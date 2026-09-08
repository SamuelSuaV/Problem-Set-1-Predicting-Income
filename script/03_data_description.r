###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    03_data_description.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Estadística descriptiva y gráficos exploratorios para la
#                       muestra de análisis limpia de la GEIH 2018.
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Cargar datos y construir etiquetas/grupos
# 3. Funciones auxiliares para las tablas descriptivas
# 4. Estadística descriptiva general
# 5. Estadística descriptiva por grupo (sexo, rango de edad, formalidad)
# 6. Histogramas de la distribución del ingreso por grupo
# 7. Diagramas de dispersión ingreso vs. edad por grupo
# 8. Exportar tablas a LaTeX

################################################################################

# Input:  Dataframe de análisis limpio de 02_data_cleaning.r
# Output: Tablas de estadísticos de resumen (output/tables/*.tex) y figuras de
#         la distribución del ingreso (output/figures/*.png)

################################################################################


## 1. Cargar librerías

library(pacman)

p_load(
  tidyverse,  # Manipulación de datos y gráficos.
  scales,     # Paletas de color y formato de ejes.
  kableExtra, # Exportar tablas a LaTeX.
  srvyr       # Diseño y análisis de encuestas (descriptivos ponderados vía fex_c).
)


## 2. Cargar datos y construir etiquetas/grupos

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

# Etiquetas reutilizadas de 02_data_cleaning.r (mismo codebook/criterios de codificación).
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

# Cuatro rangos de edad basados en los cuartiles de la muestra, para que los
# grupos sean comparables en tamaño (18-28, 29-38, 39-50, 51-94).
age_breaks <- c(18, 28, 38, 50, 94)
age_group_labels <- c("18-28", "29-38", "39-50", "51-94")

geih_clean <- geih_clean %>%
  mutate(
    age_group = cut(
      age,
      breaks = age_breaks, labels = age_group_labels, include.lowest = TRUE
    )
  )

# La GEIH es una encuesta compleja, no una muestra aleatoria simple: fex_c (el
# factor de expansión a nivel de persona) dice a cuántos individuos de la
# población representa cada encuestado. Todas las tablas descriptivas de abajo se
# calculan sobre este diseño de encuesta para que sean representativas de la
# población, no solo de esta muestra; los histogramas y los diagramas de
# dispersión de las secciones 6-7 usan fex_c directamente como peso de graficación
# por la misma razón.
geih_svy <- geih_clean %>% as_survey_design(weights = fex_c)


## 3. Funciones auxiliares para las tablas descriptivas

# Asimetría/curtosis ponderadas (vía fex_c): moments::skewness()/kurtosis() no
# aceptan pesos, así que estas las reimplementan (basadas en momentos, es decir
# curtosis == 3 para una distribución normal) sobre momentos centrales
# ponderados.
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

# a. Estadísticos descriptivos clásicos ponderados para variables continuas,
# opcionalmente por grupo (design debe ser un tbl_svy, p. ej. geih_svy). n, min y
# max se reportan sin ponderar (el tamaño de muestra y el rango no son cantidades
# poblacionales); media, sd, cuantiles, asimetría y curtosis se ponderan por
# fex_c para que sean representativos de la población. La asimetría/curtosis (no
# en exceso) -- curtosis == 3 corresponde a una distribución normal -- se
# incluyen solo cuando moments = TRUE (usado para las tablas por grupo de la
# sección 5, no para la tabla general de la sección 4).
describe_continuous <- function(design, vars, group_var = NULL, moments = TRUE) {
  round_vars <- c("mean", "sd", "min", "p25", "median", "p75", "max")
  if (moments) round_vars <- c(round_vars, "skewness", "kurtosis")

  # La lista .fns para across() tiene que escribirse literalmente inline aquí en
  # vez de construirse antes en una variable `stat_funs <- list(...)`: el
  # summarise() de srvyr re-cita la llamada a summarise() antes de evaluarla
  # contra los datos subyacentes del diseño, y una función custom que referencia
  # un símbolo de columna pelado (fex_c, para los momentos ponderados de abajo)
  # dentro de una fórmula guardada en una variable pierde el acceso a esa columna
  # ("object 'fex_c' not found") -- survey_mean()/survey_sd()/survey_quantile()/
  # unweighted() no se ven afectados porque srvyr los maneja de forma especial.
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

# b. Tabla de frecuencias ponderada para variables categóricas, opcionalmente por
# grupo (design debe ser un tbl_svy). n es el total poblacional estimado y
# ponderado para la categoría (vía fex_c, redondeado a un entero) y pct su
# participación en el total ponderado del grupo.
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


## 4. Estadística descriptiva general

general_continuous  <- describe_continuous(geih_svy, continuous_vars, moments = FALSE)
general_categorical <- describe_categorical(geih_svy, categorical_vars)

general_continuous
general_categorical


## 5. Estadística descriptiva por grupo

# La variable de resultado (y_total_m) es la única variable continua comparada
# entre grupos aquí, junto con asimetría/curtosis (dejadas fuera de la tabla
# general de arriba); la comparación categórica se restringe a la formalidad
# (informal/formal).

# a. Por sexo
by_sex_continuous  <- describe_continuous(geih_svy, "y_total_m", "sex") %>%
  mutate(sex = sex_labels[as.character(sex)], .after = sex)
by_sex_categorical <- describe_categorical(geih_svy, "formal", "sex") %>%
  mutate(sex = sex_labels[as.character(sex)])

by_sex_continuous
by_sex_categorical

# b. Por rango de edad
by_age_continuous  <- describe_continuous(geih_svy, "y_total_m", "age_group")
by_age_categorical <- describe_categorical(geih_svy, c("sex", "formal"), "age_group")

by_age_continuous
by_age_categorical

# c. Por formalidad
by_formal_continuous  <- describe_continuous(geih_svy, "y_total_m", "formal") %>%
  mutate(formal = formal_labels[as.character(formal)], .after = formal)
by_formal_categorical <- describe_categorical(geih_svy, "sex", "formal") %>%
  mutate(formal = formal_labels[as.character(formal)])

by_formal_continuous
by_formal_categorical


## 6. Histogramas de la distribución del ingreso por grupo

# Superpone la distribución del log-ingreso de cada grupo (semitransparente), con
# una línea vertical punteada en la media de cada grupo. Tanto el histograma (vía
# la estética weight) como las medias se ponderan por fex_c, consistente con las
# tablas de arriba.
# El eje x se acerca al rango de cuantiles 0.1%-99.9% del log-ingreso: con el
# rango completo (log ~4 a ~18), un puñado de outliers extremos estira el eje y
# aplana el histograma, escondiendo la forma del grueso de la distribución. Los
# bins se siguen calculando sobre los datos completos (vía coord_cartesian, no
# límites de escala), así que esto solo acerca la vista -- no descarta
# observaciones ni distorsiona la densidad.
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

  group_levels <- sort(unique(d$group))
  plot_data <- d %>% mutate(group = factor(group, levels = group_levels))

  group_means <- plot_data %>%
    group_by(group) %>%
    summarise(
      mean_log = weighted.mean(log_income, w = fex_c, na.rm = TRUE), .groups = "drop"
    )

  palette <- setNames(scales::hue_pal()(length(group_levels)), group_levels)

  p <- ggplot(plot_data, aes(x = log_income, fill = group)) +
    geom_histogram(
      aes(y = after_stat(density), weight = fex_c), position = "identity",
      alpha = 0.45, binwidth = 0.15, color = NA
    ) +
    geom_vline(
      data = group_means, aes(xintercept = mean_log, color = group),
      linetype = "dashed", linewidth = 0.5
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


## 7. Diagramas de dispersión ingreso vs. edad por grupo

# Dispersión del log-ingreso contra la edad, coloreada por grupo, con una recta
# ajustada (mínimos cuadrados ponderados, vía fex_c como peso de la regresión)
# corrida por separado para cada grupo.
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


## 8. Exportar tablas a LaTeX

# Encabezados más bonitos para las tablas LaTeX (vuelve al nombre original para
# cualquier columna no listada aquí).
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

  # scale_down y longtable son mutuamente excluyentes en kableExtra.
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

  # El hold_position de kableExtra solo pone [!h] (una pista de ubicación), lo
  # que deja que LaTeX flote estas tablas pequeñas más allá de su propia sección
  # hacia donde caigan las siguientes figuras (forzadas a [H]), intercalando
  # secciones en la salida. Forzamos [H] (requiere \usepackage{float} en el .tex
  # que la incluye) para que cada tabla se renderice exactamente donde aparece
  # en la fuente.
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

##############################Fin del script####################################

