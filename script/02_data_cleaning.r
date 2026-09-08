###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    02_data_cleaning.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Limpiar la muestra raspada de la GEIH 2018: conservar
#                       adultos ocupados, seleccionar las variables de interés
#                       y manejar los valores faltantes.
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Cargar datos
# 3. Filtrar y seleccionar variables
# 4. Manejar valores faltantes
# 6. Manejar outliers
# 7. Guardar datos limpios

################################################################################

# Input:  Dataframe crudo raspado por 01_web_scrapping.r
# Output: Dataframe de análisis limpio

################################################################################


## 1. Cargar librerías

library(pacman)
p_load(
  tidyverse,  # Manipulación de datos.
  srvyr,      # Diseño y análisis de encuestas.
  broom,      # Salida ordenada de pruebas estadísticas.
  kableExtra  # Exportar tablas a LaTeX.
)


## 2. Cargar datos
geih_scrap <- readRDS("data/geih_scrap.rds")

## 3. Filtro inicial, renombrado y selección

# Restricted_sample: solo adultos ocupados (age >= 18 y estado de empleo = 1).
# Quitamos variables innecesarias.
# También descartamos al único encuestado con maxEducLevel faltante: la educación
# es un control en toda especificación condicional aguas abajo, así que dejar la
# fila haría que los modelos incondicionales corrieran sobre una observación más
# que los condicionales. El valor falta porque esa persona respondió
# p6210 = 9 ("no sabe, no informa"), es decir una no-respuesta genuina, y es
# 1 fila de 16,542.
geih_clean <- geih_scrap %>%
  filter(age >= 18, ocu == 1, !is.na(maxEducLevel)) %>%
  select(-fweight, -fex_dpto, -depto, -clase)


## 4. Manejar valores faltantes

# Caracterización de los faltantes
# a. Porcentaje de valores faltantes por variable
missing_summary <- geih_clean %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

missing_summary

# Eliminar columnas con 100% de valores faltantes
cols_to_remove <- names(missing_summary)[missing_summary == 100]
geih_clean <- geih_clean %>% select(-all_of(cols_to_remove))

# Las columnas eliminadas son:
# p550 - Ingreso neto de la cosecha en los últimos 12 meses
# p7301 - Trabajó o buscó trabajo antes
# p7350 - En su último trabajo usted era (desempleado)
# p7422 - Ingreso obtenido del trabajo en el último mes (para desempleados)?
# p7422s1 - Cuánto (respondiendo a p7422)
# y_gananciaNetaAgro_m - Ingreso neto de actividades agrícolas en los últimos 12 meses

# Estas columnas habrían sido irrelevantes para nuestro análisis incluso sin faltantes

# Ahora extraemos los faltantes en variables de especial interés
vars_of_interest <- c(
  "age", "maxEducLevel", "y_total_m", "sex"
)

missing_vars <- missing_summary[vars_of_interest]

# b. Separar las filas con vs. sin un y_total_m faltante
geih_clean <- geih_clean %>%
  mutate(missing_income = factor(
    if_else(is.na(y_total_m), "Missing", "Non-missing"),
    levels = c("Non-missing", "Missing")
  ))

geih_missing    <- geih_clean %>% filter(missing_income == "Missing")
geih_nonmissing <- geih_clean %>% filter(missing_income == "Non-missing")

# c. Estadísticos de resumen para age, por grupo
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

# d. Desgloses porcentuales por grupo (sexo, nivel educativo)
pct_by_group <- function(data, var) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    count(missing_income, .data[[var]]) %>%
    group_by(missing_income) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()
}

sex_pct  <- pct_by_group(geih_clean, "sex")          # Distribución por sexo
educ_pct <- pct_by_group(geih_clean, "maxEducLevel") # Nivel educativo

sex_pct
educ_pct

# e. Prueba t que compara los grupos con vs. sin faltante
# Para age comparamos medias directamente; para las variables categóricas
# comparamos, para cada categoría, la proporción del grupo que cae en ella (una
# prueba t sobre una dummy), que equivale a una prueba de proporciones de dos
# muestras.
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
  ttest_categorical(geih_clean, "formal")
) %>%
  rename(
    mean_nonmissing = estimate1, mean_missing = estimate2, diff = estimate
  ) %>%
  mutate(across(
    c(mean_nonmissing, mean_missing, diff, statistic, p.value), ~ round(., 4)
  ))

# f. Tabla final: medias/porcentajes de cada grupo, su diferencia y el
# p-valor de la prueba t (la fila "age" reporta medias en años; todas las demás
# filas reportan proporciones, es decir participaciones de categoría en escala
# 0-1).
balance_table

# g. Etiquetar los niveles de sexo y educación, y colapsar variable/categoría en
# una sola columna identificadora (el nombre de la variable cuando no tiene
# categorías, p. ej. "age"; el nombre de su categoría en caso contrario, p. ej.
# "Female"). Codificación de sexo inferida de ocupaciones sesgadas por género
# (oficio): las empleadas domésticas (54) son casi todas sex == 0, los
# conductores (98) casi todos sex == 1.
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

balance_table <- balance_table %>%
  mutate(variable = case_when(
    variable == "age"          ~ "Age",
    variable == "sex"          ~ sex_labels[category],
    variable == "maxEducLevel" ~ educ_labels[category],
    variable == "formal"       ~ formal_labels[category],
    TRUE                       ~ category
  )) %>%
  select(-category)

balance_table

# h. Exportar la balance table a LaTeX
# force_float_h cambia la pista [!h] de kableExtra por un [H] duro (requiere
# \usepackage{float} en el .tex que la incluye), para que una tabla no pueda
# flotar más allá de su propia sección hacia las siguientes.
force_float_h <- function(x) {
  sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

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
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  as.character() %>%
  force_float_h()

writeLines(balance_table_tex, "output/tables/balance_table.tex")

# i. Caracterizando el grupo de ingreso faltante por relación laboral
# Con la muestra restringida a adultos ocupados (ocu == 1), el y_total_m faltante
# se concentra entre trabajadores sin un salario fijo: los trabajadores
# familiares / de otro hogar sin remuneración (relab 6-7) casi nunca están en el
# grupo no-faltante, y los trabajadores por cuenta propia, empleadores e
# informales están todos sobrerrepresentados entre los faltantes (ver
# relab_pct/formal_pct abajo).


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

# Etiquetas en inglés para las tablas del documento (latex/missing_income.tex está en inglés).
relab_labels_en <- c(
  "1" = "Private employee",
  "2" = "Government employee",
  "3" = "Domestic worker",
  "4" = "Self-employed",
  "5" = "Employer / business owner",
  "6" = "Unpaid family worker",
  "7" = "Unpaid worker, another household's business",
  "8" = "Day laborer",
  "9" = "Other"
)

##### mirar si tambien se actualiza la tabla de porcentajes
relab_pct  <- pct_by_group(geih_clean, "relab")
# Pendiente confirmar con el equipo si aplicamos las etiquetas de texto aquí:
# relab_pct <- pct_by_group(geih_clean, "relab") |>
#   mutate(relab = relab_labels[as.character(relab)])
formal_pct <- pct_by_group(geih_clean, "formal")

relab_pct
formal_pct

# j. Tasa de ingreso faltante por relación laboral
# relab_pct (arriba) da la *composición* del grupo faltante; esta tabla lo
# reformula como una tasa: la proporción de *cada* categoría de relab que a su
# vez tiene y_total_m faltante. Sin ponderar, igual que balance_table. Alimenta
# tab:relab-missing-rate en el documento "Characterizing Missing Income"
# (latex/missing_income.tex).
relab_missing_rate <- geih_clean %>%
  filter(!is.na(relab)) %>%
  group_by(relab) %>%
  summarise(
    n            = n(),
    n_missing    = sum(is.na(y_total_m)),
    missing_rate = mean(is.na(y_total_m)) * 100,
    .groups = "drop"
  ) %>%
  mutate(relab = relab_labels_en[as.character(relab)])

relab_missing_rate

relab_missing_rate_tex <- relab_missing_rate %>%
  rename(
    `Employment relationship` = relab,
    N = n,
    `N missing` = n_missing,
    `Missing (\\%)` = missing_rate
  ) %>%
  kbl(
    format = "latex", booktabs = TRUE, digits = 1, escape = FALSE,
    caption = "Missing-income rate by employment relationship",
    label = "relab-missing-rate"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  as.character() %>%
  force_float_h()

writeLines(relab_missing_rate_tex, "output/tables/relab_missing_rate.tex")

## 7. Guardar datos limpios

# geih_clean se exporta con sus valores faltantes (p. ej. y_total_m) intactos,
# ni descartados ni imputados: cómo tratarlos es una decisión de análisis que
# cada sección justifica por su cuenta. La única excepción es la no-respuesta de
# maxEducLevel descartada en la sección 3.
saveRDS(geih_clean, file = "data/geih_clean.rds")

##############################Fin del script####################################
