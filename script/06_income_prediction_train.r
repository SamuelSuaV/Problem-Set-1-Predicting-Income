###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    06_income_prediction_train.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Reestimar los cinco modelos de 04_age_labor_income.r y
#                       05_gender_gap.r solo sobre el split de entrenamiento de
#                       la Sección 3 (subset 1-7), y comparar sus coeficientes
#                       (con SE robustos a heterocedasticidad, HC1, en línea con
#                       el vcov = "hetero" que 04/05 adoptaron) contra las
#                       versiones de muestra completa. Los modelos de
#                       entrenamiento ajustados se guardan para la predicción
#                       fuera de muestra sobre el subset 8-10 en un script
#                       posterior.
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Cargar datos y construir las variables del modelo
# 3. Split entrenamiento / muestra completa
# 4. Reestimar los cinco modelos en el split de entrenamiento y en la muestra completa
# 5. Guardar los modelos de entrenamiento ajustados para la predicción fuera de muestra
# 6. Tabla de comparación de coeficientes muestra completa vs. solo entrenamiento

################################################################################

# Input:  Dataframe de análisis limpio de 02_data_cleaning.r
# Output: Modelos de entrenamiento ajustados (output/models/section3_train_models.rds) y
#         una tabla de comparación de coeficientes muestra completa vs. entrenamiento
#         (output/tables/section3_train_vs_full.tex)

################################################################################


## 1. Cargar librerías

library(pacman)
p_load(
    tidyverse,
    broom,
    kableExtra,
    sandwich)


## 2. Cargar datos y construir las variables del modelo

# Misma muestra de análisis y variables derivadas que 04_age_labor_income.r y
# 05_gender_gap.r: adultos ocupados (ya impuesto en 02_data_cleaning.r) con
# ingreso laboral estrictamente positivo, porque log(y_total_m) no está definido
# en 0 y es NA para el ingreso faltante.
clean_data <- readRDS("data/geih_clean.rds") |>
    filter(y_total_m > 0) |>
    mutate(
        age2    = age^2,
        log_inc = log(y_total_m),
        female  = as.numeric(sex == 0)
    )


## 3. Split entrenamiento / muestra completa

# La Sección 3 del problem set valida sobre chunks scrapeados que el modelo nunca
# vio: el subset 1-7 es el split de entrenamiento, el subset 8-10 se reserva para
# la validación fuera de muestra en un script posterior. Cada modelo de abajo se
# ajusta dos veces - sobre train_data y sobre el clean_data completo - para que
# podamos ver cuánto se mueve cada coeficiente una vez que los chunks de
# validación (~30% de la muestra) se retiran de la estimación.
train_data <- clean_data |> filter(subset %in% 1:7)


## 4. Reestimar los cinco modelos en el split de entrenamiento y en la muestra completa

# Las mismas cinco especificaciones que 04_age_labor_income.r (model1, model2) y
# 05_gender_gap.r (gap_uncond, gap_hk, gap_hours), ponderadas por fex_c por la
# misma razón dada allá: la GEIH es una encuesta compleja, no una muestra
# aleatoria simple.

# a. Perfil edad-ingreso (04_age_labor_income.r)
model1_train <- lm(log_inc ~ age + age2, data = train_data, weights = fex_c)
model1_full  <- lm(log_inc ~ age + age2, data = clean_data, weights = fex_c)

model2_train <- lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                    data = train_data, weights = fex_c)
model2_full  <- lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                    data = clean_data, weights = fex_c)

# b. Brecha de género (05_gender_gap.r)
gap_uncond_train <- lm(log_inc ~ female, data = train_data, weights = fex_c)
gap_uncond_full  <- lm(log_inc ~ female, data = clean_data, weights = fex_c)

gap_hk_train <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                    data = train_data, weights = fex_c)
gap_hk_full  <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                    data = clean_data, weights = fex_c)

gap_hours_train <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                         totalHoursWorked,
                       data = train_data, weights = fex_c)
gap_hours_full  <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                         totalHoursWorked,
                       data = clean_data, weights = fex_c)

# c. Chequeo rápido de que nada degeneró cuando se descartó el 30% de la muestra
# (p. ej. una categoría de relab o maxEducLevel con muy pocas observaciones de
# entrenamiento).
summary(model1_train)
summary(model2_train)
summary(gap_uncond_train)
summary(gap_hk_train)
summary(gap_hours_train)


## 5. Guardar los modelos de entrenamiento ajustados para la predicción fuera de muestra

# Una lista con nombres, no cinco objetos sueltos, para que un script de
# validación posterior pueda recorrerla (predict() cada modelo contra los datos
# del subset 8-10) en vez de hardcodear cinco llamadas separadas.
train_models <- list(
    model1     = model1_train,
    model2     = model2_train,
    gap_uncond = gap_uncond_train,
    gap_hk     = gap_hk_train,
    gap_hours  = gap_hours_train
)

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
saveRDS(train_models, "output/models/section3_train_models.rds")


## 6. Tabla de comparación de coeficientes muestra completa vs. solo entrenamiento

# a. Una fila por coeficiente, un par de columnas por muestra (train vs. full),
# agrupada por modelo para que las dos familias de modelos no se mezclen.
#
# Los SE son robustos a heterocedasticidad (HC1), para quedar en el mismo
# estándar que 04_age_labor_income.r y 05_gender_gap.r adoptaron (feols con
# vcov = "hetero"). Se extraen con sandwich::vcovHC() sobre los mismos objetos
# lm() en vez de reajustar con feols(): los coeficientes/residuos no cambian
# con el tipo de SE, así que esto da la misma tabla que reajustar con feols
# obtendría, pero conserva la clase lm() de train_models - de la que
# 07_income_prediction_validation.r depende para hatvalues() en su atajo de
# LOOCV vía leverage, y que fixest no garantiza soportar igual.
compare_model <- function(model_train, model_full) {
    full_join(
        tidy(model_train, vcov. = vcovHC(model_train, type = "HC1")) |>
            select(term, estimate, std.error),
        tidy(model_full,  vcov. = vcovHC(model_full,  type = "HC1")) |>
            select(term, estimate, std.error),
        by = "term",
        suffix = c("_train", "_full")
    )
}

age_models <- bind_rows(
    compare_model(model1_train, model1_full) |> mutate(model = "Unconditional"),
    compare_model(model2_train, model2_full) |> mutate(model = "Conditional")
)

gender_models <- bind_rows(
    compare_model(gap_uncond_train, gap_uncond_full) |> mutate(model = "Unconditional"),
    compare_model(gap_hk_train, gap_hk_full) |> mutate(model = "HK controls"),
    compare_model(gap_hours_train, gap_hours_full) |> mutate(model = "HK + hours")
)

comparison_table <- bind_rows(
    age_models    |> mutate(family = "Age-income profile (04)"),
    gender_models |> mutate(family = "Gender gap (05)")
) |>
    relocate(family, model, term)

view(comparison_table)

# b. Exportar a LaTeX, mismo arreglo [!h] -> [H] que 02_data_cleaning.r y
# 04_age_labor_income.r, para que la tabla mantenga su lugar en el documento
# compilado (requiere \usepackage{float}).
force_float_h <- function(x) {
    sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

comparison_table_tex <- comparison_table |>
    rename(
        Family = family,
        Model = model,
        Term = term,
        `Train (1-7)` = estimate_train,
        `Robust SE (train)` = std.error_train,
        `Full sample` = estimate_full,
        `Robust SE (full)` = std.error_full
    ) |>
    kbl(
        format = "latex", booktabs = TRUE, digits = 4,
        caption = "Section 3 training-split coefficients vs. full-sample coefficients (HC1-robust SEs)",
        label = "section3_train_vs_full"
    ) |>
    kable_styling(latex_options = c("hold_position", "scale_down")) |>
    collapse_rows(columns = 1:2, latex_hline = "major") |>
    as.character() |>
    force_float_h()

writeLines(comparison_table_tex, "output/tables/section3_train_vs_full.tex")
