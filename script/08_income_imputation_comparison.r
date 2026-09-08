###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    08_income_imputation_comparison.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Comparar las diez especificaciones de predicción del
#                       ingreso de la Sección 3 bajo dos regímenes de
#                       entrenamiento - solo casos completos (como en
#                       07_income_prediction_validation.r) vs. un conjunto de
#                       entrenamiento donde el y_total_m faltante se imputa vía
#                       predictive mean matching (PMM) - para medir cuánto afecta
#                       realmente al desempeño de predicción el problema de
#                       selección del ingreso faltante documentado en la balance
#                       table de 02_data_cleaning.r.
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Cargar datos y construir las variables del modelo
# 3. Las diez especificaciones, como función de un data frame de entrenamiento
# 4. Línea base: entrenamiento de casos completos (misma población que 07)
# 5. Imputación PMM del log(y_total_m) faltante entre trabajadores pagos
# 6. Reajustar las diez especificaciones sobre cada uno de los m=5 sets de entrenamiento imputados
# 7. Tabla de comparación: casos completos vs. imputado por PMM, exportar a LaTeX
# 8. Diagnóstico: densidad del log-ingreso observado vs. imputado

################################################################################

# Input:  Dataframe de análisis limpio de 02_data_cleaning.r
# Output: Tabla de comparación (output/tables/section3_imputation_comparison.tex)
#         y figura de diagnóstico
#         (output/figures/section3_imputation_density.png)

################################################################################


## 1. Cargar librerías

library(pacman)
p_load(
    tidyverse,
    broom,
    kableExtra,
    mice)


## 2. Cargar datos y construir las variables del modelo

# Mismas variables derivadas que 07_income_prediction_validation.r. relab 6-7
# (trabajadores familiares / de otro hogar sin remuneración) se descartan de
# entrada: en este split de entrenamiento cada una de las filas de relab 6/7
# tiene ingreso faltante/no positivo (165 de 165, verificado por separado) - su
# "ausencia" no es un problema de datos por corregir, es el valor correcto para
# alguien que por definición no recibe salario monetario, así que se excluyen de
# ambos regímenes de entrenamiento comparados abajo en vez de tratarse como
# imputables.
clean_data <- readRDS("data/geih_clean.rds") |>
    filter(!(relab %in% c(6, 7))) |>
    mutate(
        age2    = age^2,
        female  = as.numeric(sex == 0),
        subsidio_familiar = case_when(
            p6585s3 == 1        ~ "Sí",
            p6585s3 %in% c(2, 9) ~ "No",
            TRUE                 ~ "No aplica"
        ) |> factor(levels = c("No", "Sí", "No aplica")),
        recibe_transferencias = as.numeric(p7505 == 1),
        # log_inc es NA siempre que el ingreso está faltante O es no positivo (el
        # mismo criterio que usa cualquier otro script de este proyecto antes de
        # tomar un log) - exactamente el conjunto que PMM imputa abajo.
        log_inc = if_else(!is.na(y_total_m) & y_total_m > 0, log(y_total_m), NA_real_)
    )

train_all       <- clean_data |> filter(subset %in% 1:7)
validation_data <- clean_data |> filter(subset %in% 8:10, !is.na(log_inc))
train_complete  <- train_all  |> filter(!is.na(log_inc))

cat(sprintf(
    "Train rows: %d complete-case / %d total (%d missing income to impute)\n",
    nrow(train_complete), nrow(train_all), sum(is.na(train_all$log_inc))
))


## 3. Las diez especificaciones, como función de un data frame de entrenamiento

# Fórmulas idénticas a 06_income_prediction_train.r (model1, model2, gap_uncond,
# gap_hk, gap_hours) y 07_income_prediction_validation.r (model3-model7),
# envueltas en una función para poder reajustarlas sobre train_complete y sobre
# cada set de entrenamiento imputado sin duplicar el código cinco veces más.
fit_all_models <- function(data) {
    list(
        model1 = lm(log_inc ~ age + age2,
                    data = data, weights = fex_c),
        model2 = lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                    data = data, weights = fex_c),
        gap_uncond = lm(log_inc ~ female,
                         data = data, weights = fex_c),
        gap_hk = lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                    data = data, weights = fex_c),
        gap_hours = lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                          totalHoursWorked,
                        data = data, weights = fex_c),
        model3 = lm(log_inc ~ age + age2 + college + formal + factor(sizeFirm) +
                      formal:college,
                    data = data, weights = fex_c),
        model4 = lm(log_inc ~ age + age2 + college + factor(sizeFirm) +
                      subsidio_familiar + subsidio_familiar:college,
                    data = data, weights = fex_c),
        model5 = lm(log_inc ~ age + age2 + college + cuentaPropia +
                      cuentaPropia:college,
                    data = data, weights = fex_c),
        model6 = lm(log_inc ~ age + age2 + female + totalHoursWorked +
                      I(totalHoursWorked^2) + recibe_transferencias +
                      recibe_transferencias:totalHoursWorked,
                    data = data, weights = fex_c),
        model7 = lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                      female:factor(maxEducLevel),
                    data = data, weights = fex_c)
    )
}

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

# RMSE de validación, AIC/BIC y CV leave-one-out (atajo de leverage, ver
# 07_income_prediction_validation.r) para un conjunto ajustado de diez modelos -
# lógica idéntica a la de 07, envuelta en una función porque ahora se aplica seis
# veces (una por régimen de entrenamiento: casos completos, y cada una de las 5
# imputaciones).
evaluate_models <- function(models) {
    validation_rmse <- map_dbl(models, function(model) {
        pred <- predict(model, newdata = validation_data)
        sqrt(mean((validation_data$log_inc - pred)^2, na.rm = TRUE))
    })

    loocv_rmse <- map_dbl(models, function(model) {
        h <- hatvalues(model)
        e <- residuals(model)
        # Un nivel de factor con una sola observación de entrenamiento (el
        # factor(relab) == 8 de model2, n = 1) le da a esa fila h_ii = 1 exacto:
        # quitarla eliminaría toda su categoría, así que su residuo
        # leave-one-out es un 0/0 matemáticamente indefinido, no solo
        # numéricamente inestable - se excluye en vez de dejarlo al redondeo de
        # punto flotante (que puede convertir 0/0 en Inf o en un valor finito
        # arbitrario según la dirección del redondeo).
        valid <- (1 - h) > 1e-8
        sqrt(mean((e[valid] / (1 - h[valid]))^2))
    })

    fit_stats <- models |>
        map(glance) |>
        list_rbind(names_to = "model") |>
        select(model, AIC, BIC)

    tibble(
        model            = names(models),
        `Validation RMSE` = validation_rmse[names(models)],
        `LOOCV RMSE`      = loocv_rmse[names(models)]
    ) |>
        left_join(fit_stats, by = "model")
}


## 4. Línea base: entrenamiento de casos completos (misma población que 07)

baseline_results <- evaluate_models(fit_all_models(train_complete))


## 5. Imputación PMM del log(y_total_m) faltante entre trabajadores pagos

# Predictive mean matching: ajusta un modelo lineal de log_inc sobre los
# predictores de abajo usando solo las filas con ingreso observado, ordena esas
# filas por qué tan cerca está su valor predicho del valor predicho de cada fila
# faltante, e imputa cada fila faltante con el log_inc observado real de una de
# sus 5 coincidencias más cercanas (el tamaño de pool de donantes por defecto de
# mice) - nunca una predicción puntual basada en el modelo, siempre un valor real
# de alguien parecido. Esto refleja cómo las agencias estadísticas (p. ej. la CPS
# del US Census Bureau) imputan el ingreso faltante vía hot-deck/PMM. m = 5
# sorteos aleatorios independientes de donante protegen la comparación de abajo
# de depender de la suerte de un solo sorteo; maxit = 1 porque log_inc es la
# única variable incompleta aquí, así que no hay nada contra lo que el algoritmo
# de ecuaciones encadenadas itere. El modelo de imputación es sin ponderar (fex_c
# solo entra en los modelos sustantivos reajustados en la sección 6) - existe
# puramente para encontrar donantes plausibles, no para estimar un parámetro
# poblacional.
impute_vars <- c("log_inc", "age", "age2", "sex", "maxEducLevel", "relab",
                  "formal", "totalHoursWorked", "sizeFirm", "cuentaPropia")

impute_df <- train_all |>
    select(all_of(impute_vars)) |>
    mutate(across(c(sex, maxEducLevel, relab, formal, sizeFirm, cuentaPropia), factor))

set.seed(123)
imp <- mice(impute_df, m = 5, method = "pmm", maxit = 1, printFlag = FALSE)


## 6. Reajustar las diez especificaciones sobre cada uno de los m=5 sets de entrenamiento imputados

# complete(imp, i) preserva el orden de filas de train_all, así que su columna
# log_inc se puede sustituir de vuelta en train_all directamente - cualquier otra
# columna (fex_c, college, subsidio_familiar, ...) que necesite fit_all_models()
# ya está ahí e intacta tras la imputación.
imputed_results <- map_dfr(1:5, function(i) {
    train_imputed_i <- train_all |> mutate(log_inc = complete(imp, i)$log_inc)
    evaluate_models(fit_all_models(train_imputed_i))
}) |>
    group_by(model) |>
    summarise(across(c(`Validation RMSE`, `LOOCV RMSE`, AIC, BIC), mean), .groups = "drop")


## 7. Tabla de comparación: casos completos vs. imputado por PMM, exportar a LaTeX

comparison_table <- baseline_results |>
    rename_with(~ paste0(., " (completos)"), -model) |>
    left_join(
        imputed_results |> rename_with(~ paste0(., " (imputado)"), -model),
        by = "model"
    ) |>
    mutate(Model = model_labels[model]) |>
    select(
        Model,
        `Validation RMSE (completos)`, `Validation RMSE (imputado)`,
        `LOOCV RMSE (completos)`, `LOOCV RMSE (imputado)`,
        `AIC (completos)`, `AIC (imputado)`,
        `BIC (completos)`, `BIC (imputado)`
    ) |>
    arrange(`Validation RMSE (completos)`)

view(comparison_table)

force_float_h <- function(x) {
    sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

n_imputed <- sum(is.na(train_all$log_inc))
comparison_table_tex <- comparison_table |>
    kbl(
        format = "latex", booktabs = TRUE, digits = 4,
        caption = sprintf(
            "Section 3: complete-case (n = %d) vs. PMM-imputed (n = %d, m = 5) training, by validation RMSE, leave-one-out CV RMSE, AIC and BIC",
            nrow(train_complete), nrow(train_all)
        ),
        label = "section3_imputation_comparison"
    ) |>
    kable_styling(latex_options = c("hold_position", "scale_down")) |>
    as.character() |>
    force_float_h()

writeLines(comparison_table_tex, "output/tables/section3_imputation_comparison.tex")


## 8. Diagnóstico: densidad del log-ingreso observado vs. imputado

# Junta los valores imputados de los 5 sorteos (cada fila faltante aporta 5
# puntos, uno por imputación) contra la única distribución observada, para
# revisar que los sorteos PMM caigan en un rango plausible en vez de amontonarse
# en valores implausibles.
imputed_values <- map_dfr(1:5, function(i) {
    completed <- complete(imp, i)
    tibble(log_inc = completed$log_inc[is.na(train_all$log_inc)])
})

density_data <- bind_rows(
    tibble(log_inc = train_all$log_inc[!is.na(train_all$log_inc)], source = "Observado"),
    imputed_values |> mutate(source = "Imputado (PMM)")
)

density_plot <- ggplot(density_data, aes(x = log_inc, fill = source)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Observado" = "#4daad5", "Imputado (PMM)" = "#6c0a8a")) +
    labs(
        title = "Distribución de log-ingreso: observado vs. imputado (PMM)",
        subtitle = paste0(n_imputed, " observaciones imputadas, 5 sorteos de donante cada una"),
        x = "log(y_total_m)",
        y = "Densidad",
        fill = NULL
    ) +
    theme_minimal()

ggsave("output/figures/section3_imputation_density.png", density_plot, width = 8, height = 5)
density_plot
