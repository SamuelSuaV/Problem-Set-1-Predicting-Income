###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    07_income_prediction_validation.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Agregar cinco especificaciones adicionales de predicción
#                       del ingreso a los cinco modelos de 04_age_labor_income.r
#                       y 05_gender_gap.r (entrenados en
#                       06_income_prediction_train.r), comparar los diez por RMSE
#                       de validación, AIC, BIC y una CV leave-one-out exacta vía
#                       el atajo de leverage, y graficar la importancia de
#                       variables del mejor modelo.
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Cargar datos y construir las variables del modelo (split train / validación)
# 3. Cinco especificaciones adicionales (justificación económica en los comentarios)
# 4. Combinar con los modelos de las Secciones 1/2 de 06_income_prediction_train.r
# 5. RMSE del conjunto de validación
# 6. Ajuste dentro de muestra: AIC y BIC
# 7. CV leave-one-out vía el atajo de leverage
# 8. Tabla de comparación de modelos y exportación a LaTeX
# 9. Importancia de variables del mejor modelo (|beta| estandarizado) y gráfico
# 10. RMSE de validación por subgrupo para el mejor modelo (chequeo de sesgo) y gráfico de barras

################################################################################

# Input:  Dataframe de análisis limpio de 02_data_cleaning.r y los modelos de
#         entrenamiento guardados por 06_income_prediction_train.r
# Output: Tabla de comparación de modelos (output/tables/section3_model_comparison.tex),
#         figura de importancia de variables
#         (output/figures/section3_variable_importance.png) y figura del
#         RMSE de validación por subgrupo (output/figures/section3_subgroup_rmse.png)

################################################################################


## 1. Cargar librerías

library(pacman)
p_load(
    tidyverse,
    broom,
    kableExtra)


## 2. Cargar datos y construir las variables del modelo (split train / validación)

# Misma muestra de análisis que 04_age_labor_income.r, 05_gender_gap.r y
# 06_income_prediction_train.r: adultos ocupados con ingreso laboral
# estrictamente positivo. Dos variables derivadas extra alimentan las
# especificaciones adicionales de la sección 3 de abajo.
clean_data <- readRDS("data/geih_clean.rds") |>
    filter(y_total_m > 0) |>
    mutate(
        age2    = age^2,
        log_inc = log(y_total_m),
        female  = as.numeric(sex == 0),
        # p6585s3: "¿el mes pasado recibió subsidio familiar?" (1 = sí,
        # 2/9 = no/no sabe). Solo se pregunta a trabajadores dependientes, así
        # que ~32% de la muestra es NA - se mantiene como su propia categoría
        # "No aplica" (no se descarta) para que esas filas igual entren a cada
        # modelo, y porque la ausencia misma sirve de proxy de una relación
        # laboral no asalariada.
        subsidio_familiar = case_when(
            p6585s3 == 1        ~ "Sí",
            p6585s3 %in% c(2, 9) ~ "No",
            TRUE                 ~ "No aplica"
        ) |> factor(levels = c("No", "Sí", "No aplica")),
        # p7505: recibió dinero de otros hogares/personas/instituciones en los
        # últimos 12 meses (1 = sí, 2 = no); sin valores faltantes.
        recibe_transferencias = as.numeric(p7505 == 1)
    )

# El split train (1-7) / validación (8-10) de la Sección 3, la misma llave usada
# en 06_income_prediction_train.r.
train_data      <- clean_data |> filter(subset %in% 1:7)
validation_data <- clean_data |> filter(subset %in% 8:10)

# Datos de validación por categoría
validation_formal <- validation_data |>
    filter(formal == 1)

validation_informal <- validation_data |>
    filter(formal == 0)

validation_female <- validation_data |>
    filter(female == 1)

validation_male <- validation_data |>
    filter(female == 0)

validation_college <- validation_data |>
    filter(college == 1)

validation_nocollege <- validation_data |>
    filter(college == 0)

validation_account <- validation_data |>
    filter(cuentaPropia == 1)

validation_noaccount <- validation_data |>
    filter(cuentaPropia == 0)

validation_transfer <- validation_data |>
    filter(recibe_transferencias == 1)

validation_notransfer <- validation_data |>
    filter(recibe_transferencias == 0)

# maxEducLevel tiene siete niveles ordenados, así que sus subgrupos held-out se
# guardan en una lista con nombres (nombre = código del nivel) en vez de un
# objeto por nivel.
validation_by_educ <- validation_data |>
    filter(!is.na(maxEducLevel)) |>
    group_split(maxEducLevel)
names(validation_by_educ) <- map_chr(
    validation_by_educ, \(d) as.character(d$maxEducLevel[1])
)




## 3. Cinco especificaciones adicionales (justificación económica en los comentarios)

# Todas ponderadas por fex_c, por la misma razón que cada regresión de este
# proyecto: la GEIH es una encuesta compleja, no una muestra aleatoria simple.

# a. Prima salarial del sector formal / tamaño de firma, interactuada con un
# título universitario. Los trabajos formales agrupan protecciones legales y
# acceso a empleadores mejor capitalizados (prima salarial formal); las firmas
# más grandes pagan más por el mismo trabajador (salarios de eficiencia /
# mercados laborales internos). La interacción prueba si una credencial
# universitaria se selecciona/premia con más fuerza una vez que un trabajo es
# formal, donde los empleadores pueden verificarla y las estructuras de pago son
# más rígidas.
model3 <- lm(log_inc ~ age + age2 + college + formal + factor(sizeFirm) +
               formal:college,
             data = train_data, weights = fex_c)

# b. Paquete de beneficios no salariales. El subsidio familiar (cajas de
# compensación) es una prestación obligatoria atada a una relación laboral
# formal y asalariada - los "buenos trabajos" agrupan mayor pago y este tipo de
# beneficio en vez de cambiar uno por otro. La interacción prueba si la
# asociación de ese beneficio con el pago es mayor para los trabajadores con
# educación universitaria, que tienden a ocupar trabajos de mayor nivel dentro
# del sector formal.
# NOTA: factor(maxEducLevel) se deja fuera aquí a propósito - en este dataset
# `college` es exactamente `maxEducLevel == 6` (no el nivel 7, a pesar del
# nombre), así que incluir ambos es perfectamente colineal y lm() aliasea un
# coeficiente a NA. El gradiente de maxEducLevel ya está cubierto por
# gap_hk/gap_hours (05) y model7 abajo, así que college solo es el control de
# educación aquí.
model4 <- lm(log_inc ~ age + age2 + college + factor(sizeFirm) +
               subsidio_familiar + subsidio_familiar:college,
             data = train_data, weights = fex_c)

# c. Penalidad del trabajo por cuenta propia, interactuada con un título
# universitario. El trabajo por cuenta propia es, en promedio, una penalidad de
# ingreso de subsistencia (restricciones de liquidez, sin beneficios provistos
# por el empleador), pero los trabajadores por cuenta propia con educación
# universitaria (profesionales/consultores independientes) son una población
# distinta del trabajo por cuenta propia de subsistencia, así que la penalidad
# debería ser menor - o revertirse - para ellos.
model5 <- lm(log_inc ~ age + age2 + college + cuentaPropia +
               cuentaPropia:college,
             data = train_data, weights = fex_c)

# d. Efecto de oferta laboral / salario de reserva de las transferencias
# externas. Teoría estándar de oferta laboral: el ingreso no laboral (remesas,
# ayuda de otros hogares) sube el salario de reserva y puede amortiguar la
# intensidad de la oferta laboral (un efecto ingreso sobre las horas).
# totalHoursWorked entra con un cuadrático para permitir retornos decrecientes
# (o que se revierten) a las horas extra - la mayoría de los trabajos asalariados
# tienen una relación horas-pago plana / con tope de horas extra - y su
# interacción con recibe_transferencias prueba si la relación horas-ingreso es
# más plana para los trabajadores amortiguados por transferencias externas.
model6 <- lm(log_inc ~ age + age2 + female + totalHoursWorked +
               I(totalHoursWorked^2) + recibe_transferencias +
               recibe_transferencias:totalHoursWorked,
             data = train_data, weights = fex_c)

# e. Brecha de género en los retornos a la educación. El gap_hk de
# 05_gender_gap.r mantiene la educación fija como un solo desplazamiento de
# nivel; esta especificación deja que cada categoría de maxEducLevel lleve su
# propia brecha de género, probando si los retornos a la escolaridad divergen por
# sexo (p. ej. un "techo de cristal" que se ensancha con las credenciales, o una
# penalidad por maternidad concentrada entre las mujeres más educadas).
model7 <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel) +
               female:factor(maxEducLevel),
             data = train_data, weights = fex_c)


## 4. Combinar con los modelos de las Secciones 1/2 de 06_income_prediction_train.r

section12_models <- readRDS("output/models/section3_train_models.rds")

all_models <- c(
    section12_models,
    list(model3 = model3, model4 = model4, model5 = model5,
         model6 = model6, model7 = model7)
)

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


## 5. RMSE del conjunto de validación

# predict() sobre el subset 8-10, que ninguno de los diez modelos vio durante el
# ajuste. El RMSE se calcula en la escala de log(y_total_m) en la que se estima
# cada modelo, así que es directamente comparable entre las diez
# especificaciones - convertir de vuelta a pesos necesitaría una corrección de
# retransformación (p. ej. el estimador de smearing de Duan), que está fuera de
# alcance aquí. Los niveles de los factores se revisaron de antemano:
# validation_data no tiene ninguna categoría (relab, sizeFirm, maxEducLevel, ...)
# ausente de train_data, así que predict() no puede fallar por un nivel no visto
# aquí.
validation_rmse <- map_dbl(all_models, function(model) {
    pred <- predict(model, newdata = validation_data)
    sqrt(mean((validation_data$log_inc - pred)^2, na.rm = TRUE))
})


## 6. Ajuste dentro de muestra: AIC y BIC

# Del mismo ajuste de entrenamiento usado para la predicción de validación, así
# que AIC, BIC y el RMSE de validación describen todos el mismo objeto ajustado.
# Como cada modelo se ajusta por mínimos cuadrados ponderados (weights = fex_c),
# AIC/BIC usan la log-verosimilitud gaussiana ponderada implicada por esos pesos
# - internamente consistente entre los diez modelos porque todos llevan los
# mismos pesos.
fit_stats <- all_models |>
    map(glance) |>
    list_rbind(names_to = "model") |>
    select(model, AIC, BIC)


## 7. CV leave-one-out vía el atajo de leverage

# La LOOCV exacta reajustaría cada modelo una vez por cada observación de
# entrenamiento dejada fuera (~10,000 reajustes cada uno). Para mínimos cuadrados
# - ordinarios o ponderados, como aquí - eso es innecesario: dejar fuera la
# observación i y reajustar da exactamente el mismo residuo que
#     e_(i) = e_i / (1 - h_ii)
# donde e_i es el residuo de esa observación del único ajuste sobre todos los
# datos de entrenamiento, y h_ii es su leverage - la i-ésima diagonal de la hat
# matrix, es decir cuánto peso pone el ajuste en su propio valor al suavizar.
# hatvalues() ya tiene en cuenta los pesos fex_c con los que se ajustó cada
# modelo, así que elevar al cuadrado y promediar e_(i) entre observaciones
# reproduce el RMSE que darían n regresiones leave-one-out separadas, a partir de
# una sola regresión en vez de n.
loocv_rmse <- function(model) {
    h <- hatvalues(model)
    e <- residuals(model)
    # Un nivel de factor con una sola observación de entrenamiento (el
    # factor(relab) == 8 de model2, n = 1) le da a esa fila h_ii = 1 exacto:
    # quitarla eliminaría toda su categoría, así que su residuo leave-one-out es
    # un 0/0 matemáticamente indefinido, no solo numéricamente inestable. Se
    # excluye en vez de dejarlo al redondeo de punto flotante, que puede
    # convertir 0/0 en un valor finito arbitrario o Inf según la dirección del
    # redondeo.
    valid <- (1 - h) > 1e-8
    sqrt(mean((e[valid] / (1 - h[valid]))^2))
}

loocv_rmse_vals <- map_dbl(all_models, loocv_rmse)


## 8. Tabla de comparación de modelos y exportación a LaTeX

comparison_table <- tibble(
    model            = names(all_models),
    Model            = model_labels[names(all_models)],
    `LOOCV RMSE`     = loocv_rmse_vals[names(all_models)],
    `Validation RMSE` = validation_rmse[names(all_models)]
) |>
    left_join(fit_stats, by = "model") |>
    select(Model, `LOOCV RMSE`, `Validation RMSE`, AIC, BIC) |>
    arrange(`Validation RMSE`)

view(comparison_table)

force_float_h <- function(x) {
    sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

comparison_table_tex <- comparison_table |>
    kbl(
        format = "latex", booktabs = TRUE, digits = 4,
        caption = "Section 3: model comparison by leave-one-out CV RMSE (leverage shortcut), validation RMSE, AIC and BIC",
        label = "section3_model_comparison"
    ) |>
    kable_styling(latex_options = c("hold_position", "scale_down")) |>
    as.character() |>
    force_float_h()

writeLines(comparison_table_tex, "output/tables/section3_model_comparison.tex")


## 9. Importancia de variables del mejor modelo (|beta| estandarizado) y gráfico

# a. Mejor modelo = menor RMSE de validación, el criterio por el que el problem
# set pide ordenar las diez especificaciones.
best_model_name <- names(which.min(validation_rmse))
best_model <- all_models[[best_model_name]]

# b. Los coeficientes estandarizados (beta * sd(x) / sd(y)) ponen a las variables
# continuas, las dummies y los niveles de factor medidos en escalas distintas
# (años, horas, indicadores 0/1) en el mismo pie, así que sus magnitudes son
# comparables dentro del gráfico.
model_matrix <- model.matrix(best_model)[, -1, drop = FALSE]  # quitar el intercepto
x_sd <- apply(model_matrix, 2, sd)
y_sd <- sd(train_data$log_inc)

std_betas <- tidy(best_model) |>
    filter(term != "(Intercept)") |>
    mutate(std_beta = estimate * x_sd[term] / y_sd)

importance_plot <- ggplot(
    std_betas,
    aes(x = reorder(term, abs(std_beta)), y = std_beta, fill = std_beta > 0)
) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c(`TRUE` = "#4daad5", `FALSE` = "#6c0a8a"), guide = "none") +
    labs(
        title = paste("Variable importance:", model_labels[[best_model_name]]),
        subtitle = "Standardized coefficients, ranked by |beta| (best model by validation RMSE)",
        x = NULL,
        y = "Standardized coefficient"
    ) +
    theme_minimal()

ggsave("output/figures/section3_variable_importance.png", importance_plot,
       width = 8, height = 5)
importance_plot


## 10. RMSE de validación por subgrupo para el mejor modelo (chequeo de sesgo)

# El único RMSE de validación global de la sección 5 esconde a quién predice bien
# y a quién predice mal el mejor modelo. Recalcular ese mismo RMSE held-out
# (escala log(y_total_m), best_model = menor RMSE de validación global) dentro de
# cada contraste de categoría de la sección 2 - más cada maxEducLevel - muestra
# dónde el modelo es sistemáticamente menos preciso, es decir para qué grupos sus
# predicciones de ingreso están sesgadas. El RMSE se deja sin ponderar aquí,
# exactamente como en la sección 5, así que cada barra es comparable con el
# número global.

# a. RMSE por subgrupo: la fórmula de la sección 5 aplicada a un slice del
# conjunto de validación.
subgroup_rmse <- function(data) {
    pred <- predict(best_model, newdata = data)
    sqrt(mean((data$log_inc - pred)^2, na.rm = TRUE))
}

# b. Contrastes de categoría binaria - los frames de validación prefiltrados de
# la sección 2.
binary_subgroups <- tibble(
    category = c("Formality", "Formality", "Gender", "Gender",
                 "College", "College", "Own-account", "Own-account",
                 "Transfers", "Transfers"),
    subgroup = c("Formal", "Informal", "Female", "Male",
                 "College", "No college", "Cuenta propia",
                 "No cuenta propia", "Recibe transf.", "No recibe transf."),
    data = list(validation_formal, validation_informal,
                validation_female, validation_male,
                validation_college, validation_nocollege,
                validation_account, validation_noaccount,
                validation_transfer, validation_notransfer)
) |>
    mutate(n = map_int(data, nrow), rmse = map_dbl(data, subgroup_rmse)) |>
    select(-data)

# c. maxEducLevel - etiquetado según el codebook (1 none ... 7 tertiary).
# Construido a partir de la lista con nombres de la sección 2 para que cada nivel
# entre incluso cuando tiene solo un puñado de filas de validación (ver la
# columna n).
educ_labels <- c(
    "1" = "None", "2" = "Preschool", "3" = "Primary (inc.)",
    "4" = "Primary (comp.)", "5" = "Secondary (inc.)",
    "6" = "Secondary (comp.)", "7" = "Tertiary"
)

educ_subgroups <- tibble(
    category = "Education",
    subgroup = unname(educ_labels[names(validation_by_educ)]),
    n        = map_int(validation_by_educ, nrow),
    rmse     = map_dbl(validation_by_educ, subgroup_rmse)
)

# d. Una sola tabla larga. El orden de las categorías es fijo para el gráfico;
# los niveles de educación mantienen su orden de menor a mayor escolaridad dentro
# de su propio panel.
subgroup_errors <- bind_rows(binary_subgroups, educ_subgroups) |>
    mutate(
        category = factor(category, levels = c(
            "Formality", "Gender", "College", "Own-account",
            "Transfers", "Education"
        )),
        subgroup = factor(subgroup, levels = c(
            setdiff(subgroup, educ_labels), unname(educ_labels)
        ))
    ) |>
    arrange(category, subgroup)

overall_rmse <- validation_rmse[[best_model_name]]

view(subgroup_errors)

# e. Gráfico de barras: un panel por categoría, barras = RMSE de validación del
# subgrupo, línea punteada = RMSE de validación global como referencia.
subgroup_error_plot <- ggplot(
    subgroup_errors,
    aes(x = subgroup, y = rmse, fill = category)
) +
    geom_col() +
    geom_hline(yintercept = overall_rmse, linetype = "dashed",
               colour = "grey35") +
    geom_text(aes(label = sprintf("%.3f", rmse)), hjust = -0.15, size = 2.8) +
    coord_flip() +
    facet_grid(category ~ ., scales = "free_y", space = "free_y",
               switch = "y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    labs(
        title = paste("Validation RMSE by subgroup:",
                      model_labels[[best_model_name]]),
        subtitle = sprintf(
            "Held-out RMSE on log income within each category; dashed line = overall validation RMSE (%.3f)",
            overall_rmse
        ),
        x = NULL, y = "Validation RMSE (log income scale)"
    ) +
    theme_minimal() +
    theme(
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        panel.spacing = unit(0.5, "lines")
    )

ggsave("output/figures/section3_subgroup_rmse.png", subgroup_error_plot,
       width = 9, height = 7)
subgroup_error_plot
