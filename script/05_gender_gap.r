###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    05_gender_gap.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Estimar la brecha de ingreso laboral por género
#                       incondicional y condicional, recuperar el coeficiente de
#                       género vía la descomposición de Frisch-Waugh-Lovell con
#                       errores estándar analíticos y bootstrap, y comparar los
#                       perfiles edad-ingreso predichos de hombres y mujeres.
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Cargar datos y construir las variables del modelo
# 3. Regresiones de la brecha de género (incondicional y condicional)
# 4. Descomposición FWL de la brecha de género condicional
# 5. Error estándar bootstrap para el coeficiente FWL
# 6. Perfiles edad-ingreso predichos por sexo
# 7. Tabla de regresión y exportación a LaTeX
# 8. Gráfico del perfil edad-ingreso por sexo

################################################################################

# Input:  Dataframe de análisis limpio de 02_data_cleaning.r
# Output: Tabla de regresión (output/tables/gender_gap_regression.tex) y
#         figura del perfil edad-ingreso por sexo
#         (output/figures/age_income_profile_sex.png)

################################################################################


## 1. Cargar librerías

library(pacman)
p_load(
    tidyverse,
    boot,
    broom,
    kableExtra,
    fixest,
    sandwich)


## 2. Cargar datos y construir las variables del modelo

clean_data <- readRDS("data/geih_clean.rds")

# Misma muestra de análisis que 04_age_labor_income.r: adultos ocupados con
# ingreso laboral estrictamente positivo (los ceros y faltantes no pueden entrar
# a un resultado en logs).
# female se construye de la codificación del codebook para sex (=1 hombre, =0 mujer).
clean_data <- clean_data |>
  filter(y_total_m > 0) |>
  mutate(
    age2    = age^2,
    log_inc = log(y_total_m),
    female  = as.numeric(sex == 0),
    id_hogar = paste(directorio, secuencia_p, sep = "_" )
  )


## 3. Regresiones de la brecha de género (incondicional y condicional)

# Toda regresión se pondera por fex_c, el factor de expansión a nivel de persona,
# por la misma razón que en 04_age_labor_income.r: la GEIH es una encuesta
# compleja, así que la brecha que reportamos es un enunciado sobre los
# trabajadores de Bogotá, no sobre los encuestados que resultamos observar. Los
# errores estándar son robustos a heterocedasticidad, por la misma razón y tras
# la misma deliberación que en 04_age_labor_income.r: hacer cluster (por hogar o
# por ocupación) fue considerado y descartado, porque ninguno es el nivel al que
# el experimento mental asigna el sexo.

# a. Brecha incondicional: la diferencia cruda en la media del log-ingreso entre
# mujeres y hombres. Agrupa todos los canales (educación, horas, ocupación, ...).
gap_uncond <- feols(log_inc ~ female,
                    data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(gap_uncond)


# b. Brecha condicional, controles de capital humano. La edad y la educación son
# predeterminadas respecto al sexo, así que son confusores más que mecanismos:
# esta es nuestra especificación preferida y la que se usa para la descomposición
# FWL de la sección 4.
gap_hk <- feols(log_inc ~ female + age + age2 + factor(maxEducLevel),
                data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(gap_hk)

# c. Brecha condicional, agregando horas trabajadas. El resultado exigido
# (y_total_m) es el ingreso *mensual*, que escala mecánicamente con las horas, y
# las mujeres trabajan menos horas pagas en promedio. Mantener las horas fijas
# por lo tanto mueve la estimación hacia una comparación de pago por hora. Las
# horas las elige el trabajador y a su vez están moldeadas por el sexo, así que
# este es un control post-tratamiento: responde una pregunta distinta que (b), no
# la "mejora".
gap_hours <- feols(log_inc ~ female + age + age2 + factor(maxEducLevel) +
                     totalHoursWorked,
                   data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(gap_hours)

# d. Brecha en términos porcentuales: con un resultado en logs, exp(beta) - 1
# convierte el coeficiente en la diferencia proporcional de ingreso entre mujeres
# y hombres.
gap_pct <- function(model) (exp(coef(model)["female"]) - 1) * 100

gap_pct(gap_uncond)
gap_pct(gap_hk)
gap_pct(gap_hours)




#### FWL

#etapa 1: regresar el ingreso sobre los controles
stage1_FWL <- feols(log_inc ~ age + age2 + factor(maxEducLevel),
                    data = clean_data, weights = ~fex_c)
residuals_stage1 <- resid(stage1_FWL)

#etapa 2: regresar female sobre los controles
stage2_FWL <- feols(female ~ age + age2 + factor(maxEducLevel),
                    data = clean_data, weights = ~fex_c)
residuals_stage2 <- resid(stage2_FWL)

# Ambos conjuntos de residuos vuelven a clean_data como columnas: la etapa 3
# necesita un data frame donde feols pueda encontrar fex_c para ponderar la
# regresión. Las filas calzan porque la muestra de análisis ya no tiene valores
# faltantes, así que ninguna observación se descartó en las etapas 1 y 2.
clean_data <- clean_data |>
  mutate(res_income = residuals_stage1, res_female = residuals_stage2)


#etapa 3: regresar los residuos de la etapa 1 sobre los residuos de la etapa 2
stage3_FWL <- feols(res_income ~ res_female,
                    data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(stage3_FWL)

# La estimación puntual coincide exactamente con la regresión completa: esto es
# lo que el problem set nos pide explicar. Las etapas 1 y 2 remueven todo lo que
# los controles explican, así que la etapa 3 identifica el coeficiente de género
# a partir de la variación de female que es ortogonal a la edad y la educación -
# precisamente la variación que usa la regresión completa.
c(full = unname(coef(gap_hk)["female"]),
  fwl  = unname(coef(stage3_FWL)["res_female"]))

#grados de libertad del modelo completo
df_full_model <- gap_hk %>%
  degrees_freedom("resid")

# Una nota sobre los errores estándar: la etapa 3 reporta 0.013203 mientras que
# la regresión completa reporta 0.013206. Difieren porque feols solo ve dos
# parámetros en la etapa 3 (df = n - 2 = 14,761) y no puede saber que las etapas
# 1 y 2 ya gastaron grados de libertad en los controles (df = n - k = 14,754).
# Reescalar el error estándar de la etapa 3 por sqrt(14761/14754) = 1.000237
# recupera exactamente el de la regresión completa. Reportamos el error estándar
# de la regresión completa, que es el correcto; la brecha de 0.02% es un
# artefacto contable de correr FWL a mano, no una diferencia entre los dos
# estimadores.

 #grados de libertad del modelo FWL
  df_FWL_model <- stage3_FWL %>%
  degrees_freedom("resid")


#errores estándar
## stage3_FWL
se_FWL <- se(stage3_FWL)["res_female"]

## modelo completo
se_full_model <- se(gap_hk)["female"]

## reescalar la etapa 3 por la razón de grados de libertad recupera exactamente
## el error estándar de la regresión completa
se_FWL_corrected <- se_FWL * sqrt(df_FWL_model / df_full_model)

c(fwl_raw        = unname(se_FWL),
  fwl_corrected  = unname(se_FWL_corrected),
  full_model     = unname(se_full_model))


## 5. Error estándar bootstrap para el coeficiente de género

# El problem set pide errores estándar tanto analíticos como bootstrap. El
# analítico es el SE robusto de arriba; este es su contraparte bootstrap.
#
# El remuestreo es a nivel de individuo, en línea con esos errores estándar
# robustos: el pairs bootstrap ordinario es asintóticamente equivalente al
# estimador de varianza de White, así que los dos deberían caer en el mismo
# número.
#
# Reajustamos la especificación completa en vez de rehacer las tres etapas FWL en
# cada réplica: la sección 4 mostró que ambas dan coeficientes de género
# numéricamente idénticos, así que las dos regresiones extra por réplica solo
# costarían tiempo.

B <- 5000 # número de muestras bootstrap

# vcovBS no acepta objetos fixest, así que la misma especificación se reajusta
# con lm() puramente como vehículo para el bootstrap. Los coeficientes son
# idénticos a gap_hk; solo difiere la clase del objeto.
gap_hk_lm <- lm(log_inc ~ female + age + age2 + factor(maxEducLevel),
                data = clean_data, weights = fex_c)

set.seed(123) # para reproducibilidad
vcov_boot <- vcovBS(gap_hk_lm, R = B)
se_boot   <- sqrt(vcov_boot["female", "female"])

# Errores estándar analítico y bootstrap lado a lado.
c(analytical = unname(se_full_model),
  bootstrap  = unname(se_boot))


## 6. Perfiles edad-ingreso predichos por sexo

# a. Las especificaciones de arriba les dan a hombres y mujeres el mismo perfil
# de edad desplazado verticalmente por la dummy de género, así que ambos
# alcanzarían el pico exactamente a la misma edad y comparar sus picos sería
# vacuo. Interactuar female con age y age2 deja que cada sexo tenga su propia
# curvatura, que es lo que el problem set nos pide comparar.
gap_interact <- feols(
  log_inc ~ female + age + age2 + female:age + female:age2 +
    factor(maxEducLevel),
  data = clean_data, weights = ~fex_c, vcov = "hetero"
)
summary(gap_interact)

# b. Edad pico implícita por sexo. Para los hombres (female = 0) las
# interacciones desaparecen y aplica la fórmula usual. Para las mujeres
# (female = 1) se encienden, así que los términos lineal y cuadrático pasan a ser
# las sumas de abajo.
peak_age_by_sex <- function(model) {
  b <- coef(model)
  c(
    men   = unname(-b["age"] / (2 * b["age2"])),
    women = unname(-(b["age"] + b["female:age"]) /
                     (2 * (b["age2"] + b["female:age2"])))
  )
}

peak_age_by_sex(gap_interact)

# c. Intervalos de confianza para las dos edades pico. Volvemos a boot() a mano
# en vez de vcovBS: una edad pico es una razón de coeficientes, y las funciones
# tipo sandwich solo devuelven matrices de varianza para los coeficientes mismos.
#
# También hacemos bootstrap de la diferencia entre los dos picos. Esa es la
# cantidad que realmente responde si hombres y mujeres alcanzan el pico a edades
# distintas: los coeficientes de interacción individuales no son significativos
# por sí solos, pero las edades pico son una combinación no lineal de ellos, así
# que su diferencia hay que probarla directamente en vez de leerla de esos
# estadísticos t.
peak_sex_stat <- function(data, index) {
  model <- feols(
    log_inc ~ female + age + age2 + female:age + female:age2 +
      factor(maxEducLevel),
    data = data[index, ], weights = ~fex_c
  )
  peaks <- peak_age_by_sex(model)
  c(peaks, difference = unname(peaks["men"] - peaks["women"]))
}

set.seed(123) # para reproducibilidad
boot_peak_sex <- boot(clean_data, peak_sex_stat, R = B)
boot_peak_sex

boot.ci(boot_peak_sex, type = "perc", index = 1)  # hombres
boot.ci(boot_peak_sex, type = "perc", index = 2)  # mujeres
boot.ci(boot_peak_sex, type = "perc", index = 3)  # diferencia


## 7. Tabla de regresión y exportación a LaTeX

# El problem set pide una tabla que compare las brechas incondicional y
# condicional con errores estándar analíticos y bootstrap y una medida de ajuste
# dentro de muestra. Como el objeto de interés es el coeficiente de género y no
# todo el vector de coeficientes, la tabla lleva una fila por especificación.

# a. Errores estándar bootstrap para cada especificación, no solo la preferida.
# Misma ruta que en la sección 5: reajustar con lm() porque vcovBS no toma
# objetos fixest.
se_boot_of <- function(formula) {
  m <- lm(formula, data = clean_data, weights = fex_c)
  set.seed(123)
  sqrt(vcovBS(m, R = B)["female", "female"])
}

f_uncond <- log_inc ~ female
f_hk     <- log_inc ~ female + age + age2 + factor(maxEducLevel)
f_hours  <- log_inc ~ female + age + age2 + factor(maxEducLevel) +
  totalHoursWorked

# b. Una fila por especificación.
gap_table <- tibble(
  Specification = c(
    "(1) Unconditional",
    "(2) + age, age2, education",
    "(3) + hours worked"
  ),
  Female    = c(coef(gap_uncond)["female"], coef(gap_hk)["female"],
                coef(gap_hours)["female"]),
  SE_rob    = c(se(gap_uncond)["female"], se(gap_hk)["female"],
                se(gap_hours)["female"]),
  SE_boot   = c(se_boot_of(f_uncond), se_boot_of(f_hk), se_boot_of(f_hours)),
  Gap_pct   = c(gap_pct(gap_uncond), gap_pct(gap_hk), gap_pct(gap_hours)),
  R2        = c(r2(gap_uncond, "r2"), r2(gap_hk, "r2"), r2(gap_hours, "r2")),
  N         = c(nobs(gap_uncond), nobs(gap_hk), nobs(gap_hours))
)

gap_table

# c. Edades pico implícitas por sexo, con sus intervalos de confianza bootstrap.
ci_of <- function(i) boot.ci(boot_peak_sex, type = "perc", index = i)$percent[4:5]

peak_table <- tibble(
  Group    = c("Men", "Women", "Difference (men - women)"),
  Peak_age = boot_peak_sex$t0,
  CI_lower = c(ci_of(1)[1], ci_of(2)[1], ci_of(3)[1]),
  CI_upper = c(ci_of(1)[2], ci_of(2)[2], ci_of(3)[2])
)

peak_table

# d. Exportar ambas a LaTeX, mismo patrón que 02 y 04: kbl con booktabs, y [!h]
# forzado a [H] para que las tablas no puedan flotar fuera de su propia sección.
force_float_h <- function(x) {
  sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

gap_table_tex <- gap_table |>
  rename(
    `Female` = Female, `SE (robust)` = SE_rob,
    `SE (bootstrap)` = SE_boot, `Gap (\\%)` = Gap_pct, `R$^2$` = R2
  ) |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 4, escape = FALSE,
    caption = "Gender labour income gap: unconditional and conditional",
    label = "gender_gap"
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  as.character() |>
  force_float_h()

writeLines(gap_table_tex, "output/tables/gender_gap_regression.tex")

peak_table_tex <- peak_table |>
  rename(`Peak age` = Peak_age, `CI lower` = CI_lower, `CI upper` = CI_upper) |>
  kbl(
    format = "latex", booktabs = TRUE, digits = 2,
    caption = "Implied peak age by sex, with 95\\% bootstrap confidence intervals",
    label = "peak_age_sex"
  ) |>
  kable_styling(latex_options = c("hold_position")) |>
  as.character() |>
  force_float_h()

writeLines(peak_table_tex, "output/tables/gender_peak_age_by_sex.tex")


## 8. Gráfico del perfil edad-ingreso por sexo

# a. Grilla de edad sobre el rango observado, una copia por sexo. La educación se
# mantiene en su categoría modal: como en 04, sin interacción edad-educación en
# el modelo esto solo desplaza ambas curvas verticalmente - no cambia ni su forma
# ni las edades pico, así que la comparación no se ve afectada por la elección.
ref_educ <- clean_data |> count(maxEducLevel) |> slice_max(n, n = 1) |>
  pull(maxEducLevel)

# La grilla se detiene en 70 y no en el máximo muestral de 91: el percentil 99 de
# la edad es 71, así que más allá de eso las curvas se ajustan sobre cerca del 1%
# de las observaciones. Graficar hasta 91 le daría un tercio del ancho del
# gráfico a ese 1% y dejaría que la caída extrapolada de la parábola dominara la
# imagen.
age_max_plot <- 70
age_grid <- seq(min(clean_data$age), age_max_plot, by = 1)

profiles_sex <- expand_grid(age = age_grid, female = c(0, 1)) |>
  mutate(age2 = age^2, maxEducLevel = ref_educ)

profiles_sex$log_inc_pred <- predict(gap_interact, newdata = profiles_sex)

profiles_sex <- profiles_sex |>
  mutate(sexo = if_else(female == 1, "Women", "Men"))

# b. Graficar ambos perfiles con sus edades pico marcadas.
# lab_hjust empuja cada etiqueta de pico lejos de la otra para que no se
# traslapen cuando los dos picos quedan cerca.
peaks_sex <- tibble(
  sexo     = c("Men", "Women"),
  peak_age = c(boot_peak_sex$t0[1], boot_peak_sex$t0[2])
) |>
  mutate(lab_hjust = if_else(peak_age == min(peak_age), 1.1, -0.1))

# El resultado está en logs, que nadie puede leer de un eje, así que los breaks
# se ubican en montos redondos de pesos (duplicando, el espaciado natural en
# escala log) y se etiquetan en pesos. La curva no cambia; solo el eje se vuelve
# legible.
peso_breaks <- c(6e5, 8e5, 1e6, 1.5e6, 2e6, 3e6)
peso_labels <- c("$600K", "$800K", "$1.0M", "$1.5M", "$2.0M", "$3.0M")

# Las series se etiquetan sobre las curvas mismas, así que la identidad nunca
# depende de emparejar un color con una leyenda.
series_labels_sex <- profiles_sex |>
  group_by(sexo) |>
  slice_max(age, n = 1) |>
  ungroup()

y_top <- max(profiles_sex$log_inc_pred)

# La banda entre las curvas es la brecha de género a cada edad, así que
# sombrearla convierte la brecha que se ensancha en algo que se puede ver en vez
# de inferir.
gap_band <- profiles_sex |>
  select(age, sexo, log_inc_pred) |>
  pivot_wider(names_from = sexo, values_from = log_inc_pred)

age_profile_sex_plot <- ggplot(profiles_sex,
                               aes(x = age, y = log_inc_pred, color = sexo)) +
  geom_ribbon(
    data = gap_band, aes(x = age, ymin = Women, ymax = Men),
    inherit.aes = FALSE, fill = "grey55", alpha = 0.15
  ) +
  geom_vline(
    data = peaks_sex, aes(xintercept = peak_age, color = sexo),
    linetype = "dashed", linewidth = 0.5, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.9) +
  geom_text(
    data = peaks_sex,
    aes(x = peak_age, y = y_top, label = sprintf("%.1f", peak_age),
        hjust = lab_hjust),
    vjust = -1.2, size = 3.4, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(
    data = series_labels_sex, aes(label = sexo),
    hjust = -0.2, size = 3.8, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Sex",
    values = c(Men = "#235da3", Women = "#ad0d5d")
  ) +
  scale_y_continuous(breaks = log(peso_breaks), labels = peso_labels,
                     expand = expansion(mult = c(0.05, 0.14))) +
  scale_x_continuous(breaks = seq(20, 70, by = 10)) +
  coord_cartesian(xlim = c(min(age_grid), max(age_grid) + 6), clip = "off") +
  labs(
    title = "Women's earnings peak almost five years earlier than men's",
    x = "Age",
    y = NULL,
    caption = paste0(
      "Predicted monthly labour income, education held at its modal level. ",
      "The shaded band is the gender gap,\nwhich widens from 19% at age 25 to ",
      "38% at age 55. Dashed lines mark each profile's implied peak age.\n",
      "Vertical axis on a log scale; ages shown to 70, the sample's 99th ",
      "percentile. GEIH 2018, Bogota: employed\nadults 18+ with positive ",
      "labour income (n = 14,763), weighted by fex_c."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position       = "none",
    panel.grid.minor      = element_blank(),
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_line(color = "grey92", linewidth = 0.4),
    plot.title            = element_text(face = "bold", size = 14,
                                         margin = margin(b = 14)),
    plot.title.position   = "plot",
    plot.caption          = element_text(color = "grey45", hjust = 0, size = 8),
    plot.caption.position = "plot",
    plot.margin           = margin(12, 28, 10, 10)
  )

ggsave("output/figures/age_income_profile_sex.png", age_profile_sex_plot,
       width = 8, height = 5, dpi = 300)
age_profile_sex_plot

