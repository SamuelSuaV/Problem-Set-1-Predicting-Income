###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    04_age_labor_income.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Estimar los perfiles edad-ingreso laboral incondicional
#                       y condicional, la edad pico implícita de cada uno y su
#                       intervalo de confianza bootstrap.
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Cargar datos y construir las variables del modelo
# 3. Regresiones del perfil edad-ingreso
# 4. Edad pico implícita
# 5. Intervalos de confianza bootstrap para la edad pico
# 6. Tabla de regresión y exportación a LaTeX
# 7. Gráfico del perfil edad-ingreso

################################################################################

# Input:  Dataframe de análisis limpio de 02_data_cleaning.r
# Output: Tabla de regresión (output/tables/age_income_regression.tex) y
#         figura del perfil edad-ingreso (output/figures/age_income_profile.png)

################################################################################


## 1. Cargar librerías
#install.packages("pacman")
library(pacman)
p_load(
    tidyverse,
    boot,
    broom,
    kableExtra,
    fixest)

## 2. Cargar datos y construir las variables del modelo

clean_data <- readRDS("data/geih_clean.rds")

# id_hogar identifica un hogar: directorio es la vivienda y secuencia_p el hogar
# dentro de ella, así que solo el par es único. No hacemos cluster por él (ver
# sección 3), pero se mantiene para que la alternativa con cluster se pueda
# revisar en una línea si alguien lo pide.
clean_data <- clean_data |>
    filter(y_total_m > 0) |>
    mutate(
      age2     = age^2,
      log_inc  = log(y_total_m),  #creamos una variable de logaritmo del ingreso para poder hacer la regresión
      id_hogar = paste(directorio, secuencia_p, sep = "_")
    )

# Etiquetas de relab (duplicadas de 02_data_cleaning.r: cada script corre por su
# cuenta, así que el vector definido allá no está disponible aquí).
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

clean_data |>
  count(relab) |>
  mutate(relab_label = relab_labels[as.character(relab)])

## 3. Regresiones del perfil edad-ingreso

# Toda regresión de abajo se pondera por fex_c, el factor de expansión a nivel de
# persona: la GEIH es una encuesta compleja, no una muestra aleatoria simple, así
# que el perfil que reportamos es un enunciado sobre los trabajadores de Bogotá y
# no sobre los 14,763 encuestados que resultamos observar. Esto también mantiene
# la sección consistente con los descriptivos ponderados de 03_data_description.r.
# En la práctica la elección es inocua aquí: quitar los pesos mueve la edad pico
# implícita en 0.11 años (modelo 1) y 0.02 años (modelo 2), un orden de magnitud
# menos que el ancho de sus intervalos de confianza bootstrap.

# Los errores estándar son robustos a heterocedasticidad. Los datos de ingreso
# son marcadamente heterocedásticos - el ingreso de un asalariado es mucho más
# predecible que el de un trabajador por cuenta propia - e ignorar eso subestima
# el error estándar de beta_age en 24% (clásico 0.00325 vs robusto 0.00401).
#
# Consideramos hacer cluster y decidimos no hacerlo, siguiendo a Abadie, Athey,
# Imbens & Wooldridge, "When Should You Adjust Standard Errors for Clustering?":
# el cluster se justifica por cómo se asigna el tratamiento o cómo se tomó la
# muestra, no por la mera presencia de correlación dentro de los grupos.
#   - Por hogar (directorio + secuencia_p): 70% de la muestra comparte hogar, y
#     la GEIH sí entrevista hogares completos, pero el experimento mental que
#     asigna la edad no opera a nivel de hogar. Habría subido el error estándar
#     solo 4% de todas formas (0.00419), porque los hogares promedian 1.7
#     personas aquí - demasiado pequeños para que el cluster tenga efecto.
#   - Por ocupación (oficio): habría duplicado los errores estándar (0.00910)
#     porque esos clusters promedian 187 personas, pero las ocupaciones no son
#     una unidad de muestreo ni de asignación, así que esa correlación refleja
#     una variable omitida más que el diseño.
# Ninguna conclusión cualitativa cambia bajo ninguna de estas elecciones.

# a. Perfil incondicional
model1 <- feols(log_inc ~ age + age2,
                data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(model1) # revisamos el resumen del modelo

# b. Perfil condicional: agrega el total de horas trabajadas y el tipo de empleo,
# y ningún otro control (como lo exige el problem set).
model2 <- feols(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                data = clean_data, weights = ~fex_c, vcov = "hetero")
summary(model2) # revisamos el resumen del modelo


## 4. Edad pico implícita

# El perfil es una parábola, así que el ingreso predicho alcanza su máximo donde
# su pendiente es cero: age* = -beta_age / (2 * beta_age2).

# a. Perfil incondicional
peak_age1 <- -coef(model1)["age"] / (2 * coef(model1)["age2"])

# b. Perfil condicional
peak_age2 <- -coef(model2)["age"] / (2 * coef(model2)["age2"])


## 5. Intervalos de confianza bootstrap para la edad pico

B <- 5000 # número de muestras bootstrap

# Cada réplica reajusta la misma especificación ponderada de la sección 3, así
# que la distribución bootstrap está centrada en las estimaciones que realmente
# reportamos.
#
# La unidad de remuestreo es el individuo, en línea con los errores estándar
# robustos a heterocedasticidad de la sección 3: el pairs bootstrap ordinario es
# asintóticamente equivalente al estimador de varianza robusto (White), así que
# los dos coinciden por construcción y no por coincidencia.

# a. Perfil incondicional
peak_age_stat1 <- function(data, index) {
  model <- feols(log_inc ~ age + age2,
                 data = data[index, ], weights = ~fex_c)
  unname(-coef(model)["age"] / (2 * coef(model)["age2"]))
}

set.seed(123) # para reproducibilidad
boot_peak_age1 <- boot(clean_data, peak_age_stat1, R = B)
boot_peak_age1          # original, bias y std. error bootstrap
sd(boot_peak_age1$t)    # sd bootstrap (equivalente al "std. error" de arriba)
boot.ci(boot_peak_age1, type = "perc")  # IC 95%


# b. Perfil condicional
peak_age_stat2 <- function(data, index) {
  model <- feols(log_inc ~ age + age2 + totalHoursWorked + factor(relab),
                 data = data[index, ], weights = ~fex_c)
  unname(-coef(model)["age"] / (2 * coef(model)["age2"]))
}

set.seed(123) # para reproducibilidad
boot_peak_age2 <- boot(clean_data, peak_age_stat2, R = B)
boot_peak_age2
sd(boot_peak_age2$t)
boot.ci(boot_peak_age2, type = "perc")  # IC 95%


## 6. Tabla de regresión y exportación a LaTeX

# a. Una fila por coeficiente, un par de columnas por especificación. Los
# términos que solo aparecen en el modelo condicional quedan como NA del lado del
# modelo 1.
table_regression <- full_join(
  tidy(model1) |> select(term, estimate, std.error),
  tidy(model2) |> select(term, estimate, std.error),
  by = "term",
  suffix = c("_model1", "_model2")
)

# b. Filas extra que la tabla de coeficientes no trae: edad pico, su IC bootstrap
# y la medida de ajuste dentro de muestra que exige el problem set.
extra_rows <- tibble(
  term = c("Peak age", "Peak age CI lower", "Peak age CI upper", "R-squared"),
  estimate_model1 = c(
    peak_age1,
    boot.ci(boot_peak_age1, type = "perc")$percent[4],
    boot.ci(boot_peak_age1, type = "perc")$percent[5],
    glance(model1)$r.squared
  ),
  std.error_model1 = NA_real_,
  estimate_model2 = c(
    peak_age2,
    boot.ci(boot_peak_age2, type = "perc")$percent[4],
    boot.ci(boot_peak_age2, type = "perc")$percent[5],
    glance(model2)$r.squared
  ),
  std.error_model2 = NA_real_
)

table_regression <- bind_rows(table_regression, extra_rows)
view(table_regression)

# c. Exportar a LaTeX, mismo patrón que 02_data_cleaning.r: kbl con booktabs,
# forzando [H] en vez del [!h] de kableExtra para que la tabla no pueda flotar
# más allá de su propia sección en el documento compilado (requiere
# \usepackage{float}).
force_float_h <- function(x) {
  sub("\\\\begin\\{table\\}\\[!h\\]", "\\\\begin{table}[H]", x)
}

table_regression_tex <- table_regression |>
  rename(
    Term = term,
    `Model 1` = estimate_model1,
    `SE (Model 1)` = std.error_model1,
    `Model 2` = estimate_model2,
    `SE (Model 2)` = std.error_model2
  ) |>
  kbl(
    # NOTA: con digits = 4 el SE de age2 se imprime como 0.0000 (su valor real es
    # ~0.0000376, demasiado pequeño para 4 decimales) - revisar antes de usar
    # esta tabla en las diapositivas (más decimales solo para esa fila, o
    # notación científica).
    format = "latex", booktabs = TRUE, digits = 4,
    caption = "Age-income profile: unconditional vs. conditional",
    label = "age_income"
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  as.character() |>
  force_float_h()

writeLines(table_regression_tex, "output/tables/age_income_regression.tex")

# d. Exportaciones listas para el deck de la Sección 1
# (presentation/age_income_slides.r -> ... /age_income_slides.tex). Dos archivos,
# ambos regenerados en cada corrida para que las diapositivas queden en sync con
# este script:
#   - age_income_stats.tex: los números clave como macros de LaTeX, con \input en
#     el preámbulo del deck y usados en la diapositiva "Resultado principal".
#   - age_income_slide_table.tex: una tabla de regresión limpia (sin envoltura de
#     float, dummies de relab colapsadas en una sola fila de efectos fijos, age2
#     con suficientes decimales para mostrar su SE), con \input dentro de un frame.

ci1 <- boot.ci(boot_peak_age1, type = "perc")$percent[4:5]
ci2 <- boot.ci(boot_peak_age2, type = "perc")$percent[4:5]

fmt <- function(x, d = 1) formatC(x, format = "f", digits = d)

stats_macros <- c(
  paste0("\\newcommand{\\AgePeakUncond}{",   fmt(peak_age1), "}"),
  paste0("\\newcommand{\\AgePeakUncondLo}{", fmt(ci1[1]), "}"),
  paste0("\\newcommand{\\AgePeakUncondHi}{", fmt(ci1[2]), "}"),
  paste0("\\newcommand{\\AgePeakCond}{",     fmt(peak_age2), "}"),
  paste0("\\newcommand{\\AgePeakCondLo}{",   fmt(ci2[1]), "}"),
  paste0("\\newcommand{\\AgePeakCondHi}{",   fmt(ci2[2]), "}"),
  paste0("\\newcommand{\\AgeRsqUncond}{",    fmt(glance(model1)$r.squared, 3), "}"),
  paste0("\\newcommand{\\AgeRsqCond}{",      fmt(glance(model2)$r.squared, 3), "}"),
  paste0("\\newcommand{\\AgeNobs}{",         format(nobs(model1), big.mark = ","), "}"),
  paste0("\\newcommand{\\AgeBootReps}{",     format(B, big.mark = ","), "}"),
  paste0("\\newcommand{\\AgeBetaAge}{",      fmt(coef(model1)["age"], 3), "}"),
  paste0("\\newcommand{\\AgeBetaAgeSq}{",    fmt(coef(model1)["age2"], 5), "}")
)
writeLines(stats_macros, "output/tables/age_income_stats.tex")

t1 <- tidy(model1)
t2 <- tidy(model2)
pick <- function(tab, term, col) {
  val <- tab[[col]][tab$term == term]
  if (length(val) == 0) NA_real_ else val
}
cell <- function(est, se, d = 3) {
  if (is.na(est)) return("")
  out <- formatC(est, format = "f", digits = d)
  if (!is.na(se)) out <- paste0(out, " (", formatC(se, format = "f", digits = d), ")")
  out
}

slide_tbl <- tribble(
  ~Variable, ~Unconditional, ~Conditional,
  "Age",
    cell(pick(t1, "age", "estimate"), pick(t1, "age", "std.error")),
    cell(pick(t2, "age", "estimate"), pick(t2, "age", "std.error")),
  "Age$^2$",
    cell(pick(t1, "age2", "estimate"), pick(t1, "age2", "std.error"), 6),
    cell(pick(t2, "age2", "estimate"), pick(t2, "age2", "std.error"), 6),
  "Total hours worked (per month)",
    "",
    cell(pick(t2, "totalHoursWorked", "estimate"), pick(t2, "totalHoursWorked", "std.error"), 4),
  "Employment type (relab) fixed effects",
    "No", "Yes",
  "Implied peak age",
    fmt(peak_age1), fmt(peak_age2),
  "95\\% bootstrap CI for peak age",
    paste0("[", fmt(ci1[1]), ", ", fmt(ci1[2]), "]"),
    paste0("[", fmt(ci2[1]), ", ", fmt(ci2[2]), "]"),
  "$R^2$",
    fmt(glance(model1)$r.squared, 3), fmt(glance(model2)$r.squared, 3),
  "N",
    format(nobs(model1), big.mark = ","), format(nobs(model2), big.mark = ",")
)

slide_tbl_tex <- slide_tbl |>
  kbl(
    format = "latex", booktabs = TRUE, escape = FALSE, align = "lcc",
    col.names = c("", "Unconditional", "Conditional")
  ) |>
  row_spec(4, extra_latex_after = "\\midrule") |>
  as.character()

writeLines(slide_tbl_tex, "output/tables/age_income_slide_table.tex")


## 7. Gráfico del perfil edad-ingreso

# a. Grilla de edad. Se detiene en 70 y no en el máximo muestral de 91: el
# percentil 99 de la edad es 71, así que más allá de eso la curva se ajusta sobre
# cerca del 1% de las observaciones (16 personas tienen más de 80). Graficar
# hasta 91 le daría un tercio del ancho del gráfico a ese 1% y dejaría que un
# artefacto de extrapolación - la parábola cayendo en picada - dominara la
# imagen.
age_max_plot <- 70
age_grid <- seq(min(clean_data$age), age_max_plot, by = 1)

# b. Perfil incondicional: model1 solo depende de la edad, nada más que fijar.
profile1 <- tibble(age = age_grid, age2 = age_grid^2)
profile1$log_inc_pred <- predict(model1, newdata = profile1)

# c. Perfil condicional: model2 también depende de totalHoursWorked y relab, así
# que los fijamos en un "trabajador de referencia" (horas promedio, categoría de
# relab más frecuente) y variamos solo la edad. Como el modelo no tiene
# interacciones con la edad, esta elección solo desplaza la curva verticalmente:
# no cambia ni su forma ni la edad pico, así que la comparación de abajo es
# robusta a ella.
ref_hours <- mean(clean_data$totalHoursWorked, na.rm = TRUE)
ref_relab <- 1  # Obrero o empleado de empresa particular (categoría más común)

profile2 <- tibble(
  age = age_grid,
  age2 = age_grid^2,
  totalHoursWorked = ref_hours,
  relab = ref_relab
)
profile2$log_inc_pred <- predict(model2, newdata = profile2)

# d. Ambos perfiles en un solo data frame para poder graficarlos lado a lado.
profiles <- bind_rows(
  profile1 |> mutate(model = "Unconditional"),
  profile2 |> mutate(model = "Conditional")
)

# e. Graficar ambas curvas, marcando la edad pico de cada especificación con una
# línea punteada y sombreando el IC percentil bootstrap 95% de esa edad pico
# (ci1/ci2 de la sección 6d, el mismo intervalo reportado en la tabla de
# regresión y en el deck de la Sección 1) como una banda vertical. No se dibuja
# ninguna banda alrededor de las curvas mismas: el enunciado de incertidumbre de
# la sección es sobre la edad pico, no sobre el perfil ajustado. Solo la forma y
# la edad pico son comparables entre curvas: su posición vertical depende del
# trabajador de referencia elegido arriba.
# lab_hjust empuja cada etiqueta de pico lejos de la otra: los dos picos están
# apenas a unos tres años de distancia, así que etiquetas centradas se
# traslaparían.
peaks <- tibble(
  model = c("Unconditional", "Conditional"),
  peak_age = c(unname(peak_age1), unname(peak_age2)),
  ci_lo = c(ci1[1], ci2[1]),
  ci_hi = c(ci1[2], ci2[2])
) |>
  mutate(lab_hjust = if_else(peak_age == min(peak_age), 1.1, -0.1))

# El resultado está en logs, que nadie puede leer de un eje, así que los breaks
# se ubican en montos redondos de pesos (duplicando, el espaciado natural en
# escala log) y se etiquetan en pesos. Las curvas no cambian; solo el eje se
# vuelve legible.
peso_breaks <- c(6e5, 8e5, 1e6, 1.5e6, 2e6, 3e6)
peso_labels <- c("$600K", "$800K", "$1.0M", "$1.5M", "$2.0M", "$3.0M")

# Cada curva se etiqueta sobre sí misma, así que la identidad nunca depende de
# emparejar un color con una leyenda.
series_labels <- profiles |>
  group_by(model) |>
  slice_max(age, n = 1) |>
  ungroup()

y_top <- max(profiles$log_inc_pred)

age_profile_plot <- ggplot(profiles,
                           aes(x = age, y = log_inc_pred, color = model)) +
  geom_rect(
    data = peaks, aes(xmin = ci_lo, xmax = ci_hi, fill = model),
    ymin = -Inf, ymax = Inf, inherit.aes = FALSE, alpha = 0.15
  ) +
  geom_vline(
    data = peaks, aes(xintercept = peak_age, color = model),
    linetype = "dashed", linewidth = 0.5, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.9) +
  geom_text(
    data = peaks,
    aes(x = peak_age, y = y_top, label = sprintf("%.1f", peak_age),
        hjust = lab_hjust),
    vjust = -1.2, size = 3.4, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(
    data = series_labels, aes(label = model),
    hjust = -0.1, size = 3.8, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Specification",
    values = c(Unconditional = "#235da3", Conditional = "#ad0d5d")
  ) +
  scale_fill_manual(
    values = c(Unconditional = "#235da3", Conditional = "#ad0d5d")
  ) +
  scale_y_continuous(breaks = log(peso_breaks), labels = peso_labels,
                     expand = expansion(mult = c(0.05, 0.14))) +
  scale_x_continuous(breaks = seq(20, 70, by = 10)) +
  coord_cartesian(xlim = c(min(age_grid), max(age_grid) + 11), clip = "off") +
  labs(
    title = "Labour income peaks around age 41 to 44 in Bogota",
    x = "Age",
    y = NULL,
    caption = paste0(
      "Dashed lines mark each specification's implied peak age, shaded bands ",
      "its 95% bootstrap CI; vertical axis on a log scale.\n",
      "Only the shape and the peak age are comparable across curves: their ",
      "vertical position depends on the reference\nworker chosen for the ",
      "conditional profile. Ages shown to 70, the sample's 99th percentile.\n",
      "GEIH 2018, Bogota: employed adults 18+ with positive labour income ",
      "(n = 14,763), weighted by fex_c."
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

ggsave("output/figures/age_income_profile.png", age_profile_plot,
       width = 8, height = 5, dpi = 300)
age_profile_plot
