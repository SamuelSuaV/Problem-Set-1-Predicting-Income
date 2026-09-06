###############################################################################
# Project Name:      Predicting Income
# Script Name:       04_Age_Labor_Income.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Calculate age labour income profile regressions along with
#                    boot-strap CI using different data sets.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data
# ...

################################################################################

# Input:  Raw scraped dataframe from 01_Web_Scrapping.r
# Output: Clean analysis dataframe

################################################################################


## 1. Load libraries
#install.packages("pacman")
library(pacman)
p_load(
    tidyverse,
    boot,
    broom,
    kableExtra)

clean_data <- readRDS("data/geih_clean.rds")

clean_data <- clean_data |>
    filter(y_total_m > 0) |>
    mutate(age2 = age^2, log_inc = log(y_total_m))  #creamos una variable de logaritmo del ingreso para poder hacer la regresión

# Etiquetas de relab (duplicadas del 02_Data_Cleaning.r: cada script se corre
# independiente, asi que no podemos reusar el vector que definieron alla).
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

# modelo 1, sin condicionar
model1 <- lm(log_inc ~ age + age2, data = clean_data)
summary(model1) # revisamos el resumen del modelo


#model 2, condicionado 
model2 <- lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab), data = clean_data)
summary(model2) # revisamos el resumen del modelo


#edad pico 
##*model 1
peak_age1 <- -coef(model1)["age"] / (2 * coef(model1)["age2"])

##*model 2
peak_age2 <- -coef(model2)["age"] / (2 * coef(model2)["age2"])


B <- 3000 # number of bootstrap samples

#bootstrap con el paquete boot, para model 1
peak_age_stat1 <- function(data, index) {
  model <- lm(log_inc ~ age + age2, data = data[index, ])
  -coef(model)["age"] / (2 * coef(model)["age2"])
}

set.seed(123) # for reproducibility
boot_peak_age1 <- boot(clean_data, peak_age_stat1, R = B)
boot_peak_age1          # original, bias y std. error bootstrap
sd(boot_peak_age1$t)    # sd bootstrap (equivalente al "std. error" de arriba)
boot.ci(boot_peak_age1, type = "perc")  # IC 95%


#bootstrap con el paquete boot, para model 2
peak_age_stat2 <- function(data, index) {
  model <- lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab), data = data[index, ])
  -coef(model)["age"] / (2 * coef(model)["age2"])
}

set.seed(123) # for reproducibility
boot_peak_age2 <- boot(clean_data, peak_age_stat2, R = B)
boot_peak_age2
sd(boot_peak_age2$t)
boot.ci(boot_peak_age2, type = "perc")  # IC 95%


table_regression <- full_join(
  tidy(model1) |> select(term, estimate, std.error),
  tidy(model2) |> select(term, estimate, std.error),
  by = "term",
  suffix = c("_model1", "_model2")
)

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

# Exportar la tabla a LaTeX, mismo patron que 02_Data_Cleaning.r
# (kbl con booktabs, forzando [H] en vez del [!h] de kableExtra para que
# la tabla no flote fuera de su seccion en el documento final).
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
    # NOTA: con digits = 4 el SE de age2 sale como 0.0000 (el valor real es
    # ~0.0000376, muy chico para 4 decimales) - revisar antes de usar en las
    # slides (mas decimales solo para esa fila, o notacion cientifica).
    format = "latex", booktabs = TRUE, digits = 4,
    caption = "Age-income profile: unconditional vs. conditional",
    label = "age_income"
  ) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  as.character() |>
  force_float_h()

writeLines(table_regression_tex, "output/tables/age_income_regression.tex")


## Visualizacion de los perfiles edad-ingreso

# Grilla de edades sobre el rango observado en la muestra.
age_grid <- seq(min(clean_data$age), max(clean_data$age), by = 1)

# Perfil incondicional: model1 solo depende de age, no hay nada mas que fijar.
profile1 <- tibble(age = age_grid, age2 = age_grid^2)
profile1$log_inc_pred <- predict(model1, newdata = profile1)

# Perfil condicional: model2 tambien depende de totalHoursWorked y relab, asi
# que los fijamos en un "trabajador de referencia" (horas promedio, categoria
# de relab mas frecuente) y solo variamos la edad. Como el modelo no tiene
# interacciones con age, esta eleccion solo desplaza la curva verticalmente:
# no cambia ni su forma ni la edad pico.
ref_hours <- mean(clean_data$totalHoursWorked, na.rm = TRUE)
ref_relab <- 1  # Obrero o empleado de empresa particular (categoria mas comun)

profile2 <- tibble(
  age = age_grid,
  age2 = age_grid^2,
  totalHoursWorked = ref_hours,
  relab = ref_relab
)
profile2$log_inc_pred <- predict(model2, newdata = profile2)

# Juntamos los dos perfiles para graficarlos comparados.
profiles <- bind_rows(
  profile1 |> mutate(model = "Unconditional"),
  profile2 |> mutate(model = "Conditional")
)

peaks <- tibble(
  model = c("Unconditional", "Conditional"),
  peak_age = c(unname(peak_age1), unname(peak_age2))
)

age_profile_plot <- ggplot(profiles, aes(x = age, y = log_inc_pred, color = model)) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = peaks, aes(xintercept = peak_age, color = model),
    linetype = "dashed", show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Specification",
    values = c(Unconditional = "#6c0a8a", Conditional = "#4daad5")
  ) +
  labs(
    title = "Age-income profile: unconditional vs. conditional",
    subtitle = "Dashed lines mark the implied peak age of each specification",
    x = "Age",
    y = "Predicted log(total monthly income)"
  ) +
  theme_minimal()

ggsave("output/figures/age_income_profile.png", age_profile_plot, width = 8, height = 5)
age_profile_plot
