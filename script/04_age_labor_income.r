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
    boot)

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


#bootstrap function for model 1
set.seed(123) # for reproducibility
B <- 3000 # number of bootstrap samples
bootstrap_peak_age1 <- rep(NA,B)

for(i in 1:B){
  sample_data <- clean_data |> sample_frac(1, replace = TRUE) # bootstrap sample
    model <- lm(log_inc ~ age + age2, data = sample_data) # fit model
    bootstrap_peak_age1[i] <- -coef(model)["age"] / (2 * coef(model)["age2"]) # calculate peak age
  }


mean(bootstrap_peak_age1) #mean  = 40.61685
sd(bootstrap_peak_age1) #sd bootstrap 0.2743336
quantile(bootstrap_peak_age1, c(0.025, 0.975))  # IC 95%


#bootstrap function for model 2
set.seed(123) # for reproducibility
bootstrap_peak_age2 <- rep(NA,B)

for(i in 1:B){
  sample_data <- clean_data |> sample_frac(1, replace = TRUE) # bootstrap sample
    model <- lm(log_inc ~ age + age2 + totalHoursWorked + factor(relab), data = sample_data) # fit model
    bootstrap_peak_age2[i] <- -coef(model)["age"] / (2 * coef(model)["age2"]) # calculate peak age
  }


mean(bootstrap_peak_age2)
sd(bootstrap_peak_age2)
quantile(bootstrap_peak_age2, c(0.025, 0.975))  # IC 95%



