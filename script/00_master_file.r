###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    00_master_file.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Propósito del script: Ejecuta todo el pipeline (raspado, limpieza, descripción
#                       y Secciones 1-3) con una sola llamada.
###############################################################################

# Estructura:
# 1. Configuración
# 2. Directorio de trabajo y carpetas de salida
# 3. Definición del pipeline
# 4. Ejecutor de pasos
# 5. Ejecución

################################################################################

# Uso (correr desde la raíz del repo, la carpeta "Problem Set 1 - Predicting Income/"):
#
#   Rscript script/00_master_file.r            # corre todos los pasos
#   Rscript script/00_master_file.r 04 05      # corre solo los pasos 04 y 05
#
# El paso 01 siempre vuelve a raspar las páginas de la GEIH y sobrescribe data/geih_scrap.rds.

################################################################################


## 1. Configuración

# Args opcionales: números de paso ("01", "4", ...) para correr solo algunos de los pasos.
# Si no se pasa nada, corremos todo.
requested_steps <- commandArgs(trailingOnly = TRUE)

# Cada script carga sus propios paquetes con pacman, pero pacman mismo tiene que
# instalarse primero.
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}


## 2. Directorio de trabajo y carpetas de salida

# Todos los scripts usan rutas relativas a la raíz del repo ("data/...", "output/...").
if (!file.exists("script/01_web_scrapping.r")) {
  stop(
    "Run this from the repo root:  Rscript script/00_master_file.r",
    call. = FALSE
  )
}

for (d in c("data", "output/tables", "output/figures", "output/models")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


## 3. Definición del pipeline

# id      -> prefijo numérico del script (se usa para elegir un subconjunto de pasos)
# script  -> ruta desde la raíz del repo
# label   -> descripción impresa en la consola
pipeline <- list(
  list(
    id = "01", script = "script/01_web_scrapping.r",
    label = "Web scraping (GEIH 2018 sample -> data/geih_scrap.rds)"
  ),
  list(
    id = "02", script = "script/02_data_cleaning.r",
    label = "Data cleaning (-> data/geih_clean.rds, balance table)"
  ),
  list(
    id = "03", script = "script/03_data_description.r",
    label = "Data description (descriptive tables and figures)"
  ),
  list(
    id = "04", script = "script/04_age_labor_income.r",
    label = "Section 1: age-income profile (bootstrap peak age)"
  ),
  list(
    id = "05", script = "script/05_gender_gap.r",
    label = "Section 2: gender income gap (FWL, bootstrap SE)"
  ),
  list(
    id = "06", script = "script/06_income_prediction_train.r",
    label = "Section 3: train Section 1/2 models on subset 1-7"
  ),
  list(
    id = "07", script = "script/07_income_prediction_validation.r",
    label = "Section 3: validation RMSE, LOOCV, variable importance"
  ),
  list(
    id = "08", script = "script/08_income_imputation_comparison.r",
    label = "Section 3: complete-case vs. PMM-imputed comparison"
  )
)


## 4. Ejecutor de pasos

# Convierte un token de paso ("4", "04", "step04") en su id de dos dígitos.
norm_id <- function(x) sprintf("%02d", as.integer(gsub("\\D", "", x)))

selected_ids <- if (length(requested_steps) > 0) {
  vapply(requested_steps, norm_id, character(1))
} else {
  vapply(pipeline, function(s) s$id, character(1))
}

# Cada script lee de disco el .rds que necesita, así que lo sourceamos en su
# propio entorno para que los nombres no se filtren entre pasos.
run_step <- function(step) {
  bar <- strrep("=", 74)
  message("\n", bar)
  message(sprintf(
    "[%s]  Step %s  %s", format(Sys.time(), "%H:%M:%S"), step$id, step$label
  ))
  message(bar)

  t0 <- Sys.time()
  ok <- tryCatch({
    source(step$script, local = new.env(parent = globalenv()), echo = FALSE)
    TRUE
  }, error = function(e) {
    message("\n*** Step ", step$id, " FAILED: ", conditionMessage(e))
    FALSE
  })
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  if (!ok) {
    stop(
      sprintf("Pipeline stopped at step %s after %s s.", step$id, dt),
      call. = FALSE
    )
  }
  message(sprintf("--- step %s done (%s s)", step$id, dt))
}


## 5. Ejecución

t_start <- Sys.time()
ran <- character(0)

for (step in pipeline) {
  if (!step$id %in% selected_ids) next

  run_step(step)
  ran <- c(ran, step$id)
}

total <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 2)
message("\n", strrep("=", 74))
message(sprintf("Pipeline finished in %s min.", total))
message("  ran:     ", if (length(ran)) paste(ran, collapse = ", ") else "(none)")
message(strrep("=", 74))

##############################Fin del script####################################
