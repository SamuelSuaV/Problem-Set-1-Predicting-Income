###############################################################################
# Nombre del proyecto:  Predicting Income
# Nombre del script:    01_web_scrapping.r
# Autores:              Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez
# Propósito del script: Este script raspa el ingreso y las covariables de
#                       la GEIH, desde el sitio web:
#                       https://ignaciomsarmiento.github.io/GEIH2018_sample/
###############################################################################

# Estructura:
# 1. Cargar librerías
# 2. Función de raspado
# 3. Raspado
# 4. Organizar y guardar los datos

################################################################################

# Input: GEIH 2018 desde https://ignaciomsarmiento.github.io/GEIH2018_sample/
# Output: Dataframe con el ingreso y las covariables. El dataframe tiene una
#         variable subset que identifica a qué subset pertenece cada unidad.

################################################################################


## 1. Cargar librerías

library(pacman)

p_load(
  tidyverse, # Manipulación de datos.
  rvest,     # Web scraping.
  httr,      # Solicitud de datos para páginas dinámicas.
  jsonlite   # Lectura de datos en formato .json.
)


## 2. Función de raspado
income_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/"
# Nota: la página web no tiene robots.txt.

scrape_function <- function(i) {
  ###
  # Lee el contenido HTML de una página web de la GEIH, extrae la tabla que
  # contiene y etiqueta cada fila con el subset del que proviene.

  # Input:  i: número de página (1-10) a raspar. # nolint: indentation_linter.
  # Output: dataframe con el ingreso y las covariables, además de una variable
  #         subset que identifica a qué subset pertenece cada unidad.
  ###

  url <- paste0(income_url, "geih_page_", i, ".html")

  table <- httr::GET(url, httr::timeout(30)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    rvest::read_html() |>
    rvest::html_element("table") |>
    rvest::html_table()

  # La primera columna es el índice de fila de la tabla HTML y viene sin
  # nombre; la quitamos aquí.
  table <- table[, names(table) != "", drop = FALSE]

  # La variable i identifica el subset de esta página.
  table$subset <- i

  Sys.sleep(5)

  table
}


## 3. Raspado
# Recorremos las 10 páginas y las juntamos en un for simple.
geih_scrap <- data.frame()
for (i in 1:10) {
  message("Scraping page ", i)
  geih_scrap <- rbind(geih_scrap, scrape_function(i))
}

## 4. Guardar los datos
saveRDS(geih_scrap, file = "data/geih_scrap.rds")

##############################Fin del script####################################
