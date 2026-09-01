###############################################################################
# Project Name:      Predicting Income
# Script Name:       01_Web_Scrapping.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    This script scrapes income and covariables from
#                    the GEIH, from the website:
#                    https://ignaciomsarmiento.github.io/GEIH2018_sample/
###############################################################################

# Layout:
# 1. Load libraries
# 2. Scrape function
# 3. Scrapping
# 4. Organize and save data

################################################################################

# Input: GEIH 2018 from https://ignaciomsarmiento.github.io/GEIH2018_sample/
# Output: Dataframe with income and covariables. The dataframe has a subset var
#         which identifies which subest a unit belongs to.

################################################################################


## 1. Load libraries

library(pacman)

p_load(
  tidyverse, # Manipulación de datos.
  rvest,     # Web scraping.
  httr,      # Solicitud de datos para páginas dinámicas.
  jsonlite   # Lectura de datos en formato .json.
)


## 2. Scrape function

income_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/"
# Note: The webpage has no robots.txt.

scrape_function <- function(i) {
  ###
  # Reads the HTML content of a GEIH webpage, extracts the table it
  # contains and tags every row with the subset it came from.

  # Input:  i: page number (1-10) to scrape. # nolint: indentation_linter.
  # Output: dataframe with income and covariables, as well as a subset
  #         variable that identifies which subset a unit belongs to.
  ###

  url <- paste0(income_url, "geih_page_", i, ".html")

  # Descargamos con httr::GET + timeout: read_html(url) hace su propia
  # descarga SIN límite de tiempo y ahí es donde R se quedaba colgado.
  table <- httr::GET(url, httr::timeout(30)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    rvest::read_html() |>
    rvest::html_element("table") |>
    rvest::html_table()

  # La variable i identifica el subset de esta página.
  table$subset <- i

  Sys.sleep(0.5)

  table
}


## 3. Scrapping
 
# Recorremos las 10 páginas y las apilamos en un for simple.
geih_completa <- data.frame()
for (i in 1:10) {
  message("Scraping page ", i)
  geih_completa <- rbind(geih_completa, scrape_function(i))
}

## 4. Organize and save data

# Quitamos las columnas de índice de fila que arrastra el scraping
geih_scrap <- geih_completa %>%
  select(-"...1")

saveRDS(geih_scrap, file = "data/geih_scrap.rds")
