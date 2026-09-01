###############################################################################
# Project Name:      Predicting Income
# Script Name:       02_Data_Cleaning.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Clean the scraped GEIH 2018 sample: keep employed adults,
#                    select the variables of interest and handle missing values.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data
# 3. Filter and select variables
# 4. Handle missing values
# 5. Save clean data

################################################################################

# Input:  Raw scraped dataframe from 01_Web_Scrapping.r
# Output: Clean analysis dataframe

################################################################################


## 1. Load libraries

library(pacman)

p_load(
  tidyverse # Manipulación de datos.
)
