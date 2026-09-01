###############################################################################
# Project Name:      Predicting Income
# Script Name:       03_Data_Description.r
# Authors:           Maria Jose Perez, Juan Manuel Lozano, Samuel Suárez Valle
# Script Purpose:    Descriptive statistics and exploratory plots for the
#                    clean GEIH 2018 analysis sample.
###############################################################################

# Layout:
# 1. Load libraries
# 2. Load data
# 3. Summary statistics table
# 4. Exploratory plots

################################################################################

# Input:  Clean analysis dataframe from 02_Data_Cleaning.r
# Output: Summary statistics table and figures in output/

################################################################################


## 1. Load libraries

library(pacman)

p_load(
  tidyverse # Manipulación de datos.
)
