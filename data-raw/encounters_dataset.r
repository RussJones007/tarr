# ------------------------------------------------------------------------------------------------------------------->
# Script: encounters_dataset.r
# Description:
#   Reads and saves the Encounters.csv file and kaes it available in the package
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:  April 25, 2026
# ------------------------------------------------------------------------------------------------------------------->


encounter_descriptions <- readr::read_csv("data-raw/Encounters.csv", col_types = "c", col_select = "Description")
usethis::use_data(encounter_descriptions, internal = FALSE, overwrite = TRUE)
