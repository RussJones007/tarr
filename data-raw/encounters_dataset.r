# ------------------------------------------------------------------------------------------------------------------->
# Script: encounters_dataset.r
# Description:
#   Reads and saves the Encounters.csv file and kaes it available in the package
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:  April 25, 2026
# ------------------------------------------------------------------------------------------------------------------->


encounter_descriptions <- readr::read_csv("data-raw/Encounters.csv", 
                                          col_types = c("cc"),
                                          col_select = c("Description","attempt_class") 
) |> 
  dplyr::mutate(attempt_class = factor(attempt_class))
usethis::use_data(encounter_descriptions, internal = FALSE, overwrite = TRUE)
