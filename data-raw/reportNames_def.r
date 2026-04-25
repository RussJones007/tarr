# ==========================================================
# reportNames_def.r
# Create reportNames list type used internally to classify
# the type of NBS or EpiTrax csv report that is read
# reportNames is an internal variable
# ==========================================================

#library(tarr)

# NOTE: the "NoJurisDiseaseCountsByCountyExample.csv" and "NoJurisExample.csv" look to be the same report,
#       as a result the classifyCSV function in NBSProcess will return "NoJurisExample.csv"

# example report files to load
examples <- list(
  flat               = "FlatExample.csv",
  noJur              = "NoJurisExample.csv",
  Jur                = "WithJurisExample2.csv",
  noJurDiseaseCounts = "NoJurisDiseaseCountsByCountyExample.csv",
  HoustonEnhanced    = "HoustonEnhanced.csv",
  OriginalLineList   = "OriginalLineListExample.csv",
  TriSanoLineList    = "TriSanoLineListExample.csv",
  EpiTraxLineList    = "EpiTraxLineListExample.csv"
)

# reportNames are the different reports with column names for each
reportNames <- purrr::map(.x = examples, .f = function(x) read.csv(paste0("data-raw/",x), header=T, as.is = T))

# Import the NBS with jurisdiction security through the nbs_import function to get thet calculated fields
# Note that the calcuated fields must be accounted for in the classifyCSV function 
reportNames$Jur <- nbs_import(.file = file.path("data-raw", examples$Jur), .include.all = TRUE, .use_cache = FALSE) #|> 

rm(examples)



