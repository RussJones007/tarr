# pop_doc_dummy.R
# This script creates a dummy data file for external data that is used as a target for the
# pop_doc.r script to document.   This get's around the problem of the population data frame being documented
# directly and causing package build times to take 45 minutes instead od the usual 2 minute.

system.file("extdata", "population.rda", package = "tarr") |> load()

population_dummy <- map(population, \(df) df[0,])
rm(population)
