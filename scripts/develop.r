

#library(perplexR)
library(tidyverse)
#library(maptiles)
library(microbenchmark)
#library(tarr)
#library(sf)
#library(nanoparquet)
#library(tarr)
#library(ivs)
#packageDescription("tarr")
#news(package = "arrow")
#map_lgl(paths, file.exists)

?tarr
?population
?convert_type
convert_type()
?tarr.pop::population
?scan_cluster
str_detect(NULL, "hi")
paths
?case_interview_classifier
?make_classifier
help(package = "tarr")
load_all()
tools::buildVignettes(dir = ".")
# test case_interview_classifier
# get notes to determoine if case or proxy was interviews
encounters <- map(1:11, 
                  ~readxl::read_xlsx(path = "G:/Data/Metrics/Encounter work/Encounter_review.xlsx", .x) 
) |>
  rep(30) |> 
  bind_rows()

res <- case_interview_classifier(x = encounters$description, return_detail = TRUE)
attempts <- case_interview_classifier(x = encounters$description, return_detail = TRUE, 
                                      extra_true = c(r"(\blvm\b)", 
                                                     r"(\b(?:left/s)?voicemail\b)",
                                                     r"(\bleft voice message\b)",
                                                     r"(phone (?:has )?(?:been )?disconnected)",
                                                     r"(\bto reach\b)",
                                                     r"(\bphone rang\b)",
                                                     r"(\bContact attempt for (?:patient|pt|guardian|mother|mom|father)?\b)"
                                      )) |> 
  mutate(classification = ifelse(!is.na(matched_true_pattern) & !is.na(matched_false_pattern), TRUE, classification))
attempts$classification |> table(useNA = "always")
res$classification |> table(useNA = "always")
compare <- bind_cols(encounters, res)

patterns <- classifier_patterns(case_interview_classifier)

