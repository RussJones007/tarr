# Explore interval operations
# how to take a desired age group vector and summarise another vector
library(ivs)
library(stringr)

base_grp <- c(" 0-4", "5-9", "10-14", "15-19" , "20-24", "25-30", "35-39", "40-44", "45-49", "50-54", "55-59", "60 +")
test_grp <- c("< 4", "1 - 14", "16 to 19") 

base <- new_age_group(base_grp)
base
attributes(base)
class(base)
base
base |> unclass()

tmp <- new_age_group(test_grp)
tmp
tmp |> as.factor()
factor(tmp)
str(tmp)
tmp[2] |> class()

?iv_restore
methods(class = "ivs_iv")
