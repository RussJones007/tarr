# ============================================================================================
# DocBuild.r
# Script to process documentation and build the package
#
#  Created April 23, 2019
#  R Jones
# ============================================================================================
#install.packages(pkg, repos=NULL, type="source")
#library(utils)

# Load packages ---------------------------------------------------------------------------------------------------
library(usethis)
library(devtools)
library(glue)

print("Installing package")
# usethis::use_build_ignore("DocBuild.r")
# usethis::use_build_ignore("test.r")
# usethis::use_build_ignore("Curves_test.r")
# usethis::use_build_ignore("scripts/test_correct_city_2.r")
# usethis::use_build_ignore("cor2.r")
# use_build_ignore("scripts/")
# usethis::use_mit_license()
# usethis::use_pipe()
# usethis::use_package("testthat")
# use_package("units", min_version = TRUE)
#usethis::edit_r_profile()
# ==== Generate documentation then build the package
# 
usethis::use_version(which = "dev")

# Building documentation ------------------------------------------------------------------------------------------
system.time(document()) # update the documentation
#devtools::check()
#devtools::build_vignettes()  # for use during development
#devtools::install(build_vignettes = TRUE)
pkgName <- build()
#pkgName <- build(binary=TRUE, args = c('--preclean'))  # build a zip file of the package with binaries for installation
pkgName <- build(pkg = pkgName, binary=TRUE)  # build a zip file of the package with binaries for installation
print(paste(pkgName,"being installed"))
detach(name = package:tarr, unload = TRUE)
# Install package -------------------------------------------------------------------------------------------------
install.packages(pkgs = pkgName, repos = NULL, type = "binary") # install the package
rm(list = ls())
print("Restarting R")
.rs.restartR()
