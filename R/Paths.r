# =====================================================================================================================
# Paths.r
#
#  Script to set up data paths.
# Makes moving data files to a new location easier as all paths for scripts
# are found here
#
# R Jones
# Revised July 30, 2018
# =====================================================================================================================


#' #' paths_defined
#' Define often used paths to folders.  The named list contains the paths to
#' scripts, data folders, etc.
#'
#' @param .system defaults to paths for the system being run.   
#'
#' @return A named list of important paths to access scripts and data folders.
#' @keywords internal
paths_defined <- function(.system = Sys.info()['sysname'] ){
  paths_lists <-   pths <- list(
    "Windows" = list(
      data            = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Communicable Disease/Current Year",  # change to the folder with the data
      communicable    = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Communicable Disease", # parent data folder for most of the other folders
      scripts         = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Communicable Disease/R Scripts",
      cities          = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/GIS/Shape Files/Tarrant/Cities",  # Shape file for cities folder
      spatial         = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/GIS/Shape Files",
      monthReportCode = "//ITPNAS1.tarrantcounty.com/PublicHealth/DiseaseControl/EPI/Monthly Report/Code",
      monthReport     = "//ITPNAS1.tarrantcounty.com/PublicHealth/DiseaseControl/EPI/Monthly Report",
      population      = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Population",
      arbovirus       = "//ITPNAS1.tarrantcounty.com/PublicHealth/DiseaseControl/EPI/Arboviral Diseases",
      immunization    = "//ITPNAS1.tarrantcounty.com/PublicHealth/DiseaseControl/EPI/Immunization",
      #user           = paste(Sys.getenv("USERPROFILE"),"Documents",sep="\\"),  # must be set at run time!!
      flu             = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Influenza"
    ),
    # The linux paths are for a personal computer where code development is used, but no data is available.
    "Linux" = list(
        #data            = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Commmunicable Disease/Current Year",  # change to the folder with the data
        #communicable    = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Commmunicable Disease", # parent data folder for most of the other folders
        #scripts         = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Data/Commmunicable Disease/R Scripts",
        cities          = "/home/russ/R/Projects/GIS/Shape Files/Tarrant/Cities",  # Shape file for cities folder
        spatial         = "/home/russ/R/Projects/GIS/Shape Files",
        monthReportCode = "/home/russ/R/Projects/Comm Disease/Monthly Report/Code",
        #monthReport     = "/home/russ/R/Projects/Comm Disease/Monthly Report",
        population      = "/home/russ/R/Projects/Population",
        #arbovirus       = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Arboviral Diseases",
        immunization    = "/home/russ/R/Projects/Immunizations"
        #user           = paste(Sys.getenv("USERPROFILE"),"Documents",sep="\\"),  # must be set at run time!!
        #flu             = "//ITPNAS1.tarrantcounty.com/PublicHealth/Shared/EPI/Influenza"
    )
    
    
  )
  return(paths_lists[[.system]])
}

#' paths
#' 
#' a variable containing a named list of shared drive folders containing data and scripts.
#' @format a named list  vector of paths including 
#'\itemize{
#'  \item data: communicable disease data for the current year
#'  \item communicable: parent data folder for most of the other folders
#'  \item scripts:  R Scripts",
#'  \item cities: Shape file for cities folder
#'  \item spatial: General shape files folder
#'  \item monthReportCode : Scripts used for monthly report
#'  \item monthReport: Location of monthly report
#'  \item population: Population data folder
#'  \item arbovirus: Arboviral data folders
#'  \item immunization: Immunization data files
#'  \item user:  Path to the current users documents folder
#'  \item flu : Flu data and report folders
#'}

"paths"
