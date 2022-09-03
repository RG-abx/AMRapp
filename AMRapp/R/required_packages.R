#' Checks, installs and loads required packages
#'
#' This function checks currently installed and loaded packages
#' and loads those required that are not currently loaded
#'
#' @param NULL No parameter is required
#' @return The loaded packages
#' @examples
#' required_packages()
#' @export


required_packages <- function() {
  rm(list = ls())

packagesReqd <- c("dplyr",
                  "tidyr",
                  "ggplot2",
                  "readxl",
                  "devtools",
                  "R.utils",
                  "lubridate",
                  "shiny",
                  "shinyjs",
                  "shinydashboard",
                  "ellipsis",
                  "plotly",
                  "DT",
                  "caTools",
                  "data.table",
                  "datamods",
                  "shinythemes",
                  "dygraphs",
                  "xts",
                  "Hmisc",
                  "RColorBrewer")
### Checking the required packages against those that are already installed
new.packages <- packagesReqd[!(packagesReqd %in% installed.packages()[,"Package"])]
### Installing any new packages required
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packagesReqd, library, character.only = T, warn.conflicts = F, quietly = T))

### Cleaning up the environment
rm(new.packages)
rm(packagesReqd)
}

