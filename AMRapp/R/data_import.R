#' Imports the data in the form of the AMRapp template
#'
#' This function imports the data from a pre-prepared CSV file
#' @param location The directory and file name of the file for import
#' @return The imported data
#' @export
#' @examples
#' AMR_data <- data_import("Amrapp_template_testdata.csv")



data_import <- function(location){
   address <- paste0(location)
  x <- data.table::fread(address, select = c("genus"="character", "species"="character",
                                 "Date"="character", "antimicrobial"="character",
                                 "result"="character", "age"="integer",
                                 "sex"="character", "pt_id"="character"))
 return(x)
}
