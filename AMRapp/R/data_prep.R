#' Converting and grouping imported data
#'
#'This function takes the input file and converts dates into date format
#' and created a result factor. Data are then summarised in to
#' year and month totals at the pt_id level for genus and antimicrobial
#'
#' @param data The data imported using data_import().
#' @return The summarised data
#' @examples
#' new_AMRseries <- data_prep(data=AMR_data)
#' @export

data_prep <- function(data) {
  new_series <- data %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
           result = as.factor(result),
           month = lubridate::month(Date),
           year = lubridate::year(Date),
           ym2 = as.Date(Date, "%d/%m/%Y", by="month"),
           year_month = lubridate::make_date(year = year, month = month )) %>%
    group_by(year_month, result, genus, antimicrobial) %>%
    summarise(n=n_distinct(pt_id), .groups = "keep")

}
