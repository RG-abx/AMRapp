#' Plots the percentage tested line trend, with CI
#'
#' This function creates the percentage tested line graph with
#' confidence intervals (area)
#'
#' @param data The data generated from the bin_CI() function
#' @param filter1 The genus selected to plot in string format "genus"
#' @param filter2 The antimicrobial selected to plot in string format "antimicrobial"
#' @return The graph object
#' @examples
#' Testlinegraph <- pcentT_ci_plot(citable, "Klebsiella", "CIP")
#' @export


pcentT_ci_plot <- function(data, filter1, filter2) {

  dt <- filter(data, genus==filter1, antimicrobial==filter2)
  data3 <- xts::xts(x=cbind(dt$Test.PointEst*100, dt$Test.Upper*100, dt$Test.Lower*100), order.by = dt$year_month)

  a <- max(data3, na.rm=T)
  a <- a+5
  dygraphs::dygraph(data3, main = "Percentage tested") %>%
    dygraphs::dySeries(c("V3", "V1", "V2"), label = "% tested") %>%
    dygraphs::dyAxis("x", drawGrid = FALSE, label = "Time" ) %>%
    dygraphs::dyAxis("y", valueRange = c(0, a), label = "% tested") %>%
    dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
    dygraphs::dyRangeSelector()

}
