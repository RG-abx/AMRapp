#' Plots the percentage tested line trend, without CI
#'
#' This function creates the percentage tested line graph
#'
#' @param data The data generated from the bin_CI() function
#' @param filter1 The genus selected to plot in string format "genus"
#' @param filter2 The antimicrobial selected to plot in string format "antimicrobial"
#' @return The graph object
#' @examples
#' Testlinegraph_noCI <- pcentT_plot(citable, "Klebsiella", "CIP")
#' @export


pcentT_plot <- function(data, filter1, filter2) {

  dt <- filter(data, genus==filter1, antimicrobial==filter2)
  data3 <- xts::xts(x=cbind(dt$Test.PointEst*100), order.by = dt$year_month)

  a <- max(data3, na.rm=T)
  a <- a+5
  dygraphs::dygraph(data3, main = "Percentage tested") %>%
    dygraphs::dySeries(c("V1"), label = "% tested") %>%
    dygraphs::dyAxis("x", drawGrid = FALSE, label = "Month and Year" ) %>%
    dygraphs::dyAxis("y", valueRange = c(0, a), label = "% tested") %>%
    dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
    dygraphs::dyRangeSelector()

}
