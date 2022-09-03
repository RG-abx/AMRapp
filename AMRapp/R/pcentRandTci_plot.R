#' Plots the percentage resistance and tested line trend with CI
#'
#' This function creates the percentage resistance and
#' percentage tested line graph with confidence intervals
#'
#' @param data The data generated from the bin_CI() function
#' @param filter1 The genus selected to plot in string format "genus"
#' @param filter2 The antimicrobial selected to plot in string format "antimicrobial"
#' @return The graph object
#' @examples
#' ResandTeslinegraph_CI  <- pcentRandTci_plot(citable, "Klebsiella", "CIP")
#' @export

pcentRandTci_plot <- function(data, filter1, filter2) {

  dt <- filter(data, genus==filter1, antimicrobial==filter2)
  data2 <- xts::xts(x=cbind(dt$Res.PointEst*100, dt$Res.Upper*100, dt$Res.Lower*100, dt$Test.PointEst*100, dt$Test.Upper*100, dt$Test.Lower*100), order.by = dt$year_month)

  m <- max(data2, na.rm=T)
  m2 <- m+5
  dygraphs::dygraph(data2, main = "Percentage resistance and precentage tested") %>%
    dygraphs::dySeries(c("V2", "V1", "V3"), label = "% resistant") %>%
    dygraphs::dySeries(c("V5", "V4", "V6"), label = "% tested", axis = 'y2') %>%
    dygraphs::dyAxis("x", drawGrid = FALSE, label = "Time") %>%
    dygraphs::dyAxis("y", valueRange = c(0, m2), label = "% resistant") %>%
    dygraphs::dyAxis("y2", valueRange = c(0, m2), label = "% tested") %>%
    dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
    dygraphs::dyRangeSelector()

}
