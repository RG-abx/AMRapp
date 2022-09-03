#' Plots the percentage resistance line trend, without CI
#'
#' This function creates the percentage resistance line graph
#'
#' @param data The data generated from the bin_CI() function
#' @param filter1 The genus selected to plot in string format "genus"
#' @param filter2 The antimicrobial selected to plot in string format "antimicrobial"
#' @return The graph object
#' @examples
#' reslinegraph <- percentR_plot(citable, "Klebsiella", "CIP")
#' @export


pcentR_plot <- function(data, filter1, filter2) {

  dt <- filter(data, genus==filter1, antimicrobial==filter2)
  data2 <- xts::xts(x=cbind(dt$Res.PointEst*100), order.by = dt$year_month)

  m <- max(data2, na.rm=T)
  m2 <- m+5
  dygraphs::dygraph(data2, main = "Percentage resistance") %>%
    dygraphs::dySeries(c("V1"), label = "% resistant") %>%
    dygraphs::dyAxis("x", drawGrid = FALSE, label = "Month and Year") %>%
    dygraphs::dyAxis("y", valueRange = c(0, m2), label = "% resistant") %>%
    dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
    dygraphs::dyRangeSelector()

}
