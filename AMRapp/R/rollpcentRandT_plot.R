#' Plots the rolling average percentage resistance and tested line trend
#'
#' This function creates the rolling average percentage resistance and
#' percentage tested line graph
#'
#' @param data The data generated from the roll_av() function
#' @param filter1 The genus selected to plot in string format "genus"
#' @param filter2 The antimicrobial selected to plot in string format "antimicrobial"
#' @return The graph object
#' @examples
#' Roll_av_lineGraph  <- rollpcentRandT_plot(roll_av_data, "Klebsiella", "CIP")
#' @export


rollpcentRandT_plot <- function(data, filter1, filter2) {
  # requires data to have been pre-formatted using the bin_CI() function
  dt <- filter(data, genus==filter1, antimicrobial==filter2)
  data5 <- xts::xts(x=cbind(dt$avpcentR*100, dt$avpcentT*100), order.by = dt$year_month)

  m <- max(data5, na.rm=T)
  m2 <- m+5
  dygraphs::dygraph(data5, main = "Percentage resistance and precentage tested (rolling average)") %>%
    dygraphs::dySeries("V1", label = "% resistant") %>%
    dygraphs::dySeries("V2", label = "% tested", axis = 'y2') %>%
    dygraphs::dyAxis("x", drawGrid = FALSE, label = "Time") %>%
    dygraphs::dyAxis("y", valueRange = c(0, m2), label = "% resistant") %>%
    dygraphs::dyAxis("y2", valueRange = c(0, m2), label = "% tested") %>%
    dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
    dygraphs::dyRangeSelector()

}
