#' Plots the incidence for each susceptibility result
#'
#' This function creates stacked bar plot of incidence for each susceptibility
#' result over time - for the selected genus and antimicrobial
#'
#'
#' @param data The data generated from the data_import() function
#' @param filter1 The genus selected to plot in string format "genus"
#' @param filter2 The antimicrobial selected to plot in string format "antimicrobial"
#' @return The graph object
#' @examples
#' Stackgraph  <- stack_plot_function(AMR_data, "Klebsiella", "CIP")
#' @export


stack_plot_function <- function(data, filter1, filter2) {
  # the stack_plot_function takes in the data object and two filters.
  # The first filter is the genus of the organism being plotted.
  # The second filter is the antimicrobial selected
  # This function is for a single antibiotic plot, looking at counts by year and month.
  # What is plotted is a stacked bar graph of the susceptibility results (N, R, I, S)
  # Output is an interactive plotly graph, where selection of each result category
  # can remove it from the plot, or put it back.
  # The hover-over-text shows the month and year, and the total number of reports
  # in that result category

  # data must contain Date, result, genus and antimicrobial in the application requested format and structure
  stacked_bar <- data %>%
    ## changing some of the imported data structure
    #    mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
    #           result = as.factor(result),
    #           month = month(Date),
    #           year = year(Date),
    #           year_month = make_date(year = year, month = month )) %>%
    ## filtering the genus and antimicrobial selected
    filter(genus == filter1 & antimicrobial == filter2) %>%
    ## grouping into year and month and susceptibility result
    group_by(result=as.factor(result),
             year_month=lubridate::make_date(year=year(as.Date(Date, format = "%d/%m/%Y")),
                                  month=month(as.Date(Date, format = "%d/%m/%Y")))) %>%
    ## calculating a total of each category by month
    summarise( n=n_distinct(pt_id), .groups = "keep") %>%
    #    set_value_labels(result = c(Resistant = "R", Susceptible = "S", "Not tested" = "N", Intermediate = "I")) %>%
    ## generating the plot and assigning the colour based on the result), s2 = c(Yes = 1, No = 2)) %>>%
    #    val_labels(data) %>%
    ## generating the plot and assigning the colour based on the result
    plotly::plot_ly(x=~year_month,
            y=~n,
            color = ~result,
            text = ~paste("No. of cases=",n, "<br>", "Month=", year_month, "<br>", "Result=", result),
            hoverinfo = "text"
    ) %>%
    add_bars() %>%
    layout(title=paste("No. of", filter2, "susceptiiblity test results for", filter1, "spp."),
           xaxis = list(title="Year and Month"),
           yaxis = list(title = "No. testss"),
           barmode = "stack")
}
