#' Reshape and get binomial confidence intervals
#'
#' This function take the prepared imported data (from data_prep())
#' then reshapes the data for use within the line graph functions,
#' and generates the confidence intervals.
#'
#' @param data The data frame generated from using data_prep()
#' @param conf The degree to which the Wilson confidence intervals
#' should be calculated, for 95% CI 0.05 should be entered
#' @return The reshaped data file with confidence intervals
#' @examples
#' citable <- bin_CI(data=new_AMRseries, conf=0.05)
#' @export
bin_CI <- function(data, conf) {
  d <- spread(data = data, result, n) %>%
    mutate(across(where(is.numeric), ~replace(.x, is.na(.),0)))  %>%
    do(data.frame(., Res = Hmisc::binconf(x=as.vector(.$R),
                                          n=as.vector(.$R+.$I+.$S),
                                          alpha = conf,
                                          include.x = TRUE,
                                          include.n = TRUE,
                                          return.df = TRUE,
                                          method = "wilson"))) %>%
    do(data.frame(., Test = Hmisc::binconf(x=as.vector(.$R+.$I+.$S),
                                           n=as.vector(.$R+.$I+.$S+.$N),
                                           alpha = conf,
                                           include.x = TRUE,
                                           include.n = TRUE,
                                           return.df = TRUE,
                                           method = "wilson")))

  return(d)
}
