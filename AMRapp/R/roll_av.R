#' Rolling average table generating
#'
#' This function generates the rolling average values using the data generated
#' from bin_CI()
#'
#' @param data This the data produced by bin_CI()
#' @param k This the the lagged number of months to generate the rolling average from
#' @return The dataframe with the rolling average percentages
#' @examples
#' roll_av_data <- roll_av(citable, 3)
#' @export


 roll_av <- function(data, k) {
 # will want to change the value of k and the alignment based on user input - k is currently set to 3 month lag
 # This adds in 3 month rolling average percent Resistant and percent Test
   df <- dplyr::spread(data = data, result, n) %>%
     mutate(across(where(is.numeric), ~replace(.x, is.na(.),0)))  %>%
   do(data.frame(., avpcentR = caTools::runmean(x=.$Res.PointEst,
                                     k = k,
                                     alg = "fast",
                                     endrule = "mean",
                                     align = "left"))) %>%
   do(data.frame(., avpcentT = caTools::runmean(x=.$Test.PointEst,
                                       k = k,
                                       alg = "fast",
                                       endrule = "mean",
                                       align = "left")))
   return(df)
 }
