% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roll_av.R
\name{roll_av}
\alias{roll_av}
\title{Rolling average table generating}
\usage{
roll_av(data, k)
}
\arguments{
\item{data}{This the data produced by bin_CI()}

\item{k}{This the the lagged number of months to generate the rolling average from}
}
\value{
The dataframe with the rolling average percentages
}
\description{
This function generates the rolling average values using the data generated
from bin_CI()
}
\examples{
roll_av_data <- roll_av(citable, 3)
}
