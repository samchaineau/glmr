\name{stationarity_Check}
\alias{stationarity_Check}
\title{Check the stationarity of your variables}
\usage{
stationarity_check(x, k, pval)
}
\arguments{
  \item{x}{A dataset}
  \item{k}{The parameter k of your ADF test}
  \item{pval}{The p-value for which you assume that the variable is stationary}
}
\value{
A list containing two dataframes: "Stationary" and "Not_Stationary" which contains the variables' names and their p-value.
}
\description{
Proceeds to an Augmented Dicker-Fuller test on every variables with your specified k and p-value thresholds (default values are respectively 1 and 0.05).
}
