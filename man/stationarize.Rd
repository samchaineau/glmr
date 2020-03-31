\name{stationarize}
\alias{stationarize}
\title{Stationarizes your dataset}
\usage{
stationarize(x, Variables, Method)
}
\arguments{
  \item{x}{A dataset}
  \item{Variables}{The colnames of your selected variables, put names(x) to take everything}
  \item{Method}{A character specifying the methodology chosen from c("Difference", "Division", "Log", "Square")}
}
\value{
A dataframe containing the new variables after the process
}
\description{
Stationarizes your dataset with your chosen methodology and print the number of stationarized variables after stationarizing
}
