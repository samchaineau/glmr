\name{models_creation}
\alias{models_creation}
\title{Creates models' formulas}
\usage{
models_creation(x, Target, limit, NoIntercept)
}
\arguments{
  \item{x}{A vector of variables' names that represent your shortlisted regressors}
  \item{Target}{The colname of your targeted variable}
  \item{limit}{The maximum number of regressors you wish by model}
  \item{NoIntercept}{If you would like to have no intercept models in addition (TRUE or FALSE)}
}
\value{
A dataframe containing all the formulas
}
\description{
Creates models' formulas based on the target variable, the shortlisted regressors and additional parameters
}
