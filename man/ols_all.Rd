\name{ols_all}
\alias{ols_all}
\title{Creates OLS based on supplied formulas}
\usage{
ols_all(z, data, pval)
}
\arguments{
  \item{z}{A vector containing every models' formulas}
  \item{data}{The data on which applying the models}
  \item{pval}{If TRUE, removes the models with at least one coefficients with a p-value above 0.05, if FALSE keeps everything}
}
\value{
A list of models
}
\description{
Creates OLS based on supplied formulas
}
