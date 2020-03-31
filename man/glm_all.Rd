\name{glm_all}
\alias{glm_all}
\title{Creates Generalized Linear Models}
\usage{
glm_all(z, ChosenFamily, data, pval)
}
\arguments{
  \item{z}{A vector containing every models' formulas}
  \item{ChosenFamily}{The family that you would like to use for your model (see glm function description)}
  \item{data}{The data on which applying the models}
  \item{pval}{If TRUE, removes the models with at least one coefficients with a p-value above 0.05, if FALSE keeps everything}
}
\value{
A list of GLM
}
\description{
Creates generalized linear models based on supplied formulas
}
