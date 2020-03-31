\name{overview}
\alias{overview}
\title{Summarizes every models from a list of LMs or GLMs}
\usage{
overview(Models_list)
}
\arguments{
  \item{Models_list}{A list of models}
}
\value{
A dataframe containing the models' overview.
}
\description{
Extracts from a list of models their ranks, formulas, r.squared, adjusted r.squared and AIC. Can be launched on an object of type list or on the function ols_all or glm_all in order to save memory and time.
}
