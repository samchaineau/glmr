\name{variables_selection}
\alias{variables_selection}
\title{Select the best features for your model based on correlation coefficient}
\usage{
Variables_selection(x, Target, n)
}
\arguments{
  \item{x}{A dataset with your taregeted variable}
  \item{Target}{The colname of your targeted variable}
  \item{n}{The number of variables you would like to shortlist}
}
\value{
A dataframe containing the variables' names and their correlation coefficients with your target
}
\description{
Calculates the correlation of every variables with your targeted one then extracts the top n elements. Removes automatically non numeric variables from its analysis
}
