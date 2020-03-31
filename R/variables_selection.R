#' Select the best features for your model based on correlation
#'
#' Calculate the correlation of every variables with your targeted one then extract the top n elements
#' @param x A dataset with your targeted variable
#' @param Target The colname of your targeted variable
#' @param n The number of variables you would like to shorttlist as regressors
#' @return A dataframe containing the variables' names and their correlation with your target
#' @export
variables_selection <- function(x, Target, n){

  Parameter <- do.call(rbind,lapply(x, is.numeric))

  if(sum(Parameter) < ncol(x)){x <- x[,t(Parameter)[1,]]
  message("Removed not numeric variables from the dataset")}

  int_res <- data.frame(cor(x[names(x) != Target], x[Target]))
  names(int_res) <- "Correlation"
  int_res$Variables <- row.names(int_res)

  return(top_n(int_res, n, Correlation^2))
}

