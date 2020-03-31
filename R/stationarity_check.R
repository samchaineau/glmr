#' Check the stationarity of your variables
#'
#' Proceed to an Augmented Dicker-Fuller Test on every variables with your specified k and p-value threshold.
#' @param x A dataset
#' @param k The parameter k of your ADF test, default value is 1
#' @param pval The p-value for which you assume that the variable is stationary, default value is 0.05
#' @return A list containing two dataframes: "Stationary" and "Not_Stationary" which contains the variables' names and their p-value
#' @export


stationarity_Check <- function(x, k = 1, pval = 0.05){

  if(!require("tseries")) install.packages("tseries")
  library(tseries)

  Parameter <- do.call(rbind,lapply(x, is.numeric))

  if(sum(Parameter) < ncol(x)){x <- x[,t(Parameter)[1,]]
  message("Removed not numeric variables from the dataset")}

  test <-  do.call(rbind, lapply(apply(x, 2, adf.test, k = k), function(x){x$p.value})) %>% data.frame()
  names(test) <- "ADF"
  test$Variable <- rownames(test)

  Stationary <- test[test[,1] <= pval,]
  Not_Stationary <- test[test[,1] > pval,]

  if(nrow(Stationary) > 0){
  row.names(Stationary) <- c(1:nrow(Stationary))}

  if(nrow(Not_Stationary) > 0){
  row.names(Not_Stationary) <- c(1:nrow(Not_Stationary))}

  return <- list(Stationary, Not_Stationary)
  names(return) <- c("Stationary", "Not_Stationary")

  return(return)
}
