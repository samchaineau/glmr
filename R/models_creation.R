#' Create models' formulas
#'
#' Create models' formulas based on the target variable and the shortlisted variables
#' @param x A vector of variables' names that represent your shortlisted regressors
#' @param Target The colname of your targeted variable
#' @param Limit The maximum number of regressors by model
#' @param Nointercept If you would like to have No Intercept models in addition
#' @return A dataframe containing all the formulas
#' @export

models_creation <- function(x, Target, limit, NoIntercept = FALSE){

  deta <- NULL

  for(i in 1:limit){
    tesst <- combn(x, m = i)
    res <- data.frame(apply(tesst, 2, paste, collapse = "+" ))

    if(i == 1){deta <- res}else{deta <- rbind(deta, res)}

  }

  names(deta) <- "Models"

  deta <- data.frame(paste(Target, "~", deta$Models))
  names(deta) <- "Models"

  deta$Models <- as.character(deta$Models)


  if(isTRUE(NoIntercept)){
    NoInterceptModels <- paste(deta$Models, "+ 0")
    deta <- data.frame(c(deta$Models, NoInterceptModels))
    names(deta) <- "Models"
    deta$Models <- as.character(deta$Models)
  }

  return(deta)
}
