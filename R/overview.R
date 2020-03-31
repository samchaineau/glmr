#' Summarize every models from a list of LMs or GLMs
#'
#' Extract from a list of models their ranks, formulas, R2, adjusted R2 and AIC. Can be launched on an object of type list or on the function lm_all or glm_all in order to save memory.
#' @param Models_list A list of models
#' @return A dataframe containing the models' summary
#' @export

overview <- function(Models_list){

  if(!require("rsq")) install.packages("rsq")
  library(rsq)

  N <- do.call(rbind,lapply(Models_list, function(x){if("(Intercept)" %in% names(x$coefficients)){x$rank-1}else{x$rank}}))
  FORMULA <- sapply(Models_list, function(x){Reduce(paste, deparse(x$terms))})


  if(class(Models_list[1])[1] == "glm" && Models_list[[1]]$family$family == "binomial"){
print("Binomial family: Extracting the R-squared will take 1 to 2 seconds per models")

    Results <- NULL
    for(i in 1:length(Test_glm)){

      r2 <- rsq(glm(Test_glm[[i]]$formula, family = Test_glm[[i]]$family$family, data = Test_glm[[i]]$data))
      Results <- cbind(Results, r2)

    }
  RSQUARED <- Results

  }else{
  RSQUARED <- do.call(rbind, lapply(Models_list, function(x){cor(x$fitted.values, x$fitted.values+x$residuals)^2}))}

  Number_of_points <- do.call(rbind, lapply(Models_list, function(x){sum(!is.na(x$fitted.values))}))

  test <- data.frame(N, RSQUARED, Number_of_points)
  names(test) <- c("k", "R2", "n")

  ADJRSQUARED <- 1-(((1-test$R2)*(test$n-1))/(test$n - test$k -1))
  remove(test)

  if(class(Models_list[[1]])[1] == "lm"){
  AIC <- do.call(rbind,lapply(Models_list, function(x){AIC(x)}))}else{AIC <- sapply(Models_list, function(x) {summary(x)$aic})}

  return <- data.frame(cbind(N, FORMULA, RSQUARED, ADJRSQUARED, AIC))
  names(return) <- c("n", "formula", "r.squared", "adj.rsquared", "aic")
  return$n <- as.integer(as.character(return$n))
  return$formula <- as.character(return$formula)
  return$r.squared <- as.numeric(as.character(return$r.squared))
  return$adj.rsquared <- as.numeric(as.character(return$adj.rsquared))
  return$aic <- as.numeric(as.character(return$aic))

  return(return)
}



