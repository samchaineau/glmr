#' Create Linear Models
#'
#' Create linear models based on supplied formulas
#' @param z A vector containing every models
#' @param data The data on which applying the regressions
#' @param pval If TRUE, removes the models with at least one coefficients with a p-value above 0.05, if FALSE keeps everything
#' @return A list of models
#' @export
ols_all <- function(z, data, pval){

  if(isFALSE(pval)){return(lapply(z, function(z){lm(paste(z), data=data)}))}else{

    Models_list <- lapply(z, function(z){lm(paste(z), data=data)})
    SUMMARIES <- lapply(Models_list,summary)
    COEF <- lapply(SUMMARIES, function(x){data.frame(x$coefficients)})
    FILTER <- do.call(rbind,lapply(COEF, function(x){sum(x[row.names(x) != "(Intercept)",4]>0.05)}))
    return <- Models_list[FILTER == 0]

    return(return)
  }

}
