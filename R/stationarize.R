#' Stationarize your dataset
#'
#' Stationarize your dataset with your chosen methodology and print the number of stationarized variables after stationarizing
#' @param x A dataset
#' @param Variables The colnames of your selected variables, put names(x) to take everything
#' @param Method A character specifying the methodology chosen from c("Difference", "Division", "Log", "Square")
#' @param message A logical parameter to get a printed message with the number of successfuly stationarized variables.
#' @return A dataframe containing the new variables after the process
#' @export

stationarize <- function(x, Variables, Method, message = TRUE){

  tolower("BABA")

  Work <- dplyr::select(x, Variables)

  if( tolower(Method) == "difference"){
    Work <- data.frame(apply(Work, 2, diff))
    if(message == TRUE){
      Success <- length(Variables) - nrow(stationarity_Check(Work, k = 1)$Not_Stationary)
      message <- paste("You have", Success, "variables stationarized")
      zz <- readline(paste(message, "\n(press enter to continue)"))
      return(data.frame(Work))}else{return(data.frame(Work))}
  }

  if( tolower(Method) == "division"){
    Under <- apply(Work, 2, lag)
    return <- Work/Under
    Work <- return[-1,]
    if(message == TRUE){
      Success <- length(Variables) - nrow(stationarity_Check(Work, k = 1)$Not_Stationary)
      message <- paste("You have", Success, "variables stationarized")
      zz <- readline(paste(message, "\n(press enter to continue)"))
      return(data.frame(Work))}else{return(data.frame(Work))}
  }

  if( tolower(Method) == "log"){
    if(sum(Work <= 0) >0){print("Error log(): Negative values or zeros in the dataset")
      stop()}
    Work <- log(Work)
    if(message == TRUE){
      Success <- length(Variables) - nrow(stationarity_Check(Work, k = 1)$Not_Stationary)
      message <- paste("You have", Success, "variables stationarized")
      zz <- readline(paste(message, "\n(press enter to continue)"))
      return(data.frame(Work))}else{return(data.frame(Work))}
  }

  if( tolower(Method) == "square"){
    Work <- Work^2
    if(message == TRUE){
      Success <- length(Variables) - nrow(stationarity_Check(Work, k = 1)$Not_Stationary)
      message <- paste("You have", Success, "variables stationarized")
      zz <- readline(paste(message, "\n(press enter to continue)"))
      return(data.frame(Work))}else{return(data.frame(Work))}
  }

}
