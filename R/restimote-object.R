#' Creates list with parameters to be filled while processng
#' 
#' @return list with instantiated fields
#' @export

RestimoteObject <- function(){
  obj <- list()
  obj$participant_id <- ""
  obj$experiment_name <- ""
  obj$session <- NA
  obj$timestamp <- NA
  obj$data <- list()
  obj$data$log <- NA
  obj$data$companion <- NA
  return(obj)
}