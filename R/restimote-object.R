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
  obj$compass_offset <- NA
  obj$ <- list()
  obj$log <- NA
  obj$companion <- NA
  return(obj)
}