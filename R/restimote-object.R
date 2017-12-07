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