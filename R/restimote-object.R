#' Creates list with parameters to be filled while processng
#' 
#' @return list with instantiated fields
#' @export

RestimoteObject <- function(){
  obj <- list()
  obj$participant_id <- ""
  obj$experiment_name <- ""
  obj$session <- NA
  obj$date <- NA
  obj$compass_offset <- NA
  obj$n_trials <- NA
  obj$location_size <- NA
  obj$log <- NA
  obj$companion <- NA
  obj$goal_positions <- NA
  obj$goal_order <- NA
  class(obj) <- append(class(obj), "restimote")
  return(obj)
}