#' Gets position data inside trial
#' 
#' @param obj RestimoteObject object
#' @param trialID int designating which trial to select
#' 
#' @export

get_position_trial <- function(obj, trialID){
  
}

#' Gets number of trials in restimote experiment
#'
#' @param obj RestimoteObject
#'
#' @return number of trials or NULL
#' @export 
#'
#' @examples 
get_number_of_trials <- function(obj){
  return(get_n_events(obj$companion, "New trial"))
}

#' Gets number of times participant should point
#'
#' @param obj RestimoteObject
#'
#' @return
#' @export

get_number_of_pointings <- function(obj){
  return(get_n_events(obj$companion, "Should point"))
}