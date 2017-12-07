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
get_n_trials <- function(obj){
  return(get_n_events(obj$companion, "New trial"))
}

#' Gets number of times participant should point and pointed
#'
#' @param obj RestimoteObject
#'
#' @return list with log, companion fields
#' @export

get_n_pointings <- function(obj){
  ls <- list()
  ls$log <- get_n_events(obj$log, "pointed")
  ls$companion <- get_n_events(obj$companion, "Should point")
  print(paste0("Player pointed ", ls$log, " and companion has ", ls$companion," points registered."))
  return(ls)
}