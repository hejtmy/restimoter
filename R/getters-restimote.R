#' Gets position data inside trial
#' 
#' @param obj RestimoteObject object
#' @param trialID int designating which trial to select
#' 
#' @export

get_position_trial <- function(obj, trialId){
  
}

#' 
#'
#' @param obj Restimote object
#' @param trialId 
#'
#' @return List with start and end. For the last trial, 
#' @export
#'
#' @examples
get_trial_times <- function(obj, trialId){
  if(!is_companion_preprocessed(obj)) return(NULL)
  ls <- list()
  ls$start <- obj$companion$Time[get_row_action_id(obj$companion, NEW_TRIAL, trialId)]
  if(trialId == obj$n_trials - 1){
    ls$end <- obj$companion$Time[get_row_action_id(obj$companion, NEW_TRIAL, trialId + 1)]
  } else {
    ls$end <- obj$companion$Time[get_row_action_id(obj$companion, NEW_SOP_VIEW, 1)]
  }
  return(ls)
}

#' Gets number of times participant should point and pointed
#'
#' @param obj RestimoteObject
#'
#' @return list with log, companion fields
#' @export

get_n_pointings <- function(obj){
  ls <- list()
  ls$log <- get_n_events(obj$log, POINTED)
  ls$companion <- get_n_events(obj$companion, SHOULD_POINT)
  print(paste0("Player pointed ", ls$log, " and companion has ", ls$companion," points registered."))
  return(ls)
}

#' Gets the orientation of pointing in particular trial
#'
#' @param obj RestimoteObject
#' @param trialId integer with valid trial id
#'
#' @return
#' @export
#'
#' @examples
get_trial_point <- function(obj, trialId){
  
}