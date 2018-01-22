#' Gets position data inside trial
#' 
#' @param obj RestimoteObject object
#' @param trialID int designating which trial to select
#' 
#' @return log data.frame with only rows from during the trial
#' 
#' @export
get_position_trial <- function(obj, trialId){
  timewindow <- get_trial_times(obj, trialId)
  df_log <- get_position_between(obj$log, timewindow$start, timewindow$end)
  return(df_log)
}

#' Colelcts start and end times for trial of particular id
#'
#' @param obj Restimote object
#' @param trialId 
#'
#' @return List with start and end. For the last trial, 
#' @export
#'
#' @examples
# TODO - redo to trial finishes
get_trial_times <- function(obj, trialId){
  if(!is_companion_preprocessed(obj)) return(NULL)
  ls <- list()
  ls$start <- obj$companion$Time[get_index_action_id(obj, NEW_TRIAL, trialId)]
  if(trialId == obj$n_trials - 1){
    ls$end <- obj$companion$Time[get_index_action_id(obj, NEW_SOP_VIEW, 1)]
  } else {
    ls$end <- obj$companion$Time[get_index_action_id(obj, NEW_TRIAL, trialId + 1)]
  }
  return(ls)
}

#' Returns of times when people were pointing
#'
#' @param obj
#' @param pointId which point you want
#' @param viewpoint if set, only returns pointings from given viewpoint
#' 
#' @return list with start and end. for last trial, end is NA, as there is no finishing signal
#' @export
get_trial_point_times <- function(obj, pointId, viewpoint = NULL){
  if(!is_companion_preprocessed(obj)) return(NULL)
  ls <- list()
  ls$start <- obj$companion$Time[get_index_action_id(obj, SHOULD_POINT, pointId)]
  ls$end <- obj$companion$Time[get_index_action_id(obj, SHOULD_POINT, pointId + 1)]
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
#' @return log line where participant pointed
#' @export 
#'
#' @examples
get_trial_point_orientation <- function(obj, trialId){
  times <- get_trial_point_times(obj, trialId)
  i_pointed <- get_next_point_index(obj, times = times)
  pointed_line <- obj$log[i_pointed, ]
  return(pointed_line)
}

#' returns vector 2 of x and Y position of trial goal position
#'
#' @param obj 
#' @param trialId 
#'
#' @return vector 2 of x and Y of the starting position
#' @export 
#'
#' @examples
get_start_position <- function(obj, trialId){
  if(trialId == 1) return(c(obj$log$Position.X[1], obj$log$Position.Y[1])) #returns STARTING POSITION AT the start of the expeiremnt
  return(get_goal_position(obj, trialId - 1))
}

#' returns vector 2 of x and Y position of trial goal position
#'
#' @param obj 
#' @param trialId 
#'
#' @return vector 2 of x and Y of the goal position
#' @export 
#'
#' @examples
get_goal_position <- function(obj, trialId){
  if(!check_goal_fields(obj)) return(NULL)
  if(!is_valid_trial(obj,trialId)) return(NULL)
  i_goal <- obj$goal_order[trialId]
  return(obj$goal_positions[i_goal, 2:3])
}