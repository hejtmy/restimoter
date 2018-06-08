#' Returns log  data frame with positions during the experiment
#'
#' @param obj 
#'
#' @return data.frame with the log
#' @export
#'
#' @examples
get_log.restimote <- function(obj){
  return(obj$log)
}

#' Returns companion data frame with all the actions experiemnter logged
#'
#' @param obj 
#'
#' @return data.frame with the log
#' @export
#'
#' @examples
get_companion <- function(obj){
  return(obj$companion)
}

#' Returns log from particular time to another time
#'
#' @param obj Restimote object with log loaded
#' @param start Starting time of the log
#' @param end last accepable time of the log
#'
#' @return data.frame participant log
#' @export
#'
#' @examples
get_log_timewindow.restimote <- function(obj, start, end){
  df_log <- get_log.restimote(obj)
  df_log <- get_df_timewindow(df_log, start, end)
  return(df_log)
}

#' Returns companion from particular time to another time
#'
#' @param obj Restimote object with companion loaded
#' @param start Starting time of the log
#' @param end last accepable time of the log
#'
#' @return data.frame companion log
#' @export
#'
#' @examples
get_companion_timewindow <- function(obj, start, end){
  df_log <- get_companion(obj)
  df_log <- get_df_timewindow(df_log, start, end)
  return(df_log)
}

#' Gets position data inside trial
#' 
#' @param obj RestimoteObject object
#' @param trialID int designating which trial to select
#' 
#' @return log data.frame with only rows from during the trial
#' 
#' @export
get_trial_log.restimote <- function(obj, trialId){
  timewindow <- get_trial_times.restimote(obj, trialId)
  df_log <- get_log_timewindow.restimote(obj, timewindow$start, timewindow$end)
  return(df_log)
}

#' Gets companion data inside trial
#' 
#' @param obj RestimoteObject object
#' @param trialID int designating which trial to select
#' 
#' @return log data.frame with only rows from during the trial
#' 
#' @export
get_trial_companion.restimote <- function(obj, trialId){
  timewindow <- get_trial_times.restimote(obj, trialId)
  df_log <- get_companion_timewindow(obj, timewindow$start, timewindow$end)
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
get_trial_times.restimote <- function(obj, trialId){
  if(!is_companion_preprocessed(obj)) return(NULL)
  ls <- list()
  ls$start <- get_action_times(obj, NEW_TRIAL, trialId)
  ls$end <- get_action_times(obj, FINISH_TRIAL, trialId)
  return(ls)
}

#' Returns length of the trial withouth pauses/app resets included
#'
#' @param obj Restimote object
#' @param trialId 
#' @param without_pauses bool. Defaults to true
#' @param pause_limit Defaults to 10
#'
#' @return numeric of time length
#' @export
#'
#' @examples
get_trial_duration.restimote <- function(obj, trialId, without_pauses = T, pause_limit = 10){
  log <- get_trial_log.restimote(obj, trialId)
  times <- get_trial_times.restimote(obj, trialId)
  if(without_pauses){
    time_diff <- diff(log$Time)
    pause_time <- sum(time_diff[time_diff > pause_limit])
    dur <- times$end - times$start - pause_time
  } else {
    dur <- times$end - times$start
  }
  return(dur)
}

#' Returns distance traveled during a particular trial
#'
#' @param obj 
#' @param trialId 
#' @param true_log 
#' @param benevolence only valid if true_log. See benevolence in true_trial_log
#' @param radius only valid if true_log. See radius in true_trial_log
#'
#' @return
#' @export
#'
#' @examples
get_trial_distance.restimote <- function(obj, trialId, true_log = T, benevolence = 20, radius = 5){
  if(true_log){
    ls <- find_true_trial_log(obj, trialId, benevolence, radius)
    return(sum(ls$log$distance) + ls$radius) #adds the radius to the door participant never reached
  } else {
    log <- get_trial_log(obj, trialId)
    return(sum(log$distance))
  }
}

#' Returns times when certain action occured during entire experiment
#'
#' @param obj Restimote object with companion loaded
#' @param trialId integer designating which trial to load
#' @param action string with the name of the action
#'
#' @return vector of times
#' @export
#'
#' @examples
get_action_times.restimote <- function(obj, action){
  df_log <- get_companion(obj)
  times <- get_df_action_times(df_log, action)
  return(times)
}

#' Returns times when certain action occured during a trial
#'
#' @param obj Restimote object with companion loaded
#' @param trialId integer designating which trial to load
#' @param action string with the name of the action
#'
#' @return vector of times
#' @export
#'
#' @examples
get_trial_action_times.restimote <- function(obj, trialId, action){
  df_log <- get_trial_companion.restimote(obj, trialId)
  times <- get_df_action_times(df_log, action)
  return(times)
}

#' Returns how many actions of particular type were recorded
#'
#' @param obj
#' @param action string with searched action name
#' @param ... extra parameters
#'
#' @return integer with number of recorded actions
#' @export
#'
#' @examples
get_n_actions.restimote <- function(obj, action){
  return(length(get_action_times.restimote(obj, action)))
}

#' Returns how many actions of particular type were recorded during particular trial
#' 
#' @param obj
#' @param trialId integer with valid trialId
#' @param action string with searched action name
#' @param ...
#'
#' @return integer with number of recorded actions during particular trial
#' @export
#'
#' @examples
get_trial_n_actions.restimote <- function(obj, trialId, action){
  return(length(get_trial_action_times.restimote(obj, trialId, action)))
}

#' Gets number of times participant should point and pointed
#'
#' @param obj RestimoteObject
#'
#' @return list with log, companion fields
#' @export
get_n_pointings <- function(obj){
  ls <- list()
  ls$log <- get_df_n_actions(obj$log, POINTED)
  ls$companion <- get_df_n_actions(obj$companion, SHOULD_POINT)
  print(paste0("Player pointed ", ls$log, " and companion has ", ls$companion," points registered."))
  return(ls)
}

#' Returns of times when people were pointing
#'
#' @param obj Restimote object
#' @param pointId which point order you want
#' @param viewpoint if set, only returns pointings from given viewpoint
#' 
#' @return list with start and end. for last trial, end is NA, as there is no finishing signal
#' @export
get_trial_point_times.restimote <- function(obj, pointId, viewpoint = NULL){
  if(!is_companion_preprocessed(obj)) return(NULL)
  ls <- list()
  point_interval <- get_action_interval(obj, SHOULD_POINT, pointId)
  ls$start <- point_interval$start
  i_pointed <- get_next_point_index(obj, point_interval$start, point_interval$end)
  ls$end <- obj$log[i_pointed, ]$Time
  return(ls)
}

#' Gets the orientation of pointing in particular trial
#'
#' @param obj RestimoteObject
#' @param trialId integer with valid trial id
#'
#' @return orientation fo the participant when they pointed
#' @export 
#'
#' @examples
get_trial_point_orientation.restimote <- function(obj, trialId){
  times <- get_action_interval(obj, SHOULD_POINT, trialId)
  point_orientation <- get_point_orientation(obj, times$start, times$end)
  return(point_orientation)
}

#'returns 2D vector with x and Y position of trial start position
#'
#' @param obj Restimote object
#' @param trialId integer of the trial to fetch
#'
#' @return 2D vector with x and Y of the starting position
#' @export 
#'
#' @examples
get_start_position <- function(obj, trialId){
  if(trialId == 1) return(c(obj$log$Position.X[1], obj$log$Position.Y[1])) #returns STARTING POSITION AT the start of the expeiremnt
  return(get_goal_position(obj, trialId - 1))
}

#' returns 2D vector with x and Y position of trial goal position
#'
#' @param obj Restimote object
#' @param trialId what trial
#'
#' @return 2D vector with x and Y of the goal position
#' @export 
#'
#' @examples
get_goal_position <- function(obj, trialId){
  if(!check_goal_fields(obj)) return(NULL)
  if(!is_valid_trial(obj,trialId)) return(NULL)
  i_goal <- obj$goal_order[trialId]
  return(obj$goal_positions[i_goal, 2:3])
}