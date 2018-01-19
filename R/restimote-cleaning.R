#' Title
#'
#' @param df_player data fram withg player position
#' @param allowed_speed what is the maximum allowed sppeed before it is assumed to be artifact
#'
#' @return smoothed df_player 
#' @export
#'
#' @examples
remove_random_points <- function(df_player, allowed_speed = 4){
  
}

#' Goes through the restimote log and defines trials not in respect to the 
#' companion log but in respect to when player achieved started at the start 
#' and when they reached given goal - times are incorrect, but path shoudl correspond
#'
#' @param obj Preprocessed Restimote object
#' @param trialID valid id of the trial
#'
#' @return
#' @export
#'
#' @examples
true_trial_log <- function(obj, trialId){
  BENEVOLENCE_INTERVAl <- 200 #length of benevolence when 
  start_pos <- get_start_position(obj, trialId)
  goal_pos <- get_goal_position(obj, trialId)
  # positions are shifted to the later times ... so we know that the trial definitely starte at 
  # this time or later
  timewindow <- get_trial_times(obj, trialId)
  #' finding the place that is the closest to the starting position ..... it might come sooner 
  #' than person really started this trial, but it doesn't matter, as times are calculated from a 
  #' differnet log - we need this for the distance summary
  small_log <- get_position_between(obj$log, timewindow$start, timewindow$start + BENEVOLENCE_INTERVAl)
  distances <- apply(small_log[, c('Position.X', "Position.Y")], 1, function(x) euclid_distance(x, goal_pos))
  
}