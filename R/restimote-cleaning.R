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
#' @param trialId 
#' @param benevolence how much time after the trial ended you give the function to search (default 30s)
#' @param radius what radius around goal position do you consider arriving at the goal position? (defualt 2 meters)
#'
#' @return log with the trial from the beginning to the 
#' @export
#'
#' @examples
true_trial_log <- function(obj, trialId, benevolence = 30, radius = 2){
  start_pos <- get_start_position(obj, trialId)
  goal_pos <- get_goal_position(obj, trialId)
  # positions are shifted to the later times ... so we know that the trial definitely starte at 
  # this time or later
  timewindow <- get_trial_times(obj, trialId)
  #' finding the place that is the closest to the starting position ..... it might come sooner 
  #' than person really started this trial, but it doesn't matter, as times are calculated from a 
  #' differnet log - we need this for the distance summary
  small_log <- get_position_between(obj$log, timewindow$start, timewindow$end + benevolence)
  distances_start <- apply(small_log[, c('Position.X', "Position.Y")], 1, function(x) euclid_distance(x, start_pos))
  distances_goal <- apply(small_log[, c('Position.X', "Position.Y")], 1, function(x) euclid_distance(x, goal_pos))
  i_close_to_start <- NA
  i_close_to_goal <- NA
  for(rad in seq(0.5, radius, 0.25)){
    if(is.na(i_close_to_start)) i_close_to_start <- which(distances_start < rad)[1]
    if(is.na(i_close_to_goal)) i_close_to_goal <- which(distances_goal < rad)[1]
  }
  if(any(is.na(c(i_close_to_goal, i_close_to_start)))){
    print("wasn't able to find a goal within such radius")
    return(NULL)
  }
  return(small_log[i_close_to_start:i_close_to_goal,])
}
