#' Title
#'
#' @param log Data framew with Position.X and Position.Y column 
#' @param points necessary if choosing median smoothing
#' @param type median, approx, spline
#'
#' @return log with smoothed X and Y columns
#' @export
#'
#' @examples
smooth_positions <- function(log, type = "median", points = 11){
  # running median
  if(type == "median"){
    log$Position.X <- runmed(log$Position.X, points, endrule = "constant")
    log$Position.Y <- runmed(log$Position.Y, points, endrule = "constant")
  }
  if(type == "approx"){
    log$Position.X  <- approx(log$Position.X)
    log$Position.Y <- approx(log$Position.Y)
  }
  if(type == "spline"){
    log$Position.X  <- smooth.spline(log$Position.X)$y
    log$Position.Y <- smooth.spline(log$Position.Y)$y
  }
  return(log)
}

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
 ls <- create_true_trial_log(obj, trialId, benevolence, radius)
 return(ls$log)
}

create_true_trial_log <- function(obj, trialId, benevolence = 30, radius = 2){
  start_pos <- get_start_position(obj, trialId)
  goal_pos <- get_goal_position(obj, trialId)
  # positions are shifted to the later times ... so we know that the trial definitely starte at 
  # this time or later
  timewindow <- get_trial_times(obj, trialId)
  #' finding the place that is the closest to the starting position ..... it might come sooner 
  #' than person really started this trial, but it doesn't matter, as times are calculated from a 
  #' differnet log - we need this for the distance summary
  small_log <- get_log_timewindow.restimote(obj, timewindow$start, timewindow$end + benevolence)
  distances_start <- apply(small_log[, c('Position.X', "Position.Y")], 1, function(x) navr::euclid_distance(x, start_pos))
  distances_goal <- apply(small_log[, c('Position.X', "Position.Y")], 1, function(x) navr::euclid_distance(x, goal_pos))
  
  i_close_to_start <- NA
  i_close_to_goal <- NA
  goal_radius <- NA
  start_radius <- NA
  
  for(rad in seq(0.5, radius, 0.1)){
    if(is.na(i_close_to_start)){
      i_close_to_start <- which(distances_start < rad)[1]
      start_radius <- rad
    }
    if(is.na(i_close_to_goal)){
      i_close_to_goal <- which(distances_goal < rad)[1]
      goal_radius <- rad
    }
  }
  if(any(is.na(c(i_close_to_goal, i_close_to_start)))){zzzz
    print("wasn't able to find a goal within such radius")
    return(NULL)
  }
  found_log <- small_log[i_close_to_start:i_close_to_goal, ]
  
  # changing first and last point to start and goal
  found_log[1,"Position.X"] <- start_pos[1]
  found_log[1, "Position.Y"] <- start_pos[2]
  found_log[nrow(found_log), "Position.X"] <- goal_pos[1]
  found_log[nrow(found_log), "Position.Y"] <- goal_pos[2]
  
  # recalculating distance
  
  found_benevolence <- small_log$Time[i_close_to_goal] - timewindow$end
  return(list(log = found_log, goal_radius = goal_radius, start_radius = start_radius, benevolence = found_benevolence))
}
