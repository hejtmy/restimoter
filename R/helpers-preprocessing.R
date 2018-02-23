add_actions_ids <- function(df){
  actions <- unique(df$Action)
  df$Id <- 0
  for (action in actions){
    n_events <- get_df_n_actions(df, action)
    df$Id[df$Action == action] <- 1:n_events
  }
  return(df)
}

add_walked_distance <- function(obj){
  obj$log$distance <- navr::euclid_distance_between_rows(data.frame(obj$log$Position.X, obj$log$Position.Y))
  return(obj)
}

is_companion_preprocessed <- function(obj){
  if(!("Id" %in% colnames(obj$companion))){
    warning("Companion doesn't have ID column. Have you preprocessed it yet?")
    return(FALSE)
  }
  return(TRUE)
}

remove_mistakes <- function(df){
  
}

# returns bool if the log has appropriate columns
is_log_preprocessed <- function(obj){
  
}

correct_compass_offset <- function(df_log, compass_shows, compass_should_be){
  compass_difference <- compass_should_be - compass_shows
  df_log$Orientation <- angle_to_360(df_log$Orientation + compass_difference)
  return(df_log)
}

point_compass_calibrations <- function(obj){
  LENIENCE <- 2 #given calibration shouldn't be preceded by pointing, we give a lenience of -2 seconds when participatn 
  #can point earlier than researcher presses the calibrate button
  n_calibrations <- get_df_n_actions(obj$companion, "Calibrate")
  print(paste0("Threre are ", n_calibrations, " calibration points"))
  if(n_calibrations == 0) return(obj)
  for(i in 1:n_calibrations){
    calib_interval <- get_action_interval(obj, "Calibrate", i)
    calib_interval$start <- calib_interval$start - LENIENCE
    point_orientation <- get_point_orientation(obj, calib_interval$start, calib_interval$end)
    #Correction only from the calibration forth
    i_point <- get_next_point_index(obj, calib_interval$start, calib_interval$end)
    i_subset <- i_point:nrow(obj$log)
    obj$log[i_subset,] <- correct_compass_offset(obj$log[i_subset,], point_orientation, 0)
  }
  return(obj)
}