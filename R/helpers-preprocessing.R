add_actions_ids <- function(df){
  actions <- unique(df$Action)
  df$Id <- 0
  for (action in actions){
    n_events <- get_n_actions(df, action)
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

calibrate_compass_offset <- function(obj){
  n_calibrations <- get_n_actions(obj$companion, "Calibrate")
  print(paste0("Threre are ", n_calibrations, " calibration points"))
  for(i in 1:n_calibrations){
    calib_interval <- get_action_interval(obj, "Calibrate", i)
    
  }
  return(obj)
}