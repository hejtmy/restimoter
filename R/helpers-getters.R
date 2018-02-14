get_n_actions <- function(df, action){
  if(is.null(nrow(df))) return(NULL)
  n_trials <- sum(df$Action == action)
  return(n_trials)
}


# Returns index in df where time was larger than given time
get_time_row <- function(df, time){
  ids <- which(df$Time >= time)
  if(length(ids) == 0){
    print("Couldn't find any recordings made after this time.")
    return(NULL)
  }
  return(ids[1])
}

get_point_orientation <- function(obj, start, end){
  i_pointed <- get_next_point_index(obj, start, end)
  if(is.null(i_pointed)) return(NA)
  pointed_line <- obj$log[i_pointed, ]
  return(pointed_line$Orientation)
}

# Returns index line where participant pointed - only a first one
# times is a list with start and end field
get_next_point_index <- function(obj, start, end = NULL){
  if(is.null(end)) end <- start + 100 #last trial
  first_point <- which(obj$log$Action == POINTED & obj$log$Time > start & obj$log$Time < end)
  if(length(first_point) == 0) {
    print("Couldn't find any points made after this time.")
    return(NULL)
  }
  return(first_point[1])
}

#' Returns indices of rows when certain action occured
#' ids are which order of action you want to extract ... e.g. second "Calibrate" action row would be 
#' get_action_rows(obj, "Calibrate", 2)
get_action_rows <- function(obj, action, ids = NULL){
  action_rows <- which(obj$companion$Action == action)
  if(is.null(ids)) return(action_rows)
  n_actions <- length(action_rows)
  if(all(ids %in% 1:n_actions)) return(action_rows[ids])
  print(paste0("You required actions ", ids, ", but only ", n_actions, "are present"))
  return(NULL)
}

get_action_interval <- function(obj, action, actionId){
  if(!is_companion_preprocessed(obj)){
    warning("Companion log is not preprocessed. Cannot do this action.")
    return(NULL)
  }
  ls <- list()
  i_row <- get_action_rows(obj, action, actionId)
  ls$start <- get_action_times(obj, action, actionId) #whatever action after that
  ls$end <- ifelse(nrow(obj$companion) > i_row, obj$companion$Time[i_row + 1], ls$end <- tail(obj$log$Time, 1)) #last pointing
  return(ls)
}

get_action_times <- function(obj, action, ids = NULL){
  i_action <- get_action_rows(obj, action, ids)
  if(length(i_action) != 0) return(obj$companion$Time[i_action])
  return(NULL)
}

is_valid_trial <- function(obj, trialId){
  if(trialId > obj$n_trials){
    warning("you are passing trial number larger than number of trials overall.")
    return(FALSE)
  }
  return(TRUE)
}

check_goal_fields <- function(obj){
  if(!has_goal_positions(obj)){
    warning("There are no positions entered. Returning NULL")
    return(FALSE)
  }
  if(!has_goal_order(obj)){
    warning("There is no goal order entered. Returning NULL")
    return(FALSE)
  }
  return(TRUE)
}

has_goal_positions <- function(obj){
  return(is.data.frame(obj$goal_positions) && nrow(obj$goal_positions) > 1)
}

has_goal_order <- function(obj){
  return(length(obj$goal_order) > 1 && !is.na(obj$goal_order))
}