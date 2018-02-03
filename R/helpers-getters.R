get_n_events <- function(df, event){
  if(is.null(nrow(df))) return(NULL)
  n_trials <- sum(df$Action == event)
  return(n_trials)
}

get_position_between <- function(df_log, start, end){
  i_start <- get_index_time(df_log, start)
  i_end <- get_index_time(df_log, end)
  if(is.null(i_start) || is.null(i_end)){
    return(NULL)
  }
  return(df_log[i_start:i_end, ])
}

# Returns index in df where time was larger than given time
get_index_time <- function(df, time){
  ids <- which(df$Time > time)
  if(length(ids) == 0){
    print("Couldn't find any recordings made after this time.")
    return(NULL)
  }
  return(ids[1])
}

# Returns index line where participant pointed - only a first one
# times is a list with start and end field
get_next_point_index <- function(obj, times){
  if(is.null(times$end)) times$end <- times$start + 100 #last trial
  first <- which(obj$log$Action == POINTED & obj$log$Time > times$start & obj$log$Time < times$end)
  if(length(first) == 0) {
    print("Couldn't find any points made after this time.")
    return(NULL)
  }
  return(first[1])
}

#returns row index for particular action and id
get_index_action_id <- function(obj, action, id){
  id <- which(obj$companion$Action == action & obj$companion$Id == id)
  if(length(id) == 0){
    print("There isn't an event with this id")
    return(NULL)
  }
  return(id)
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