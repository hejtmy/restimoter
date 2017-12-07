get_n_events <- function(df, event){
  if(is.null(nrow(df))) return(NULL)
  n_trials <- sum(df$Action == event)
  return(n_trials)
}

get_log_between <- function(df_log, start, end){
  i_start <- get_index_time(df_log, start)
  i_end <- get_index_time(df_log, end)
  if(is.null(i_start) || is.null(i_end)){
    return(NULL)
  }
  return(df_log[i_start:i_end, ])
}

get_index_time <- function(df, time){
  ids <- which(df$Time > time)
  if(length(ids) == 0){
    print("Couldn't find any recordings made after this time.")
    return(NULL)
  }
  return(ids[1])
}

# Returns index of line where participant pointed
get_next_point <- function(time, df_log, time_limit){
  pointings <- which(df_log$Action == "pointed" & df_log$Time > time)
  if(length(first) == 0) {
    print("Couldn't find any points made after this time.")
    return(NULL)
  }
  return(pointings[1])
}

#returns row index for particular action and id
get_index_action_id <- function(df, action, id){
  id <- which(df$Action == action & df$Id == id)
  if(length(id) == 0){
    print("There isn't an event with this id")
    return(NULL)
  }
  return(id)
}