add_actions_ids <- function(df){
  actions <- unique(df$Action)
  df$Id <- 0
  for (action in actions){
    n_events <- get_n_events(df, action)
    df$Id[df$Action == action] <- 1:n_events
  }
  return(df)
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

add_walked_distance <- function(obj){
  obj$log$distance <- euclid_distance_col(data.frame(obj$log$Position.X, obj$log$Position.Y))
  return(obj)
}

correct_compass <- function(obj){
  return(obj)
}