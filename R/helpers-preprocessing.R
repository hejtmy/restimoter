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
    warning("Companion doesn'thave ID column. Have you preprocessed it yet?")
    return(FALSE)
  }
  return(TRUE)
}

remove_mistakes <- function(df){
  
}