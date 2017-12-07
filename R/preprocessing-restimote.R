#' Title
#'
#' @param obj RestimoteObject 
#'
#' @return 
#' @export
#'
#' @examples
preprocess_companion_log <- function(obj){
  actions <- unique(obj$companion$Action)
  ## ADDS ID number
  obj$companion$Id <- 0
  for (action in actions){
    n_events <- get_n_events(obj$companion, action)
    obj$companion$Id[obj$companion$Action == action] <- 1:n_events
  }
  return(obj)
}


#' Title
#'
#' @param obj 
#'
#' @return
#' @export
#'
#' @examples
preprocess_restimote_log <- function(obj){
  
}

## Post preprocessing cleaning ----
##
#' Title
#'
#' @param obj 
#' @param ids 
#'
#' @return
#' @export
#'
#' @examples
remove_trials <- function(obj, ids){
  
}

#' Title
#'
#' @param obj 
#' @param ids 
#'
#' @return
#' @export
#'
#' @examples
remove_pointing <- function(obj, ids){
  
}