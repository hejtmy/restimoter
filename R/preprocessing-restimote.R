#' Title
#'
#' @param obj RestimoteObject 
#'
#' @return 
#' @export
#'
#' @examples
preprocess_companion_log <- function(obj){
  obj$companion <- add_actions_ids(obj$companion)
  obj$n_trials <- get_n_events(obj$companion, NEW_TRIAL)
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

#' Remove trial mistakes from companion
#'
#' @param obj RestimoteObject. Needs to be preprocessed
#' @param ids vector of Ids to be removed
#'
#' @return RestimoteObject with removed Ids and recomputed new ids
#' @export
#'
#' @examples

remove_trials <- function(obj, ids){
  obj <- remove_companion_ids(obj, NEW_TRIAL, ids)
  obj$n_trials <- sum(obj$companion$Action == "New trial")
  return(obj)
}

#' Remove pointing mistakes from companion
#'
#' @param obj RestimoteObject. Needs to be preprocessed
#' @param ids vector of Ids to be removed
#'
#' @return RestimoteObject with removed Ids and recomputed new ids
#' @export
#'
#' @examples
remove_pointing <- function(obj, ids){
  obj <- remove_companion_ids(obj, SHOULD_POINT, ids)
  return(obj)
}

#' Title
#'
#' @param obj RestimoteObject. Needs to be preprocessed
#' @param action Name of the indexed event that you want ot remove
#' @param ids vector of Ids to be removed
#'
#' @return RestimoteObject with removed Ids and recomputed new ids
#' @export
#'
#' @examples
remove_companion_ids <- function(obj, action, ids){
  if(!is_companion_preprocessed(obj)) return(obj)
  should_remove <- (obj$companion$Action == action & (obj$companion$Id %in% ids))
  obj$companion <- obj$companion[!should_remove, ]
  obj$companion <- add_actions_ids(obj$companion)
  return(obj)
}