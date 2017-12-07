#' Preprocessses and returns RestimoteObject companion
#'
#' @param obj RestimoteObject 
#'
#' @return preprocesse RestimoteObject
#' @export
#'
#' @examples
preprocess_companion_log <- function(obj){
  obj$companion <- add_actions_ids(obj$companion)
  obj$n_trials <- get_n_events(obj$companion, NEW_TRIAL)
  return(obj)
}


#' Preprocesses and returns restimote log
#'
#' @param obj 
#'
#' @return preprocessed RestimoteObject
#' @export
#'
#' @examples
preprocess_restimote_log <- function(obj){
  obj <- add_walked_distance(obj)
  obj <- correct_compass(obj)
  return(obj)
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
#' obj <- remove_trials(obj, c(1, 4))
#' obj <- remove_trials(obj, 9)
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
#' obj <- remove_pointing(obj, c(1, 4))
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
#' obj <- remove_pointing(obj, "Should point", c(1, 4))
remove_companion_ids <- function(obj, action, ids){
  if(!is_companion_preprocessed(obj)) return(obj)
  should_remove <- (obj$companion$Action == action & (obj$companion$Id %in% ids))
  obj$companion <- obj$companion[!should_remove, ]
  obj$companion <- add_actions_ids(obj$companion)
  return(obj)
}