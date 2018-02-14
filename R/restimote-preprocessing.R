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
  obj$n_trials <- get_n_actions(obj$companion, NEW_TRIAL)
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

#' Adds goal positions to the restimote object
#' 
#' @param obj Restimote object. 
#' @param positions Data frame with goal positions. eg. Needs to have name, position.x and position.y columns 
#'
#' @return RestimoteObject with goal positions field
#'
#' @export
add_goal_positions.restimote <- function(obj, positions){
  #some validations in the future
  obj$goal_positions <- positions
  return(obj)
}

#' Adds goal order vector to determine in what order goals came
#'
#' @param obj Restimote object
#' @param order vector of goal order. eg. (1, 3, 5, 1) 
#'
#' @return RestimoteObject witha added field
#' @export
#'
#' @examples 
add_goal_order.restimote <- function(obj, order){
  #validate numebr of goals
  #validate if numbers
  obj$goal_order <- order
  return(obj)
}

#' Fixes possible compass offsets created by iPhone irregular comapss settings
#'
#' @description 
#' Estimote sets each map to a certain orientation and then substracts that from compass information it is getting. 
#' But it happens, that iPhone compass calibrates like crazy. This function searches for parts of the log that are 
#' CAlibrated using the calibrate function in the restimote app and then separately preprocesses those parts where 
#' researcher asked participant to calibrate manually
#' 
#' @param obj RestimoteObject
#' @param estimote_offset Offset of the map as set in the estimote Layout
#'
#' @return estimote object
#' @export
#'
#' @examples 
#' obj <- calibrate_compass(obj, 321)
calibrate_compass <- function(obj, estimote_offset){
  offset <- obj$info$compass_offset
  if(!is.null(offset)){
    print(paste0("Correcting compass offset of", offset, " to match original ", estimote_offset, "."))
    obj$log <- correct_compass_offset(obj$log, offset, estimote_offset)
  }
  if(!is.null(obj$companion)){
    obj$log <- calibrate_compass(obj, estimote_offset)
  }
}