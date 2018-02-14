#' Opens restimote log and attachï¿½s it to the restimote object
#' 
#' @param path path to the restomote log, oir direcotry where the log is located
#' @param exp_timestamp what timestamp should the log have?
#' @param obj If instantiated, object to be filled with data
#' @return restimote object with loaded log 
#' @export
load_restimote_log <- function(log_path, exp_timestamp = NULL, obj = NULL){
  obj <- load_restimote_generic_log(log_path, "restimote", "log", exp_timestamp, obj)
  return(obj)
}

#' Opens restimote companion app in a given folder 
#' 
#' @param log_path either a filepath, or directory where the file is located
#' @param exp_timestamp in case multiple restimotes are in the directory
#' @param obj already created restimote object
#' @return Restimote object with filled in data 
#' @export
load_restimote_companion_log <- function(log_path, exp_timestamp = NULL, obj = NULL){
  obj <- load_restimote_generic_log(log_path, "restimote-companion", "companion", exp_timestamp, obj)
  return(obj)
}

#' Add locations
#' 
#' @param obj RestimoteObject
#' @param name name of hte location
#' @param x x position 
#' @param y y position
#' @return 
#' @export 
add_location <- function(obj, name, x, y){
  if(is.null(obj$locations)){
    obj$locations <- data.frame(name = name, x = x, y = y)
    return(obj)
  }
  new <- data.frame(name = name, x = x, y = y)
  obj$locations <- rbind(obj$locations, new)
  return(obj)
}