#' plot entire path log from the restimote
#'
#' @param obj 
#'
#' @return plot with walked path
#' @export
#'
#' @examples
plot_path <- function(obj){
  plot_walking_path(obj$log, obj$location_size)
}

#' plots a single trial path
#'
#' @param obj RestimoteObject Needs to be preprocessed
#' @param trialId integed of given trial
#'
#' @return prints plot
#' @export 
#' @import ggplot2
#' @examples
#' plot_trial_path(obj, 1)
plot_trial_path <- function(obj, trialId){
  df_trial_log <- get_position_trial(obj, trialId)
  plot_walking_path(df_trial_log, obj$location_size)
}


#' Plots path between two timepoints
#'
#' @param obj RestimoteObject
#' @param start time when we want to start plotting
#' @param end time at which we shoudl stop plotting
#'
#' @return plot
#' @export
#'
#' @examples
plot_path_time <- function(obj, start, end){
  df_log <- get_position_between(obj, start, end)
  plot_walking_path(df_trial_log, obj$location_size)
}