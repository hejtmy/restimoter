#' Create a plot wiht constraints
#' 
#' @param obj RestimoteObject
#' @return ggplot2 plot
#' @example 
#' plt <- create_plot(obj)
#' 
#' @export
create_plot <- function(obj){
  if(!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 package")
  }
  plt <- ggplot2::ggplot()
  if (!is.null(obj$map_limits)){
    plt <- plt + xlim(obj$map_limits$x) + ylim(obj$map_limits$y)
  }
  return(plt)
}

#' plot entire path log from the restimote
#'
#' @param obj 
#'
#' @return plot with walked path
#' @export
#'
#' @examples
add_restimote_path <- function(plt, df_pos){
  plt <- plt + geom_path(data = df_pos, aes(Position.X, Position.Y))
  return(plt)
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
  plt <- create_plot(obj)
  df_trial_log <- get_position_trial(obj, trialId)
  plt <- add_restimote_path(plt, df_trial_log)
  return(plt)
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
  plt <- create_plot(obj)
  df_log <- get_position_between(obj, start, end)
  plt <- add_restimote_path(plt, df_log)
  return(plt)
}