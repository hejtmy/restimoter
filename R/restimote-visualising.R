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
plot_add_restimote_path <- function(plt, df_pos){
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
  plt <- plot_add_restimote_path(plt, df_trial_log)
  plt <- plot_add_trial_start_goal(plt, obj, trialId)
  return(plt)
}

#' Plots path between two timepoints
#'
#' @param obj RestimoteObject
#' @param start time when we want to start plotting
#' @param end time at which we shoudl stop plotting
#'
#' @return plot
#' @examples
#' 
#' @export
plot_path_time <- function(obj, start, end){
  plt <- create_plot(obj)
  df_log <- get_position_between(obj, start, end)
  plt <- plot_add_restimote_path(plt, df_log)
  return(plt)
}

#' Title
#'
#' @param plt plt to have the start and goal added to
#' @param obj Restimote object with goal positions and goal order
#' @param trialId valid trialID
#'
#' @return plot
#' @export
#'
#' @examples 
plot_add_trial_start_goal <- function(plt, obj, trialId){
  ls <- list(goal = get_goal_position(obj, trialId))
  if(is.null(ls$goal)) return(plt)
  plt <- plot_add_points(plt, ls, color = "green")
  return(plt)
}

#' Adds specified points to the given plot
#' 
#' @param plot already created ggplot
#' @param ls list with XY vectors. eg. (list(start = c(0, 0), end = C(10, 5)))
#' @return modified plot
#' 
#' @export
plot_add_points <- function(plt, ls, size = 4, color = "blue"){
  list_names <- names(ls)
  df <- data.frame(point.x = numeric(0), point.y = numeric(0), point.name = character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    df[i, 1] <- ls[[i]][1]
    df[i, 2] <- ls[[i]][2]
    df[i, 3] <- list_names[i]
  }
  plt <- plt + geom_point(data = df, aes(point.x, point.y), size = size, color = color) + 
    geom_text(data = df, aes(point.x, point.y, label = point.name))
  return(plt)
}