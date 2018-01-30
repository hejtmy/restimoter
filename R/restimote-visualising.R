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
  plt <- navr::create_plot()
  if(!is.null(obj$map_limits)){
    plt <- plt + xlim(obj$map_limits$x) + ylim(obj$map_limits$y)
  }
  df_trial_log <- get_position_trial(obj, trialId)
  plt <- navr::plot_add_path(plt, df_trial_log)
  plt <- plot_add_trial_start_goal(plt, obj, trialId)
  return(plt)
}

#' Title
#'
#' @param obj 
#' @param columns 
#' @param indices 
#'
#' @return
#' @export
#'
#' @examples
plot_trials_paths <- function(obj, columns = 5, indices = c()){
  indices <- if (length(indices) == 0) 1:obj$n_trials else indices
  plots <- list()
  for(i in 1:length(indices)){
    plots[[i]] <- plot_true_trial_path(obj, indices[i])
  }
  navr::multiplot(plots, cols = columns)
}

#' Plots path as calculated by the true trial log, rather than by given timestamps
#'
#' @param obj 
#' @param trialId 
#'
#' @return
#' @export
#'
#' @examples
plot_true_trial_path <- function(obj, trialId){
  df_trial_log <- true_trial_log(obj, trialId, radius = 5)
  if(is.null(df_trial_log)) {
    print("Never reached those points")
    return(plot_trial_path(obj, trialId))
  }
  plt <- create_plot(obj) 
  plt <- navr::plot_add_path(plt, df_trial_log)
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
  plt <- navr::create_plot(obj)
  df_log <- get_position_between(obj, start, end)
  plt <- navr::plot_add_path(plt, df_log)
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
  ls <- list(goal = get_goal_position(obj, trialId), start = get_start_position(obj, trialId))
  if(is.null(ls$goal)) return(plt)
  plt <- navr::plot_add_points(plt, ls, color = "green")
  return(plt)
}