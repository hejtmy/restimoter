#' Title
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
  if (!requireNamespace("ggplot2", quietly = T)){
    stop("Needs ggplot2 to continue.")
  }
  library(ggplot2)
  df_trial_log <- get_position_trial(obj, trialId)
  plt <- ggplot(df_trial_log, aes(Position.X, Position.Y))
  plt <- plt + geom_path()
  plt <- plt + xlim(0, 25) + ylim(0, 25)
  plt
}