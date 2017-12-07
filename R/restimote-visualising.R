#' Title
#'
#' @param obj RestimoteObject Needs to be preprocessed
#' @param trialId integed of given trial
#'
#' @return prints plot
#' @export 
#'
#' @examples
#' plot_trial_path(obj, 1)
plot_trial_path <- function(obj, trialId){
  df_trial_log <- get_position_trial(obj, trialId)
  plot(df_trial_log$Position.X, df_trial_log$Position.Y, type = "l")
}