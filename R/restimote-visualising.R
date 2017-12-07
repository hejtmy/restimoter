plot_trial_path <- function(obj, trialId){
  df_trial_log <- get_position_trial(obj, trialId)
  plot(df_trial_log$Position.X, df_trial_log$Position.Y, type = "l")
}