#' Returns how long the trial took and removes potential pauses in the log
#'
#' @param obj 
#' @param withoutPauses Defaults to true
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
get_trial_duration <- function(obj, trialId, withoutPauses = T, ...){
  UseMethod("get_trial_duration")
}