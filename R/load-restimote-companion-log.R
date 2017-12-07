#' Opens restimote companion app in a given folder 
#' 
#' @param log_path either a filepath, or directory where the file is located
#' @param exp_timestamp in case multiple restimotes are in the directory
#' @param obj already created restimote object
#' @return Restimote object with filled in data 
#' @export
load_restimote_companion_log <- function(log_path, exp_timestamp = NULL, obj = NULL){
  # if we didn't pass a file to load
  if(!is_directory(log_path)){
    log_path <- find_restimote_file(log_path, "restimote-companion", exp_timestamp)
  }
  if(is.null(log_path)) return(NULL)
  if(is.null(obj)) obj <- RestimoteObject()
  text <- readLines(log_path, warn = F)
  #needs to be before resaving text
  bottomHeaderIndex <- get_indicies_between(text, "SETTINGS")$end
  ls <- get_json_between(text, "SETTINGS")
  obj <- fill_in_settings(obj, ls)
  obj$log <- read.table(log_path, header = T, sep = ";", 
                             stringsAsFactors = F, skip = bottomHeaderIndex)
  return(obj)
}