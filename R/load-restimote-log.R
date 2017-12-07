#' Opens restimote log and attachï¿½s it to the restimote object
#' 
#' @param path path to the restomote log, oir direcotry where the log is located
#' @param exp_timestamp what timestamp should the log have?
#' @param obj If instantiated, object to be filled with data
#' @return restimote object with loaded log 
#' @export
load_restimote_log <- function(log_path, exp_timestamp = NULL, obj = NULL){
  # if we didn't pass a file to load
  if(!is_directory(log_path)){
    log_path <- find_restimote_file(log_path, exp_timestamp)
  }
  if(is.null(log_path)) return(NULL)
  if(is.null(obj)) obj <- RestimoteObject()
  text <- readLines(log_path, warn = F)
  #needs to be before resaving text
  bottomHeaderIndex <- get_indicies_between(text, "SETTINGS")$end
  ls <- get_json_between(text, "SETTINGS")
  obj$participant_id <- ls$participant_id
  obj$compass_offset <- ls$compass_offset
  obj$timestamp <- ls$date
  obj$data$log <- read.table(log_path, header = T, sep = ";", 
                        stringsAsFactors = F, skip = bottomHeaderIndex)
  return(obj)
}

#' Goes throught given folder and finds a restimote log
#' Returns filepath or returns NULL

find_restimote_file <- function(dir, exp_timestamp){
  ptr <- create_log_search_pattern("restimote", exp_timestamp)
  logs <- list.files(dir, pattern = ptr, full.names = T)
  if(length(logs) < 1){
    smart_print(c("Could not find any test logs in ", directory))
    return(NULL)
  }
  return(logs[1])
}