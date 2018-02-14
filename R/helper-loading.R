# Helper loading functions

#' @param log_path where to search
#' @param log_type what type of resitmote log - resitmote-comapnion, or restimote
#' @param data_field where to save parsed table
#' @param exp_timestamp 
#' @param obj either 
#' @noRd
load_restimote_generic_log <- function(log_path, log_type, data_field, exp_timestamp = NULL, obj = NULL){
  # if we didn't pass a file to load
  if(!is_directory(log_path)){
    log_path <- find_restimote_file(log_path, log_type, exp_timestamp)
  }
  if(is.null(log_path)) return(NULL)
  if(is.null(obj)) obj <- RestimoteObject()
  text <- readLines(log_path, warn = F)
  #needs to be before resaving text
  bottomHeaderIndex <- get_indicies_between(text, "SETTINGS")$end
  ls <- get_json_between(text, "SETTINGS")
  obj <- fill_in_info(obj, ls)
  obj[[data_field]] <- read.table(log_path, header = T, sep = ";", 
                                stringsAsFactors = F, skip = bottomHeaderIndex)
  return(obj)
}

create_separator <- function(string){
  ls <- list()
  ls$beginning <- paste("\\*\\*\\*\\",string, "\\*\\*\\*", sep="")
  ls$end <- paste("\\-\\-\\-",string, "\\-\\-\\-", sep="")
  return(ls)
}

create_log_search_pattern <- function(log_name, log_timestamp){
  ptr <- paste0("_", log_name, "_")
  if(!is.null(log_timestamp)){
    ptr <- paste0(ptr, "*", log_timestamp)
  }
  return(ptr)
}

# Gets RestimoteObject and loaded settings in list
# iterates through given params and fills them in if empty
fill_in_info <- function(obj, ls){
  settings_params <- c("compass_offset", "date")
  for (param in settings_params){
    if(is.null(obj$info[[param]])) obj$info[[param]] <- ls[[param]]
  }
  return(obj)
}

# Goes throught given folder and finds a restimote log
# Returns filepath or returns NULL
find_restimote_file <- function(dir, type, exp_timestamp = NULL){
  ptr <- create_log_search_pattern(type, exp_timestamp)
  logs <- list.files(dir, pattern = ptr, full.names = T)
  if(length(logs) < 1){
    print(paste0("Could not find any ", type, " logs in ", dir))
    return(NULL)
  }
  return(logs[1])
}

get_json_between <- function(text, string){
  ls <- json_to_list(get_text_between(text, string))
  return(ls)
}

get_indicies_between <- function(text, string){
  ls <- list()
  ls$beginning <- which(grepl(create_separator(string)$beginning, text))
  ls$end <- which(grepl(create_separator(string)$end, text))
  return(ls)
}

get_text_between <- function(text, string){
  indices <- get_indicies_between(text, string)
  if (length(indices$beginning) != 1 || length(indices$end) != 1) return (NULL)
  text <- text[(indices$beginning + 1):(indices$end - 1)]
  return(text)
}

is_directory <- function(str){
  last_char <- str[length(str)]
  return(str[length(str)] == "/" || str[length(str)] == "\\")
}

json_to_list <- function(text){
  #JSON checking
  if (!requireNamespace("jsonlite", quietly = T)){
    stop("jsonlite package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  ls <- jsonlite::fromJSON(text)
  return(ls)
}
