# Helper loading functions

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
fill_in_settings <- function(obj, ls){
  settings_params <- c("participant_id", "compass_offset", "date")
  for (param in settings_params){
    if(length(obj[[param]] == 0)) obj[[param]] <- ls[[param]]
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
