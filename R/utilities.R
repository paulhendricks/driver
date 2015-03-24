#Creates a Google Drive APIv2 URL. Little wrapper around paste0().
create_url <- function(parameters){
  if(grepl(x = parameters, pattern = "https?://")){
    return(parameters)
  }
  return(paste0("https://www.googleapis.com/drive/v2/",parameters))
}

#For DELETE calls and similar, checks if the status code is within an acceptable range and returns a boolean.
check_result_status <- function(result){
  if(result$status_code %in% c(200, 202, 204)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Detects if the "Id" is actually a full Google Drive URL and handles it accordingly
#'@importFrom stringi stri_extract_first
detect_full_url <- function(id){
  if(grepl(x = id, pattern = ".*\\.google.com/")){
    id <- stri_extract_first(id, regex = "(?<=/d/).*?(?=/)")
  }
  return(id)
}

#Base simplifier function, calling class-specific methods. Those are stored
#in the relevant file (so, file_list's simplifier lives in files.R)
simplify_response <- function(x){
  UseMethod("simplify_response", x)
}