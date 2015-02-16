simplify_response <- function(x){
  UseMethod("simplify_response", x)
}

file_simp <- function(x){
  x$labels <- unlist(x$labels)
  x$exportLinks <- unlist(x$exportLinks)
  x$userPermission <- unlist(x$userPermission)
  x$ownerNames <- unlist(x$ownerNames)
  return(x)
}

simplify_response.file_list <- function(x){
  if(length(x$items) == 1){
    x$items <- file_simp(x$items[[1]])
  } else {
    x$items <- lapply(x$items, file_simp)
  }
  return(x)
}

simplify_response.file_metadata <- function(x){
  return(file_simp(x))
}