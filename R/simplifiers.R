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
  x$items <- lapply(x$items, file_simp)
  return(x)
}

simplify_response.file_metadata <- function(x){
  return(file_simp(x))
}