#'@export
list_revisions <- function(token, file_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/revisions")
  results <- driver_get(parameters, "rev_list", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

get_revision <- function(token, file_id, rev_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/revisions/", rev_id)
  results <- driver_get(parameters, "rev_metadata", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

delete_revision <- function(token, file_id, rev_id, ...){
  driver_delete
}

update_revision <- function(token, file_id, rev_id, ...){
  driver_put
}