#'@title list revisions of a Google Drive file
#'@description retrieves the metadata associated with each revision of a specific
#'Google Drive file.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'@export
list_revisions <- function(token, file_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/revisions")
  results <- driver_get(parameters, "rev_list", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

#'@title Get the metadata of a single revision of a Google Drive file
#'@description retrieves the metadata associated with a particular revision of a specific
#'Google Drive file.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param rev_id the ID of a revision of that file.
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@export
get_revision <- function(token, file_id, rev_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/revisions/", rev_id)
  result <- driver_get(parameters, "rev_metadata", token, ...)
  if(simplify){
    result <- simplify_response(result)
  }
  return(result)
}

#'@title remove a particular revision of a Google Drive file
#'@description junks a specified revision of a Google Drive file. Note that some file types
#'may not support revision deletion, in which case a 400 error will be returned.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param rev_id the ID of a revision of that file.
#'
#'@param ... further arguments to pass to httr's DELETE.
#'
#'@export
delete_revision <- function(token, file_id, rev_id, ...){
  parameters <- paste0("files/", file_id, "/revisions/", rev_id)
  result <- driver_delete(parameters, token, ...)
  return(check_result_status(result))
}

update_revision <- function(token, file_id, rev_id, ...){
  driver_put
}