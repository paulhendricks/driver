#Revision simplifiers. This is the core logic
rev_simp <- function(x){
  x$lastModifyingUser <- unlist(x$lastModifyingUser)
  x$exportLinks <- unlist(x$exportLinks)
}

#This is the single-rev application.
simplify_response.rev_metadata <- function(x){
  return(rev_simp(x))
}

#...and this is the rev_list application.
simplify_response.rev_list <- function(x){
  if(length(x$items) == 1){
    x$items <- rev_simp(x$items[[1]])
  } else {
    x$items <- lapply(x$items, rev_simp)
  }
  return(x)
}

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
revision_metadata <- function(token, file_id, rev_id, simplify = FALSE, ...){
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

#'@title Download a specific revision of a Google Drive file
#'@description download a specific revision of a Google Drive file in
#'a user-specified format, and save it to disk.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param metadata a metadata object retrieved from \code{\link{revision_metadata}} or
#'\code{\link{list_files}}.
#'
#'@param download_type the format to download the file in. Available formats for a specific file
#'can be found in the "exportLinks" field of a metadata object.
#'
#'@param destination a file path to write the downloaded file to.
#'
#'@param ... any further arguments to pass to httr's GET.
#'
#'@return TRUE if the file could be downloaded, FALSE or an error otherwise.
#'@export
download_revision <- function(token, metadata, download_type, destination, ...){
  download_url <- unlist(unname(metadata$exportLinks[names(metadata$exportLinks) == download_type]))
  result <- GET(download_url, config(token = token, useragent = "driver - https://github.com/Ironholds/driver"),
                write_disk(destination), ...)
  return(check_result_status(result))
}