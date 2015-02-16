#'@title Retrieve the metadata for a specific file.
#'
#'@description \code{file_metadata} retrieves the metadata for a specific file the user
#'has access to. To retrieve the metadata for all files, see \code{\link{list_files}}
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id a file ID, as a string. This can be retrieved from the URL bar when you're accessing
#'the file: for example, "https://docs.google.com/document/d/1gOxog56F2bCnxwum7VhmN3JqTX7usTYcK5X3V4QDnxg"
#'has the file_id "1gOxog56F2bCnxwum7VhmN3JqTX7usTYcK5X3V4QDnxg".
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@seealso \code{\link{list_files}}, for requesting the metadata associated with many files.
#'
#'@examples
#'\dontrun{
#'#Once we've authenticated and grabbed a token, we can grab the metadata for the example file:
#'example_metadata <- file_metadata(token, "1gOxog56F2bCnxwum7VhmN3JqTX7usTYcK5X3V4QDnxg")
#'}
#'@export
file_metadata <- function(token, file_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id)
  result <- driver_get(parameters, "file_metadata", token, ...)
  if(simplify){
    result <- simplify_response(result)
  }
  return(result)
}

#'@title create an empty file, or upload a local file
#'
#'@importFrom httr upload_file
#'@export
create_file <- function(token, file_path = NULL, ...){
  parameters <- "files?uploadType=media"
  if(is.null(file_path)){
    result <- driver_post(parameters, token, ...)
  } else {
    result <- driver_post(parameters, token, body = upload_file(file_path), ...)
  }
  return(result)
}

update_file <- function(){
  
}

#'@title copy a Google Drive file
#'@description takes a Google Drive file and creates a copy of it, with the same
#'access restrictions.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of the file; see \code{\link{file_metadata}} for further
#'commentary.
#'
#'@param ... further arguments to pass to httr's POST.
#'
#'@return a set of metadata associated with the copy of the file, matching
#'the output of \code{\link{file_metadata}}.
#'
#'@export
copy_file <- function(token, file_id, ...){
  parameters <- paste0("files/", file_id, "/copy")
  result <- driver_post(parameters, token, ...)
  return(result)
}

#'@title delete a Google Drive file
#'@description \code{delete_file} removes a file completely, assuming the user has
#'permission to do so. In the process it completely bypasses the trash bin, rendering
#'the file unrecoverable by the user.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of the file; see \code{\link{file_metadata}} for further
#'commentary. 
#'
#'@param ... further arguments to pass to httr's DELETE.
#'
#'@return TRUE if the file was successfully deleted, FALSE or an error otherwise.
#'
#'@export
delete_file <- function(token, file_id, ...){
  parameters <- paste0("files/", file_id)
  result <- driver_delete(parameters, token)
  if(result$status_code %in% c(200, 202, 204)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'@title Retrieve the metadata for all files
#'
#'@description \code{list_files} allows an authenticated user to retrieve the metadata
#'associated with each file they have access to. For the metadata for a single file, see
#'\code{\link{get_file_metadata}}.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param max_results the maximum number of results to return; any number between 1 and 1000.
#'Set to 100 by default.
#'
#'@param page_token in the event that the requested files are split over multiple pages,
#'each object returned from \code{list_files} will contain an element named "nextPageToken".
#'Plugging this into the \code{page_token} parameter provides for query continuation.
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@export
list_files <- function(token, max_results = 100, page_token = NULL, simplify = FALSE, ...){
  parameters <- paste0("files?", "maxResults=", max_results)
  if(!is.null(page_token)){
    parameters <- paste0(parameters, "&pageToken=", page_token)
  }
  result <- driver_get(parameters, "file_list", token, ...)
  if(simplify){
    result <- simplify_response(result)
  }
  return(result)
}

#'@title update the time a file was viewed
#'@description \code{\link{update_file_time}} updates the metadata associated with a specific
#'file to state that the file was last viewed/modified at [system time].
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of the file; see \code{\link{file_metadata}} for further
#'commentary. 
#'
#'@param ... further arguments to pass to httr's POST.
#'
#'@return TRUE if the file was successfully deleted, an error otherwise.
#'@export
update_file_time <- function(token, file_id, ...){
  parameters <- paste0("files/", file_id, "/touch")
  driver_post(parameters, token, ...)
  return(TRUE)
}

trash_file <- function(){
  
}

untrash_file <- function(){
  
}

empty_trash <- function(){
  
}
watch_file <- function(){
  
}

