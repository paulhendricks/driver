#Simplifiers for file content. This is the actual workhorse function.
file_simp <- function(x){
  x$labels <- unlist(x$labels)
  x$exportLinks <- unlist(x$exportLinks)
  x$userPermission <- unlist(x$userPermission)
  x$ownerNames <- unlist(x$ownerNames)
  return(x)
}

#And this is the wrapper for file lists.
simplify_response.file_list <- function(x){
  x$items <- lapply(x$items, file_simp)
  return(x)
}

#...and THIS is the wrapper for individual files!
simplify_response.file_metadata <- function(x){
  return(file_simp(x))
}

#'@title Retrieve the metadata for a specific file.
#'
#'@description \code{file_metadata} retrieves the metadata for a specific file the user
#'has access to. To retrieve the metadata for all files, see \code{\link{list_files}}
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id a file ID, as a string. This can be retrieved from the URL bar when you're accessing
#'the file: for example, "https://docs.google.com/document/d/1gOxog56F2bCnxwum7VhmN3JqTX7usTYcK5X3V4QDnxg"
#'has the file_id "1gOxog56F2bCnxwum7VhmN3JqTX7usTYcK5X3V4QDnxg". Alternately, you can pass in
#'the full URL, and driver will do its best to extract the ID.
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
  parameters <- paste0("files/", detect_full_url(file_id))
  result <- driver_get(parameters, "file_metadata", token, ...)
  if(simplify){
    result <- simplify_response(result)
  }
  return(result)
}

#'@title copy a Google Drive file
#'@description takes a Google Drive file and creates a copy of it, with the same
#'access restrictions.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param ... further arguments to pass to httr's POST.
#'
#'@return a set of metadata associated with the copy of the file, matching
#'the output of \code{\link{file_metadata}}.
#'
#'@export
copy_file <- function(token, file_id, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/copy")
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
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param ... further arguments to pass to httr's DELETE.
#'
#'@return TRUE if the file was successfully deleted, FALSE or an error otherwise.
#'
#'@export
delete_file <- function(token, file_id, ...){
  parameters <- paste0("files/", detect_full_url(file_id))
  result <- driver_delete(parameters, token)
  return(check_result_status(result))
}

#'@title Retrieve the metadata for all files
#'
#'@description \code{list_files} allows an authenticated user to retrieve the metadata
#'associated with each file they have access to. For the metadata for a single file, see
#'\code{\link{file_metadata}}.
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
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param ... further arguments to pass to httr's POST.
#'
#'@return TRUE if the file was successfully updated, an error otherwise.
#'@export
update_file_time <- function(token, file_id, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/touch")
  driver_post(parameters, token, ...)
  return(TRUE)
}

#'@title update the metadata associated with a Google Drive file
#'
#'@description \code{update_file_metadata} allows you to update the metadata associated
#'with a file on Google Drive - for example, changing the title or description, or modifying
#'the permissions.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param metadata a blob of metadata retrieved from \code{\link{file_metadata}}, with your modifications
#'made to it.
#'
#'@param ... further arguments to pass to httr's PATCH.
#'
#'@return the new metadata associated with the file, allowing you to confirm the changes took effect.
#'
#'@export
update_file_metadata <- function(token, metadata, ...){
  patch_result <- driver_put(paste0("files/",metadata$id), token, body = metadata, encode = "json")
  return(patch_result)
}

#'@title move a file to the trash
#'@description moves a file to Google Drive's "trash" folder, which is automatically
#'emptied after a set number of days.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of the file; see \code{\link{file_metadata}} for further
#'commentary. 
#'
#'@param ... further arguments to pass to httr's POST.
#'
#'@return TRUE if the file was successfully trashed, an error otherwise.
#'@export
trash_file <- function(token, file_id, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/trash")
  driver_post(parameters, token, ...)
  return(TRUE)
}

#'@title move a file out of the trash
#'@description moves a file in Google Drive's "trash" folder, which is automatically
#'emptied after a set number of days, out and back into the user's Drive.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param ... further arguments to pass to httr's POST.
#'
#'@return TRUE if the file was successfully untrashed, an error otherwise.
#'@export
untrash_file <- function(token, file_id, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/untrash")
  driver_post(parameters, token, ...)
  return(TRUE)
}

#'@title Empties the trash
#'@description empties the user's Google Drive "trash" folder.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param ... further arguments to pass to httr's DELETE.
#'
#'@return TRUE if the trash was successfully emptied, FALSE otherwise.
#'@export
empty_trash <- function(token, ...){
  parameters <- "files/trash"
  result <- driver_delete(parameters, token, ...)
  return(check_result_status(result))
}

#'@title Download a Google Drive file
#'@description download a Google Drive file in a specified format and save it to disk.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param metadata a metadata object retrieved from \code{\link{file_metadata}} or
#'\code{\link{list_files}}.
#'
#'@param download_type the format to download the file in. Available formats for a specific file
#'can be found in the "exportLinks" field of a metadata object.
#'
#'@param destination a file path to write the downloaded file to.
#'
#'@param overwrite whether to overwrite any existing file at \code{destination}. Set to TRUE
#'by default.
#'
#'@param ... any further arguments to pass to httr's GET.
#'
#'@return TRUE if the file could be downloaded, FALSE or an error otherwise.
#'@importFrom httr write_disk
#'@export
download_file <- function(token, metadata, download_type, destination, overwrite = TRUE, ...){
  download_url <- unlist(unname(metadata$exportLinks[names(metadata$exportLinks) == download_type]))
  result <- GET(download_url, config(token = token, useragent = "driver - https://github.com/Ironholds/driver"),
                write_disk(destination), ...)
  return(check_result_status(result))
}

#'@title Upload a local file to Google Drive
#'
#'@description \code{upload_file} allows you to upload a locally-stored file to Google Drive, setting title and
#'description along the way if you so choose.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_path the full path to the file you want to upload
#'
#'@param title what to set in the "title" field of the resulting Google Drive file
#'
#'@param description what to set in the "description" field.
#'
#'@param ... further arguments to pass to httr's POST.
#'
#'@return a metadata object referring to the uploaded file, which can be used in subsequent requests. Matches the
#'output format of \code{\link{file_metadata}}
#'
#'@importFrom httr upload_file
#'@export
upload_file <- function(token, file_path, title = NULL, description = NULL, ...){
  post_result <- driver_post(parameters = "https://www.googleapis.com/upload/drive/v2/files?uploadType=media", 
                             token = token, body = httr::upload_file(file_path), ...)
  if(all(is.null(title), is.null(description))){
    return(post_result)
  }
  if(!is.null(title)){
    post_result$title <- title
  }
  if(is.null(description)){
    post_result$description <- description
  }
  patch_result <- driver_put(paste0("files/",post_result$id), token, body = post_result, encode = "json")
  return(patch_result)
}