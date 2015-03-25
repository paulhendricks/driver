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
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'@export
list_revisions <- function(file_id, simplify = FALSE, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/revisions")
  results <- driver_get(parameters, "rev_list", ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

#'@title Get the metadata of a single revision of a Google Drive file
#'@description retrieves the metadata associated with a particular revision of a specific
#'Google Drive file.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param rev_id the ID of a revision of that file.
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@export
revision_metadata <- function(file_id, rev_id, simplify = FALSE, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/revisions/", rev_id)
  result <- driver_get(parameters, "rev_metadata", ...)
  if(simplify){
    result <- simplify_response(result)
  }
  return(result)
}

#'@title remove a particular revision of a Google Drive file
#'@description junks a specified revision of a Google Drive file. Note that some file types
#'may not support revision deletion, in which case a 400 error will be returned.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param rev_id the ID of a revision of that file.
#'
#'@param ... further arguments to pass to httr's DELETE.
#'
#'@export
delete_revision <- function(file_id, rev_id, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/revisions/", rev_id)
  result <- driver_delete(parameters, ...)
  return(check_result_status(result))
}

#'@title Download a specific revision of a Google Drive file
#'@description download a specific revision of a Google Drive file in
#'a user-specified format, and save it to disk.
#'
#'@param metadata a metadata object retrieved from \code{\link{revision_metadata}},
#'\code{\link{list_files}}, or \code{\link{file_metadata}}. If the latter, \code{version}
#'must be supplied
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser. May be provided
#'in place of \code{metadata}.  If used, \code{version} must be supplied
#'
#'@param version The version number of a google doc (found in the "id" of version metadata)
#'or a date value that can be coerced to POSIXct.  If a date/time is provided, the last version
#'saved prior to that time is returned.
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
#'@export
download_revision <- function(metadata=NULL, download_type, destination, file_id=NULL, version = NULL, overwrite=TRUE, ...) {
  
  if(is.null(metadata) & is.null(file_id)) {
    stop("file_id or metadata must be provided.")
  }
  
  if(is.null(file_id)) {
    file_id = detect_full_url(file_id)
  }
  
  if(class(metadata) != "rev_metadata") {
    if(class(metadata) == "file_metadata") file_id <- metadata$id
    
    version_ <- try(as.POSIXct(version), silent=TRUE)
    if("try-error" %in% class(version_)) {
      version_ <- version
    } else {
      attributes(version_)$tzone <- "UTC"
    }
    
    file_id <- detect_full_url(file_id)
    if(any("POSIXct" %in% class(version_))) {
      rev_list <- list_revisions(file_id = file_id, ...)
      dates <- sapply(rev_list$items, function(x) x$modifiedDate)
      dates <- as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
      version_index <- suppressWarnings(max(which(dates <= version_)))
      if(version_index == -Inf) stop('File created before date provided')
      metadata <- rev_list$items[[version_index]]
    } else {
      metadata <- revision_metadata(file_id, version_)
    }
  }
  
  
  download_url <- unlist(unname(metadata$exportLinks[names(metadata$exportLinks) == download_type]))
  if (is.null(download_url)) {
    download_url <- metadata$selfLink
  }
  
  driver_get(download_url, out_class = NULL, write_disk(destination, overwrite), ...)
  return(TRUE)
}