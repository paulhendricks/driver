#Comment-based simplification methods.
#First, the actual logic
comment_simp <- function(x){
  x$author <- unlist(x$author)
  x$context <- unlist(x$context)
  return(x)
}

#Second, how to handle a single comment, from get_comment
simplify_response.comment <- function(x){
  return(comment_simp(x))
}

#Third, a comment_list from list_comments
simplify_response.comment_list <- function(x){
  if(length(x$items) == 1){
    x$items <- comment_simp(x$items[[1]])
  } else {
    x$items <- lapply(x$items, comment_simp)
  }
  return(x)
}

#'@title delete a comment on a Google Drive file
#'@description \code{delete_comment}, when provided with an authentication token, a file ID
#'and a comment ID, will let you delete a comment automatically. It's worth noting that this will
#'/also/ junk any replies to the initial comment.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be easily retrieved with \code{\link{list_comments}}
#'
#'@param ... further arguments to pass to httr's DELETE
#'
#'@seealso \code{\link{list_comments}} to retrieve comment metadata (including the comment
#'IDs) for a file.
#'
#'@return TRUE if the comment was successfully deleted, FALSE otherwise.
#'
#'@examples
#'\dontrun{
#'#Grab a list of comments in a file and delete the first one
#'comments <- list_comments(file_id = "f38rggruFKJad30")
#'delete_comment("f38rggruFKJad30", comments$items[[1]]$id)
#'}
#'@export
delete_comment <- function(file_id, comment_id, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/comments/", comment_id)
  results <- driver_delete(parameters, ...)
  return(check_result_status(results))
}

#'@title retrieve a comment's metadata and content
#'@description grabs the metadata and content (both as HTML and plaintext) of a specified
#'comment to a Google Drive file.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be easily retrieved with \code{\link{list_comments}}
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET
#'
#'@seealso \code{\link{list_comments}} to retrieve comment metadata (including the comment
#'IDs) for an entire file.
#'@export
get_comment <- function(file_id, comment_id, simplify = FALSE, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/comments/", comment_id)
  results <- driver_get(parameters, "comment", ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

#'@title retrieve the metadata and content of all comments to a file
#'@description grabs the metadata and content (both as HTML and plaintext) of each comment
#'in a specified Google Drive file.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET
#'
#'@seealso \code{\link{list_comments}} to retrieve comment metadata (including the comment
#'IDs) for an entire file.
#'@export
list_comments <- function(file_id, simplify = FALSE, ...){
  parameters <- paste0("files/", detect_full_url(file_id), "/comments")
  results <- driver_get(parameters, "comment_list", ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

#'@title comment on a Google Drive file
#'
#'@description add a comment to a Google Drive file. These comments are not (currently) anchored,
#'meaning that they're associated with the file as a whole rather than tied to any particular element
#'or line of text.
#'
#'@param file_id the ID of a file - or the full URL for accessing it via your browser.
#'See \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_text a string containing the comment you wish to leave
#'
#'@param ... further arguments to pass to httr's POST
#'
#'@return a basic comment object, similar to those returned from \code{\link{get_comment}}
#'
#'@seealso \code{\link{upload_file}} for uploading a file, and \code{\link{add_reply}} for replying
#'to an existing comment.
#'
#'@examples
#'\dontrun{
#'file_id <- list_files(token, max_results = 1)$items[[1]]$id
#'comment_metadata <- add_comment(token, file_id, comment = text = "I have strong negative opinions
#'                                about this proposal but no actual idea for how to improve it")
#'}
#'@export
add_comment <- function(file_id, comment_text, ...){
  result <- driver_post(paste0("files/",file_id,"/comments"), body = list(content = comment_text), encode = "json", ...)
  return(result)
}