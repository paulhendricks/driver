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
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be easily retrieved with \code{\link{list_comments}}
#'
#'@param ... further arguments to pass to httr's DELETE
#'
#'@seealso \code{\link{list_comments}} to retrieve comment metadata (including the comment
#'IDs) for a file.
#'@return TRUE if the comment was successfully deleted, FALSE otherwise.
#'@export
delete_comment <- function(token, file_id, comment_id, ...){
  parameters <- paste0("files/", file_id, "/comments/", comment_id)
  results <- driver_delete(parameters, token, ...)
  return(check_result_status(results))
}

#'@title retrieve a comment's metadata and content
#'@description grabs the metadata and content (both as HTML and plaintext) of a specified
#'comment to a Google Drive file.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
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
get_comment <- function(token, file_id, comment_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/comments/", comment_id)
  results <- driver_get(parameters, "comment", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

#'@title retrieve the metadata and content of all comments to a file
#'@description grabs the metadata and content (both as HTML and plaintext) of each comment
#'in a specified Google Drive file.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET
#'
#'@seealso \code{\link{list_comments}} to retrieve comment metadata (including the comment
#'IDs) for an entire file.
#'@export
list_comments <- function(token, file_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/comments")
  results <- driver_get(parameters, "comment_list", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}