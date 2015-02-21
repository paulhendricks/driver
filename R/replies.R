#'@title delete a reply to a comment in a Google Drive file
#'@description when provided with the relevant file, comment and reply IDs, \code{\link{delete_reply}}
#'allows you to trash a reply. Note that this is irrevocable.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be easily retrieved with \code{\link{list_comments}}
#'
#'@param comment_id the ID of a comment, which can be easily retrieved with \code{\link{list_replies}}
#'or \code{\link{get_reply}}
#'
#'@param ... further arguments to pass to httr's DELETE
#'
#'@seealso \code{\link{get_reply}} or \code{\link{list_replies}}, for retrieving the metadata (including reply
#'ID) from a specific reply or all replies to a comment, respectively.
#'
#'@export
delete_reply <- function(token, file_id, comment_id, reply_id, ...){
  parameters <- paste0("files/", file_id, "/comments/", comment_id, "/replies/", reply_id)
  results <- driver_delete(parameters, token, ...)
  return(check_result_status(results))
}

get_reply <- function(token, file_id, comment_id, reply_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/comments/", comment_id, "/replies/", reply_id)
  results <- driver_get(parameters, "reply", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

list_replies <- function(token, file_id, comment_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/comments/", comment_id)
  results <- driver_get(parameters, "reply_list", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}