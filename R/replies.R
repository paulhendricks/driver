#'@title delete a reply
#'@description when provided with the relevant file, comment and reply IDs, \code{\link{delete_reply}}
#'allows you to trash a reply. Note that this is irrevocable.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be easily retrieved with \code{\link{list_comments}}
#'
#'@param reply_id the ID of a comment, which can be easily retrieved with \code{\link{list_replies}}
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

#'@title get a reply's metadata and text
#'
#'@description retrieve the metadata and text for a specific reply to a comment in a
#'Google Drive file.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be easily retrieved with \code{\link{list_comments}}
#'
#'@param reply_id the ID of a comment, which can be found in the output of \code{\link{list_replies}}
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET
#'
#'@seealso \code{\link{delete_reply}} for deleting a reply, and \code{\link{list_replies}}
#'for retrieving all replies associated with a comment
#'
#'@export
get_reply <- function(token, file_id, comment_id, reply_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/comments/", comment_id, "/replies/", reply_id)
  results <- driver_get(parameters, "reply", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

#'@title Get all replies to a comment
#'@description retrieves the metadata and text of all replies to a comment in a
#'Google Drive file.
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be found in the output of \code{\link{list_comments}}
#'
#'@param simplify whether or not to perform some (small) simplification of the returned
#'list, to make it less nested, headachey and impossible to read. Set to FALSE by default.
#'
#'@param ... further arguments to pass to httr's GET
#'
#'@seealso \code{\link{delete_reply}} for deleting a reply, and \code{\link{get_reply}}
#'for retrieving a specific reply.
#'
#'@export
list_replies <- function(token, file_id, comment_id, simplify = FALSE, ...){
  parameters <- paste0("files/", file_id, "/comments/", comment_id)
  results <- driver_get(parameters, "reply_list", token, ...)
  if(simplify){
    results <- simplify_response(results)
  }
  return(results)
}

#'@title Reply to a comment
#'
#'@description replies to a specific comment on a Google Drive file
#'
#'@param token a token, generated with \code{\link{driver_connect}}.
#'
#'@param file_id the ID of a file - see \code{\link{file_metadata}} for further discussion.
#'
#'@param comment_id the ID of a comment, which can be found in the output of \code{\link{list_comments}}
#'
#'@param reply_text a string containing the reply you wish to add
#'
#'@param ... further arguments to pass to httr's POST
#'
#'@return a basic reply object, similar to those returned from \code{\link{get_reply}}
#'
#'@seealso \code{\link{upload_file}} for uploading a file, and \code{\link{add_comment}} for adding
#'a new comment
#'
#'@export
add_reply <- function(token, file_id, comment_id, reply_text, ...){
  result <- driver_post(paste0("files/",file_id,"/comments/",comment_id,"/replies"),
                        token, body = list(content = reply_text), encode = "json", ...)
  return(result)
}