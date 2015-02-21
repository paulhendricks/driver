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