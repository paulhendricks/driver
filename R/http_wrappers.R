#'@title send a GET request to the Google Drive API
#'
#'@param parameters a constructed query string passed from one of the exported functions.
#'
#'@param out_class a classname to set, used to distinguish between simplifier methods
#'
#'@param ... further arguments to httr's GET function.
#'
#'@importFrom httr GET
driver_get <- function(parameters, out_class, ...){
  result <- GET(create_url(parameters), config(token = get_driver_token(), useragent = "driver - https://github.com/Ironholds/driver"),
                ...)
  stop_for_status(result)
  result <- content(result)
  class(result) <- out_class
  return(result)
}

#'@title send a DELETE request to the Google Drive API
#'
#'@param parameters a constructed query string passed from one of the exported functions.
#'
#'@param ... further arguments to httr's DELETE function.
#'
#'@importFrom httr DELETE
driver_delete <- function(parameters, ...){
  result <- DELETE(create_url(parameters), config(token = get_driver_token(), useragent = "driver - https://github.com/Ironholds/driver"))
  stop_for_status(result)
  return(result)
}

#'@title send a POST request to the Google Drive API
#'
#'@param parameters a constructed query string passed from one of the exported functions.
#'
#'@param ... further arguments to httr's POST function.
#'
#'@importFrom httr POST
driver_post <- function(parameters, ...){
  result <- POST(create_url(parameters), config(token = get_driver_token(), useragent = "driver - https://github.com/Ironholds/driver"),
                 ...)
  stop_for_status(result)
  result <- content(result)
  return(result)
}

#'@title send a PUT request to the Google Drive API
#'
#'@param parameters a constructed query string passed from one of the exported functions.
#'
#'@param ... further arguments to httr's PUT function.
#'
#'@importFrom httr PUT
driver_put <- function(parameters, ...){
  result <- PUT(create_url(parameters), config(token = get_driver_token(), useragent = "driver - https://github.com/Ironholds/driver"), ...)
  stop_for_status(result)
  result <- content(result)
  return(result)
}