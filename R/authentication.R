#Environment to hold credentials.
.state <- new.env(parent = emptyenv())

#'@title connect to Google Drive and authorise driver to access your files.
#'@description \code{driver_connect} produces an OAuth 2.0 token which can be passed
#'into other functions in the driver package, enabling driver to access your Google Drive
#'files on your behalf.
#'
#'#'
#'@examples
#'\dontrun{
#'#Use driver_connect to generate a token which is than quietly saved and called internally
#'in other driver functions.
#'driver_connect()
#'}
#'
#'@importFrom httr oauth_app oauth_endpoints oauth2.0_token
#'@export
driver_connect <- function(){
  app <- oauth_app("driver", getOption("driver.client_id"), getOption("driver.client_secret"))
  .state$driver_token <- oauth2.0_token(oauth_endpoints("google"), app = app,
                                        scope = "https://www.googleapis.com/auth/drive", cache = FALSE)
  return(invisible())
}

#Retrieve a token.
get_driver_token <- function(){
  if(is.null(.state$driver_token)) {
    driver_connect()
  }
  return(.state$driver_token)
}