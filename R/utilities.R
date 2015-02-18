#Creates a Google Drive APIv2 URL. Little wrapper around paste0().
create_url <- function(parameters){
  return(paste0("https://www.googleapis.com/drive/v2/",parameters))
}

#For DELETE calls and similar, checks if the status code is within an acceptable range and returns a boolean.
check_result_status <- function(result){
  if(result$status_code %in% c(200, 202, 204)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'@title connect to Google Drive and authorise driver to access your files.
#'@description \code{driver_connect} produces an OAuth 2.0 token which can be passed
#'into other functions in the driver package, enabling driver to access your Google Drive
#'files on your behalf.
#'
#'A necessary prerequisite is creating a client app:
#'
#' - Go to \url{https://console.developers.google.com}
#' - Create a client app, with http://localhost:1410/ as the redirect URI
#' - Connect it to the Drive API.
#' - note the provided clientID and secret.
#'
#'@param id the clientID of the client app you've created (see above).
#'
#'@param secret the secret of the client app you've created (see above).
#'
#'@return an OAuth token that can be passed into other function calls.
#'
#'@examples
#'\dontrun{
#'#Use driver_connect to generate a token, pass that token in to list_files
#'token <- driver_connect("foo", "bar")
#'}
#'
#'@importFrom httr oauth_app oauth_endpoints oauth2.0_token
#'@export
driver_connect <- function(id, secret){
  app <- oauth_app("driver", id, secret)
  token <- oauth2.0_token(oauth_endpoints("google"), app = app,
                          scope = "https://www.googleapis.com/auth/drive", cache = FALSE)
  return(token)
}

#Base simplifier function, calling class-specific methods. Those are stored
#in the relevant file (so, file_list's simplifier lives in files.R)
simplify_response <- function(x){
  UseMethod("simplify_response", x)
}