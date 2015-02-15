driver_get <- function(url, ...){
  result <- GET(url, user_agent("driver - https://github.com/Ironholds/driver"))
  stop_for_status(result)
  return(result)
}

driver_post <- function(url, ...){
  result <- POST(url, user_agent("driver - https://github.com/Ironholds/driver"))
  stop_for_status(result)
  return(result)
}