.onLoad <- function(libname, pkgname) {
  op <- options()
  op.driver <- list(
    driver.client_id = "472848037940-veu8isq38600ojq8c062ae8d4j2p2dl5.apps.googleusercontent.com",
    driver.client_secret = "K7juAmdqRAhEER5RjJjpopXq"
  )
  toset <- !(names(op.driver) %in% names(op))
  if(any(toset)) options(op.driver[toset])
  invisible()
}