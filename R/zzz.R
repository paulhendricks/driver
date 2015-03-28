.onLoad <- function(libname, pkgname) {
  op <- options()
  op.driver <- list(
    driver.client_id = "472848037940-d725mm1ntr7qae8cs8ujdv0sbh9008c2.apps.googleusercontent.com",
    driver.client_secret = "ZSB9r0Z_PupOAHm6Bwrsysqt"
  )
  toset <- !(names(op.driver) %in% names(op))
  if(any(toset)) options(op.driver[toset])
  invisible()
}