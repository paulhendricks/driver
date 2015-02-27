library(testthat)
library(driver)

id <- readline("ID?\n")
secret <- readline("Secret?\n")
token <- driver_connect(id, secret)
