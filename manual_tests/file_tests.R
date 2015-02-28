context("File metadata retrieval, content retrieval and timestamp-updating works")

test_that("Retrieving metadata for a list of un-simplified files works", {
  files <- list_files(token, max_results = 1)
  expect_true(exists("files")) #It worked?
  expect_equal(length(files$items), 1) #It respected the "only get one" element.
  expect_true(is.list(files$items[[1]]$labels)) #It didn't simplify things.
})

test_that("Retrieving metadata for a list of simplified files works", {
  files <- list_files(token, max_results = 1, simplify = TRUE)
  expect_true(exists("files")) #It worked?
  expect_equal(length(files$items), 1) #It respected the "only get one" element.
  expect_false(is.list(files$items[[1]]$labels)) #It DID simplify things.
})

test_that("Retrieving metadata for a single un-simplified file works", {
  file_id <- list_files(token, max_results = 1)$items[[1]]$id #Get file_id
  file <- file_metadata(token, file_id = file_id, simplify = FALSE) #Retrieve actual file
  expect_true(exists("files")) #It worked?
  expect_true(is.list(file$labels)) #It didn't simplify things.
})

test_that("Retrieving metadata for a single simplified file works", {
  file_id <- list_files(token, max_results = 1)$items[[1]]$id #Get file_id
  file <- file_metadata(token, file_id = file_id, simplify = TRUE) #Retrieve actual file
  expect_true(exists("files")) #It worked?
  expect_false(is.list(file$labels)) #It didn't simplify things.
})