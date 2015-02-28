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

test_that("File copying works", {
  file <- list_files(token, max_results = 1)$items[[1]] #Get the original file
  copy_result <- copy_file(token, file_id = file$id) #Copy!
  expect_that(paste0("Copy of ", file$title), equals(copy_result$title)) #Expect the title is "Copy of $FOO"
  expect_that(file$mimeType, equals(copy_result$mimeType)) #Expect they have matching MIME types. Copies, right?
})

#Insert trashing/untrashing experiments here where they can take advantage of the copy which
#hasn't yet been consumed by the file deletion testing.

test_that("File deletion works",{
  file <- list_files(token, max_results = 1)$items[[1]]$id #Get the most recent fileID.
  result <- delete_file(token, file) #Of course, we just created it with "File copying works", so deleting it is fine.
  expect_true(result)
  
  
})