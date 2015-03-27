context("File metadata retrieval, content retrieval and timestamp-updating works")
test_that("Retrieving metadata for a list of un-simplified files works", {
  files <- list_files(max_results = 1)
  expect_true(exists("files")) #It worked?
  expect_equal(length(files$items), 1) #It respected the "only get one" element.
  expect_true(is.list(files$items[[1]]$labels)) #It didn't simplify things.
})

test_that("Retrieving metadata for a list of simplified files works", {
  files <- list_files(max_results = 1, simplify = TRUE)
  expect_true(exists("files")) #It worked?
  expect_equal(length(files$items), 1) #It respected the "only get one" element.
  expect_false(is.list(files$items[[1]]$labels)) #It DID simplify things.
})

test_that("Retrieving metadata for a single un-simplified file works", {
  file_id <- list_files(max_results = 1)$items[[1]]$id #Get file_id
  file <- file_metadata(file_id = file_id, simplify = FALSE) #Retrieve actual file
  expect_true(exists("file")) #It worked?
  expect_true(is.list(file$labels)) #It didn't simplify things.
})

test_that("Retrieving metadata for a single simplified file works", {
  file_id <- list_files(max_results = 1)$items[[1]]$id #Get file_id
  file <- file_metadata(file_id = file_id, simplify = TRUE) #Retrieve actual file
  expect_true(exists("file")) #It worked?
  expect_false(is.list(file$labels)) #It didn't simplify things.
})

test_that("Using full URLs works",{
  full_url <- list_files(max_results = 1)$items[[1]]$alternateLink
  cat(full_url)
  files <- file_metadata(file_id = full_url)
})

test_that("File copying works", {
  file <- list_files(max_results = 1)$items[[1]] #Get the original file
  copy_result <- copy_file(file_id = file$id) #Copy!
  expect_that(file$mimeType, equals(copy_result$mimeType)) #Expect they have matching MIME types. Copies, right?
})

test_that("File trashing works", {
  file_id <- list_files(max_results = 1)$items[[1]]$id #Get file_id
  trash_result <- trash_file(file_id) #Trash
  expect_true(trash_result) #It worked?
  is_trashed <- list_files(max_results = 1)$items[[1]]$labels$trashed #Get trashed status
  expect_true(is_trashed) #Which should be true.
})

test_that("File untrashing works", {
  file_id <- list_files(max_results = 1)$items[[1]]$id #Get file_id
  untrash_result <- untrash_file(file_id) #Untrash
  expect_true(untrash_result) #Untrashing /claims/ to have worked?
  is_trashed <- list_files(max_results = 1)$items[[1]]$labels$trashed #Get trashed status
  expect_false(is_trashed) #Which should be false.
})

test_that("Modifying the metadata of an existing file works",{
  file <- list_files(max_results = 1)$items[[1]] #Get the most recent file.
  file$title <- "This isn't the greatest file in the wo-orld, no. This is a tribute."
  result <- update_file_metadata(file)
  expect_equal(result$title, "This isn't the greatest file in the wo-orld, no. This is a tribute.")
})

test_that("File deletion works",{
  file <- list_files(max_results = 1)$items[[1]]$id #Get the most recent fileID.
  result <- delete_file(file) #Of course, we just created it with "File copying works", so deleting it is fine.
  expect_true(result)
})

test_that("File uploading works",{
  file <- upload_file(file_path = system.file("test.jpeg", package = "driver"), title = "This is a test")
  expect_equal(file$title, "This is a test")
  expect_equal(file$mimeType, "image/jpeg")
  result <- delete_file(file$id)
  expect_true(result)
})

test_that("File revision downloading works",{
  file <- list_files(max_results = 1)$items[[1]]$id
  file_meta <- file_metadata(file)
  format <- names(file_meta$exportLinks)[1]
  revs <- list_revisions(file)
  last_rev <- revs$items[[length(revs$items)]]
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  download_revision(file_id = file, download_type = format, version = last_rev$id, destination = tmp1)
  download_revision(file_id = file, download_type = format, version = Sys.time(), destination = tmp2)
  expect_equal(readLines(tmp1, warn = FALSE), readLines(tmp2, warn = FALSE))
})
