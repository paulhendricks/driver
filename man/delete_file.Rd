% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/files.R
\name{delete_file}
\alias{delete_file}
\title{delete a Google Drive file}
\usage{
delete_file(file_id, ...)
}
\arguments{
\item{file_id}{the ID of a file - or the full URL for accessing it via your browser.
See \code{\link{file_metadata}} for further discussion.}

\item{...}{further arguments to pass to httr's DELETE.}
}
\value{
TRUE if the file was successfully deleted, FALSE or an error otherwise.
}
\description{
\code{delete_file} removes a file completely, assuming the user has
permission to do so. In the process it completely bypasses the trash bin, rendering
the file unrecoverable by the user.
}

