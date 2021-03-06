% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/replies.R
\name{delete_reply}
\alias{delete_reply}
\title{delete a reply}
\usage{
delete_reply(file_id, comment_id, reply_id, ...)
}
\arguments{
\item{file_id}{the ID of a file - or the full URL for accessing it via your browser.
See \code{\link{file_metadata}} for further discussion.}

\item{comment_id}{the ID of a comment, which can be easily retrieved with \code{\link{list_comments}}}

\item{reply_id}{the ID of a comment, which can be easily retrieved with \code{\link{list_replies}}
or \code{\link{get_reply}}}

\item{...}{further arguments to pass to httr's DELETE}
}
\description{
when provided with the relevant file, comment and reply IDs, \code{\link{delete_reply}}
allows you to trash a reply. Note that this is irrevocable.
}
\seealso{
\code{\link{get_reply}} or \code{\link{list_replies}}, for retrieving the metadata (including reply
ID) from a specific reply or all replies to a comment, respectively.
}

