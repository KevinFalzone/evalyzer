% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_performance.R
\name{get_performance}
\alias{get_performance}
\title{Get performance information of all users}
\usage{
get_performance(session, id)
}
\arguments{
\item{session}{A rvest session. The rvest session is created by the
\code{\link{auth}} function.}

\item{id}{The id of a project. The id lies in url like
\url{https://app.evalyzer.com/evalyzer/analyzes/participants/<id>}}
}
\value{
A \code{data.frame}
}
\description{
Get performance information of all users for each task.
Information include :
\itemize{
\item task duration
\item task status
\item task duration (ms)
\item number of clicks.
\item number of scrolls,
\item number of page inputs
\item number of consulted pages,
}
}
\examples{
session <- auth("my-username", "my-password")
user_performance <- get_performance(session, "project-id")
}
