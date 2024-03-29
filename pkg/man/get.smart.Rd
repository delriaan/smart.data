% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smart_functions.R
\name{get.smart}
\alias{get.smart}
\title{Get a Smart Object}
\usage{
get.smart(..., list.only = FALSE)
}
\arguments{
\item{...}{Names of the smart objects (as found in class member \code{name}) given as symbols or strings}

\item{list.only}{(logical | FALSE) When \code{TRUE} existing keys of the smart-cache are returned before exiting}
}
\value{
A list of retrieved \code{smart.data} objects, invisibly.
}
\description{
\code{get.smart} retrieves \code{\link{smart.data}} objects from the global smart-cache or throws a message if the global cache does not exist
}
