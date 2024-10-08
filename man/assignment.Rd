% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/communities.R
\name{assignment}
\alias{assignment}
\alias{assignment.communities}
\alias{assignment.igraph}
\title{Extract assignment of graph vertices to communities.}
\usage{
assignment(x, ...)

\method{assignment}{communities}(x, ...)

\method{assignment}{igraph}(x, ...)
}
\arguments{
\item{x}{an object of class \code{communities} or \code{igraph}.}

\item{...}{extra arguments passed to methods.}
}
\value{
a tibble.
}
\description{
Retrieves assignment of graph nodes to communities in a data frame.
The first column specifies the vertex index. The remaining columns are
name (if present) and community identifier.
}
\details{
If no community assignment can be retrieved from an \code{igraph} object,
\code{NULL} is returned with a warning.
}
