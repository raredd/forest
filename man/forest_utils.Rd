% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{forest_utils}
\alias{forest_utils}
\alias{add_reference}
\alias{merge_forest}
\alias{prepare_forest}
\title{Forest utilities}
\usage{
add_reference(x, header = FALSE, keep_strata = FALSE, total = NULL)

merge_forest(x, y)

prepare_forest(x)
}
\arguments{
\item{x, y}{objects}

\item{header}{a character vector of header labels for each variable in
the model}

\item{keep_strata}{logical; if \code{FALSE} (default), strata variables,
e.g., \code{y ~ strata(a) + b} will be ignored}

\item{total}{optional total sample size, useful if model excludes
observations with missing data}
}
\description{
Not recommended to be called by the user.
}