% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkdb.R
\name{checkdb}
\alias{checkdb}
\alias{checkdb_down}
\alias{checkdb_load}
\title{CRAN checks file (check_results.rds)}
\usage{
checkdb_down(dir = ".", repos = getOption("repos")[1])

checkdb_load(filename = "check_results.rds")
}
\arguments{
\item{dir}{character. The directory where \code{filename} or tar.gz files
are saved. Default value \code{"."} is the current directory.}

\item{repos}{character. The address of your local CRAN.}

\item{filename}{character. The path to file "check_results.rds" (or equivalent).}
}
\description{
\code{checkdb_down} downloads from CRAN the file \emph{check_results.rds},
saves it unchanged in the designated directory and loads it in .GlobalEnv
under the name \code{checkdb}.

\code{checkdb_load} loads the file \code{check_results.rds} in .GlobalEnv
under the name \code{checkdb}.

\code{checkdb} is a data.frame of dimension 200000 x 10 (approximatively). It is
used by the functions \code{\link{p_check_lst}} and \code{\link{p_checkdeps_lst}}.
}
\examples{
### zcheck_results.rds is a subset of 107 packages synchronized with zcrandb.

checkdb_load(system.file("aabb", "zcheck_results.rds", package = "RWsearch"))
dim(checkdb)
head(checkdb, 15)
unique(checkdb$Package)

}
