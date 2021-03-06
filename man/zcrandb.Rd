% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_intro.R
\docType{data}
\name{zcrandb}
\alias{zcrandb}
\title{File zcrandb.rda: A Subset of crandb Dataset}
\description{
File \emph{zcrandb.rda} loads in .Globalenv as \code{crandb}, a data.frame of dim 50 x 65. 
It contains 50 packages that match the keywords used in the examples of this package. 

File \emph{zcrandb.rda}, 22 ko, acts as a replacement of the original but large file 
\emph{crandb.rda} to be downloaded from CRAN. The weight of \emph{crandb.rda} was 
4.3 Mo with 13001 packages on August 31, 2018 and 6.7 Mo with 13902 packages on 
March 17, 2019 . The use of \code{zcrandb.rda} avoids inappropriate connections 
to CRAN and increases the speed in the examples.
}
\examples{
crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
}
\keyword{datasets}
