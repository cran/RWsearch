% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_archive.R
\name{p_archive}
\alias{p_archive}
\alias{p_archive_lst}
\alias{l_targz}
\title{Read Packages in CRAN archive}
\usage{
p_archive(..., char = NULL)

p_archive_lst(..., char = NULL,
  url = "https://cran.r-project.org/src/contrib/Archive")

l_targz(lst, before = Sys.Date())
}
\arguments{
\item{...}{any format recognized by \code{\link{cnsc}}, except list.
A vector of packages.}

\item{char}{(name to) a character vector. Use this argument if \code{...} fails 
or if you call the function from another function. If used, 
argument \code{...} is ignored.}

\item{url}{character. The url address of CRAN archive html file.}

\item{lst}{list. A list produced by \code{p_archive_lst}.}

\item{before}{character which can be converted to a Date, for instance 
"2017-05-14". Extract from CRAN archive the package(s) 
available before this date. Can be synchronized with
the release dates of base-R versions listed at: 
\url{https://CRAN.R-project.org/src/contrib/} and 
\url{https://CRAN.R-project.org/package=rversions/readme/README.html}}
}
\description{
\code{p_archive} opens in the browser one page per package and displays 
the package versions stored in CRAN archive. 

\code{p_archive_lst} prints in the console a list of the package versions 
stored in CRAN archive. 

Use \code{\link{l_targz}} takes as input the list obtained from \code{p_archive_lst}
and extracts the packages before a certain date (Default is today).

Use \code{\link{p_downarch}} to download packages from CRAN archive, either the 
latest version or a specific version number.

Use \code{\link{archivedb_list}} to list all packages stored in CRAN archive 
(does not include the valid packages having a single version which are stored 
in regular CRAN only).
}
\examples{
\donttest{
p_archive(brew, RWsearch)
}
lst <- p_archive_lst(brew, pacman, RWsearch, fitur, zmatrix, NotAPkg) ; lst
l_targz(lst, before = "2017-05-14")

}
