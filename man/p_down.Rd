% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_down.R
\name{p_down}
\alias{p_down}
\alias{p_down0}
\alias{p_downarch}
\alias{targz_down}
\title{Download the Package Documentation in One Directory or in Several Subdirectories}
\usage{
p_down(..., char = NULL, index = TRUE, manual = TRUE, vignettes = TRUE,
  RqmdRmd = FALSE, README = TRUE, NEWS = FALSE, ChangeLog = FALSE,
  checks = FALSE, targz = FALSE, untar = FALSE, binary = FALSE,
  type = "binary", script = FALSE, dir = ".", crandb = get("crandb",
  envir = .GlobalEnv), repos = getOption("repos")[1])

p_down0(..., char = NULL, index = FALSE, manual = FALSE,
  vignettes = FALSE, RqmdRmd = FALSE, README = FALSE, NEWS = FALSE,
  ChangeLog = FALSE, checks = FALSE, targz = FALSE, untar = FALSE,
  binary = FALSE, type = "binary", script = FALSE, dir = ".",
  crandb = get("crandb", envir = .GlobalEnv),
  repos = getOption("repos")[1])

p_downarch(..., char = NULL, before = Sys.Date(), dir = ".",
  untar = FALSE, url = "https://cran.r-project.org/src/contrib/Archive")

targz_down(ptargz, dir = ".", untar = FALSE,
  url = "https://cran.r-project.org/src/contrib/Archive")
}
\arguments{
\item{...}{any format recognized by \code{\link{cnsc}}, including list.
A vector or packages or a named list of packages (with names
being the keywords).}

\item{char}{(name to) a character vector or a list. Use this argument if
\code{...} fails or if you call the function from another function.
If used, argument \code{...} is ignored.}

\item{index}{logical. Download the html index page of each package.}

\item{manual}{logical. Download the pdf manual.}

\item{vignettes}{logical. Download the html and pdf vignettes, if they exist.}

\item{RqmdRmd}{logical. Download the R, Rmd and qmd source files of the vignettes, if they exist.}

\item{README}{logical. Download the README file, if it exists.}

\item{NEWS}{logical. Download the NEWS file, if it exists.}

\item{ChangeLog}{logical. Download the ChangeLog file, if it exists.}

\item{checks}{logical. Download the CRAN checks file.}

\item{targz}{logical. Download the *.tar.gz source file.}

\item{untar}{logical. Decompress the downloaded tar.gz file.}

\item{binary}{logical. Download the *.tgz (Mac OSX) or *.zip (Windows)
binary file, depending the \code{type} value.}

\item{type}{character. Either \code{"mac.binary"}, \code{"mac.binary.el-capitan"},
or \code{"win.binary"}. The default, \code{"binary"},
automatically detects the local OS and the variants
between R-3.6.3, R-4.0.0 or (for Windows) gcc8. See the
\code{type} section of \code{utils::install.packages}.}

\item{script}{logical. Create a mini-script and save it in a *.R file.}

\item{dir}{character. The directory in which the files are saved.
Default value \code{"."} is the current directory.}

\item{crandb}{data.frame \code{crandb}. The data.frame of CRAN packages.}

\item{repos}{character. The address of your local CRAN.}

\item{before}{character which can be converted to a Date, for instance
"2017-05-14". Extract from CRAN archive the package(s)
available before this date. Can be synchronized with
the release dates of base-R versions listed at:
\url{https://CRAN.R-project.org/src/contrib/} and
\url{https://CRAN.R-project.org/package=rversions/readme/README.html}}

\item{url}{character. The url address of CRAN archive html file.}

\item{ptargz}{character. A vector of package(s) with their version number
and tar.gz extension stored in CRAN archive. These packages
can be identified with \code{\link{l_targz}}.}
}
\description{
If \code{pkgs} is a vector of packages obtained from \code{\link{s_crandb}},
\code{p_down} downloads from CRAN and saves in the \code{dir} directory (by default
the current directory) the index page, the manual, the vignettes, the README, NEWS,
ChangeLog, CRAN checks files, the source code in \emph{pkg_ver.tar.gz} format,
the binary code in \emph{pkg_ver.tgz} (Mac OSX) or \emph{pkg_ver.zip} (Windows)
format and a minimal R-script of each package. The files that do not exist are
ignored, with no warning.

If \code{pkgs} is a list of packages obtained from \code{\link{s_crandb_list}},
\code{p_down} saves the downloaded files in subdirectories named after the names
of the list, e.g. the keywords used at the search step. The names are
eventually modified with \code{gsub(".", "_", make.names(pkg), fixed = TRUE)}
to cope with Unix and Windows directory names.

\code{p_down0} calls \code{p_down} with different values for each argument.
With the default configuration, this function downloads nothing. It is mostly
used to download one specific item which has not been previously downloaded.

\code{p_downarch} downloads from CRAN archive the tar.gz file of one or several
packages, either the last version(s) with \code{Sys.Date()} or the version(s)
before a given date. It combines 3 functions: \code{\link{p_archive_lst}}
lists the packages stored in CRAN archive and their version numbers,
\code{\link{l_targz}} extracts the appropriate version numbers available
before a given date, \code{targz_down} downloads the tar.gz files in the selected
directory (default is the current directory) and eventually decompresses it.

\code{targz_down} downloads the tar.gz files from CRAN archive to the selected
directory (default is the current directory) and eventually decompresses it.
If \code{url = "https://cran.r-project.org/src/contrib"}, \code{targz_down}
will take the latest version of the package.
}
\examples{
## In real life, download crandb from CRAN or load it from your directory
## with functions crandb_down() or crandb_load().
## In this example, we use a small file.
crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
\donttest{
## Download the documentation in the "dirpkgs" directory. Flat representation.
dir <- file.path(tempdir(), "dirpkgs")
p_down(RWsearch, pdfsearch, sos, dir = dir, repos = "https://cloud.r-project.org")
list.files(dir, recursive = TRUE, full.names = TRUE)

## Download the documentation in subdirectories named after the keywords.
dir   <- file.path(tempdir(), "dirpkgslist")
(lst  <- s_crandb_list(thermodynamic, "chemical reaction"))
(lst2 <- lapply(lst, function(x) x[1:2]))
system.time(
  p_down(lst2, dir = dir, repos = "https://cloud.r-project.org")
)
list.files(dir, recursive = TRUE, full.names = TRUE)

## Download tar.gz files stored in CRAN archive.
dir <- file.path(tempdir(), "targzip")
p_downarch(fitur, zmatrix, NotAPkg, before = "2017-05-14", dir = dir)
targz_down("SVN_1.0.tar.gz", dir = dir, untar = TRUE)
list.files(dir, recursive = TRUE, full.names = TRUE)
}
}
