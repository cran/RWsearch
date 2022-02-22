## binarydb
## @include archivedb.R



#' @title CRAN Matrix Of Available Binary Packages (archivedb.rda)
#' @description
#' \code{binarydb_down} downloads the matrix of the packages available in binary
#' format available in CRAN (Windows or macOS depending the computer), saves it
#' it in a file named \code{binarydb} and finally loads it in .GlobalEnv under
#' the name \code{binarydb}. It is a wrapper around the function
#' \code{available.packages(type = "binary")}.
#'
#' \code{binarydb_load} loads the file \code{filename} in .GlobalEnv under the
#' name \code{binarydb}.
#'
#' When loaded in .GlobalEnv, \code{binarydb} is recognized by the functions
#' \code{\link{p_vers}} and \code{\link{p_vers_deps}} to compare the package
#' version numbers of the installed packages with the most recent versions of
#' the binary and the source versions available in CRAN.
#'
#'
#' @param   dir         character. The directory where "binarydb.rda" is saved
#'                      Default value \code{"."} is the current directory.
#' @param   repos       character. The address of your local CRAN.
#' @param   filename    character. The (path to a) file "binarydb.rda" or an equivalent.
#'
#' @name binarydb
NULL

#' @export
#' @name binarydb
binarydb_down <- function(dir = ".", repos = getOption("repos")[1]) {
    binarydb <- available.packages(method = "libcurl", type = "binary", repos = repos)
    save(binarydb, file = file.path(dir, "binarydb.rda"),
         compress = "xz", compression_level = 6)
    binarydb <<- binarydb
}

#' @export
#' @rdname binarydb
binarydb_load <- function(filename = "binarydb.rda") {
    if (file.exists(filename)) {
        load(filename, envir = .GlobalEnv)
    } else {
        stop(paste("File", basename(filename), "does not exist in this directory."))
    }
}

