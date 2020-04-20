## checkdb
## @include cnsc.R



#' @title CRAN checks file (check_results.rds)
#' @description
#' \code{checkdb_down} downloads from CRAN the file \emph{check_results.rds}, saves 
#' it unchanged in the designated directory, by default in the current directory,
#' then loads it in .GlobalEnv under the name \code{checkdb}. \code{checkdb} is
#' a data.frame of dimension 200000 x 10 (approximatavely).
#'  
#' \code{checkdb_load} loads the file \code{filename}, by default \code{check_results.rds}  
#' in .GlobalEnv under the name \code{checkdb}.
#' 
#' @param   dir        character. The directory where \code{filename} or tar.gz files 
#'                     are saved. Default value \code{"."} is the current directory.
#' @param   repos      character. The address of your local CRAN.
#' @param   filename   character. The path to file "check_results.rds" (or equivalent). 
#' 
#' @examples
#' ### In this first example, we use a small file synchronized with zcrandb.
#' checkdb_load(system.file("aabb", "zcheck_results.rds", package = "RWsearch"))
#' dim(checkdb)
#' head(checkdb, 15)
#' 
#' @name checkdb
NULL

#' @export
#' @name checkdb
checkdb_down <- function(dir = ".", repos = getOption("repos")[1]) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
	destfile <- file.path(dir, "check_results.rds")
    urlrds   <- file.path(repos, "web/checks/check_results.rds")
	tdu      <- trydownloadurl(urlrds, destfile)
	if (tdu == 0) {
		message("check_results.rds downloaded.")
		checkdb_load(destfile)
	} else stop("check_results.rds is unavailable for download.")
}

#' @export
#' @rdname checkdb
checkdb_load <- function(filename = "check_results.rds") {
    if (file.exists(filename)) {
        checkdb <- readRDS(filename)
		checkdb <<- checkdb
        txt2    <- "checkdb loaded."
        txt3    <- length(unique(checkdb$Package))
        txt4    <- "packages were checked on"
        txt5    <- length(levels(checkdb$Flavor))
        txt6    <- "architectures:"
        message(paste(txt2, txt3, txt4, txt5, txt6))
		print(table(checkdb$Status))
    } else {
        stop(paste("File", basename(filename), "does not exist in this directory."))
    }
}



