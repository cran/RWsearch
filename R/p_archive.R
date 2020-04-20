## p_inun
## @include h_ttp.R


#' @title Read Packages in CRAN archive
#' @description
#' \code{p_archive} opens in the browser one page per package and displays 
#' the package versions stored in CRAN archive. 
#' 
#' \code{p_archive_lst} prints in the console a list of the package versions 
#' stored in CRAN archive. 
#'
#' Use \code{\link{l_targz}} takes as input the list obtained from \code{p_archive_lst}
#' and extracts the packages before a certain date (Default is today).
#'
#' Use \code{\link{p_downarch}} to download packages from CRAN archive, either the 
#' latest version or a specific version number.
#' 
#' Use \code{\link{archivedb_list}} to list all packages stored in CRAN archive 
#' (does not include the valid packages having a single version which are stored 
#' in regular CRAN only).
#' 
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     A vector of packages.
#' @param   char       (name to) a character vector. Use this argument if \code{...} fails 
#'                     or if you call the function from another function. If used, 
#'                     argument \code{...} is ignored.
#' @param   url        character. The url address of CRAN archive html file.
#' @param   lst        list. A list produced by \code{p_archive_lst}.
#' @param   before     character which can be converted to a Date, for instance 
#'                     "2017-05-14". Extract from CRAN archive the package(s) 
#'                     available before this date. Can be synchronized with
#'                     the release dates of base-R versions listed at: 
#'                     \url{https://CRAN.R-project.org/src/contrib/} and 
#'                     \url{https://CRAN.R-project.org/package=rversions/readme/README.html}
#' 
#' @examples
#' \donttest{
#' p_archive(brew, RWsearch)
#' }
#' lst <- p_archive_lst(brew, pacman, RWsearch, fitur, zmatrix, NotAPkg) ; lst
#' l_targz(lst, before = "2017-05-14")
#' 
#' @export
#' @name p_archive
p_archive <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/")
        utils::browseURL(url)
    }
}

#' @export
#' @rdname p_archive
p_archive_lst <- function(..., char = NULL, 
				 url = "https://cran.r-project.org/src/contrib/Archive") {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    lst <- as.list(pkgs)
    names(lst) <- pkgs
    for (pkg in pkgs) {
        urlp     <- paste(url, pkg, sep ="/")
        destfile <- tempfile()
        trdc     <- trydownloadurl(urlp, destfile)
        if (trdc == 0) {
			TABX  <- XML::readHTMLTable(destfile, header = TRUE, skip.rows = 1:2, 
										which = 1, stringsAsFactors = FALSE)
			colnames(TABX) <- make.names(colnames(TABX))
			lst[[pkg]] <- TABX[order(TABX[-nrow(TABX), "Last.modified"], 
			                         decreasing = TRUE),
						       c("Name", "Last.modified", "Size")]
        } else {
			message(paste("Package not in CRAN archive:", pkg))
			lst[[pkg]] <- NA
		}
	}
	lst
}

#' @export
#' @rdname p_archive
l_targz <- function(lst, before = Sys.Date()) {
	subdfr <- function(dfr, before) {
		if (inherits(dfr, "data.frame")) {
			res <- dfr[dfr[, "Last.modified"] <= before, ]
			if (dim(res)[1] == 0) NA else res
		} else NA
	}
	before <- as.Date(before)
	lst    <- lapply(lst, subdfr, before)
	lst    <- lapply(lst, function(x) if (inherits(x, "data.frame")) x[1,1] else NA)
	tgz    <- unlist(lst)
	targz  <- unique(tgz[!is.na(tgz)])
	notav  <- names(tgz)[is.na(tgz)]
	if (length(notav) != 0) {
		message(paste("tar.gz file(s) not in CRAN archive at this date:", 
		              paste(notav, collapse = ", ")))
	}
	targz
}



