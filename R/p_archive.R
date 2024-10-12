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
#' \code{\link{l_targz}} takes as input the list issued by \code{p_archive_lst}
#' and lists the last package versions archived before a certain date.
#'
#' Use \code{\link{p_downarch}} to download packages from the CRAN archives,
#' either the latest version stored or a specific version number.
#'
#' Use \code{\link{archivedb_list}} to list all packages stored in CRAN archive
#' (does not include the valid packages having a single version which are stored
#' in regular CRAN).
#'
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     A vector of packages.
#' @param   char       (name to) a character vector. Use this argument if \code{...}
#'                     fails or if you call the function from another function.
#'                     If used, argument \code{...} is ignored.
#' @param   lst        list. A list produced by \code{p_archive_lst}.
#' @param   before     character which can be converted to a Date, for instance
#'                     "2017-05-14". Extract from CRAN archive the package(s)
#'                     available before this date. Can be synchronized with
#'                     the release dates of base-R versions listed at:
#'                     \url{https://CRAN.R-project.org/src/contrib/} and
#'                     \url{https://CRAN.R-project.org/package=rversions/readme/README.html}
#'
#' @examples
#' if (interactive()) p_archive(brew, RWsearch, NotAPkg)
#' 
#' lst <- p_archive_lst(RWsearch, zmatrix, NotAPkg) ; lst
#' l_targz(lst, before = "2020-01-01")
#'
#' @export
#' @name p_archive
p_archive <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    url0 = "https://cran.r-project.org/src/contrib/Archive"
    for (pkg in pkgs) {
        url <- paste0(url0, "/", pkg, "/")
        msF <- paste("Package", pkg, "is not in CRAN archive.")
        trybrowseURL(url, msgF = msF)
    }
}

#' @export
#' @rdname p_archive
p_archive_lst <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    lst <- as.list(pkgs)
    names(lst) <- pkgs
    url0 = "https://cran.r-project.org/src/contrib/Archive"
    for (pkg in pkgs) {
        urlp     <- paste(url0, pkg, sep = "/")
        destfile <- tempfile(paste0(pkg, "-archive"), fileext = ".html")
        trdl     <- trydownloadurl(urlp, destfile)
        if (trdl == 0) {
            TABX  <- XML::readHTMLTable(destfile, header = TRUE, skip.rows = 1:2,
                                        which = 1, stringsAsFactors = FALSE)
            colnames(TABX) <- make.names(colnames(TABX))
            lst[[pkg]] <- TABX[order(TABX[-nrow(TABX), "Last.modified"],
                                     decreasing = TRUE),
                               c("Name", "Last.modified", "Size")]
        } else {
            message(paste("Package", pkg, "is not in CRAN archive."))
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



