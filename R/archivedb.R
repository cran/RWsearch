## archivedb
## @include a_intro.R


#' @title CRAN archive (CRAN-archive.html + archivedb)
#' @description
#' The following functions deal with the packages archived in CRAN. The html file 
#' downloaded from CRAN contains the regular packages that have been updated once and 
#' the packages that have been removed from CRAN by CRAN administrators. It does not contain
#' the first version of the packages uploaded to CRAN and never updated. These files and
#' the files removed from CRAN index can be guessed through a comparison with \code{crandb}. 
#' 
#' \code{archivedb_down} downloads from CRAN the html file of the archived packages, saves 
#' it on the disk under the name \code{filename}, extracts from it and loads in .GlobalEnv
#' a data.frame named \code{archivedb}.
#' 
#' \code{archivedb_load} reads the html file \code{filename} saved on the disk, extracts 
#' from it and loads in .GlobalEnv a data.frame named \code{archivedb}.
#' 
#' \code{archivedb_npkgs} returns the number of packages listed each category: number of
#' packages in \code{crandb}, in \code{archivedb}, at first version, at subsequent version
#' and removed from \code{crandb} (CRAN index). 
#' 
#' \code{archivedb_pkgs} returns the packages listed in CRAN archive (= \code{archivedb}). 
#' 
#' \code{archivedb_rempkgs} returns the packages removed from CRAN but available in CRAN  
#' archive. The result can be combined with \code{\link{p_check}} to display the last 
#' CRAN check performed (if available). See the example. 
#' 
#' \code{archivedb_list} compares the data.frame \code{archivedb} and \code{crandb} and
#' returns a list with the following items:
#' \itemize{
#' \item{pkgs_crandb: the packages listed in \code{crandb}.}
#' \item{pkgs_archivedb: the packages listed in \code{archivedb}.}
#' \item{pkgs_first: the packages in first version in \code{crandb}.}
#' \item{pkgs_updated: the packages with more than one version in \code{crandb}.}
#' \item{pkgs_removed: the archived packages removed from CRAN regular index, 
#' i.e. not listed in \code{crandb}.}
#' \item{dfr_crandb: data.frame pkgs_crandb + Published date.}
#' \item{dfr_archivedb: data.frame pkgs_archivedb + Archived date.}
#' \item{dfr_first: data.frame pkgs_first + Published date.}
#' \item{dfr_updated: data.frame pkgs_updated + Published date.}
#' \item{dfr_removed: data.frame pkgs_removed+ Archived date.}
#' \item{npkgs: the number of packages in each category.}
#' }
#' 
#' Use \code{\link{p_archive_lst}} to list the package versions stored in CRAN archive.
#' 
#' Use \code{\link{p_downarch}} to download packages from CRAN archive, either the 
#' latest version or a specific version number.
#' 
#' @param   filename   character. The path to file "CRAN-archive.html" (or equivalent). 
#' @param   dir        character. The directory where \code{filename} or tar.gz files 
#'                     are saved. Default value \code{"."} is the current directory.
#' @param   url        character. The url address of CRAN archive html file.
#' @param   archivedb  data.frame \code{archivedb}. The archivedb data.frame format loaded 
#'                     in memory by \code{archivedb_down} or \code{archivedb_load}. 
#' @param   crandb     data.frame \code{crandb}. The data.frame of CRAN packages.
#' 
#' @examples
#' ### DOWNLOAD archivedb AND COMPARE IT WITH crandb.
#' ## In real life, download archivedb and crandb from CRAN
#' ## with the functions archivedb_down() and crandb_down().  
#' ## In this example, we load two small files.
#' 
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' archivedb_load(system.file("aabb", "zCRAN-archive.html", package = "RWsearch")) 
#' archivedb_npkgs()
#' archivedb_pkgs()
#' archivedb_rempkgs()
#' 
#' lst <- archivedb_list()
#' lapply(lst, head)
#' lapply(lst, tail)
#' xlim <- as.Date(range(lst$dfr_archivedb$Archived)) ; xlim
#' op <- par(mfrow = c(2,1))
#' hist(as.Date(lst$dfr_first$Published), 
#'      breaks = 12, freq = TRUE, las = 1, xlim = xlim)
#' hist(as.Date(lst$dfr_archivedb$Archived), 
#'      breaks = 12, freq = TRUE, las = 1, xlim = xlim)
#' par(op)
#' @name archivedb
NULL

#' @export
#' @rdname archivedb
archivedb_down <- function(filename = "CRAN-archive.html", dir = ".",
                  url = "https://cran.r-project.org/src/contrib/Archive") {
    TC <- tryconurl(url)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    destfile <- file.path(dir, filename)
    utils::download.file(url, destfile, method = "libcurl", 
                         quiet = TRUE, mode = "wb", cacheOK = FALSE)
    close(TC)
    TABX <- XML::readHTMLTable(destfile, header = TRUE, skip.rows = 1:3, 
                               which = 1, stringsAsFactors = FALSE)
    archivedb <- data.frame(Package  = gsub("/", "", TABX[-nrow(TABX), "Name"]), 
                            Archived = as.Date(TABX[-nrow(TABX), "Last modified"]), 
                            stringsAsFactors = FALSE)
    archivedb <<- archivedb
    ff <- if (dir == ".") filename else destfile
message(paste(ff, "saved. archivedb loaded in .GlobalEnv."))
}

#' @export
#' @rdname archivedb
archivedb_load <- function(filename = "CRAN-archive.html") {
    TABX <- XML::readHTMLTable(filename, header = TRUE, skip.rows = 1:3, 
                               which = 1, stringsAsFactors = FALSE)
    archivedb <- data.frame(Package  = gsub("/", "", TABX[-nrow(TABX), "Name"]), 
                            Archived = as.Date(TABX[-nrow(TABX), "Last modified"]), 
                            stringsAsFactors = FALSE)
    archivedb <<- archivedb
message(paste("archivedb loaded in .GlobalEnv.", nrow(archivedb), "listed."))
}

#' @export
#' @rdname archivedb
archivedb_npkgs <- function(archivedb = get("archivedb", envir = .GlobalEnv), 
                               crandb = get("crandb", envir = .GlobalEnv)) {
    CinA  <- is.element(crandb$Package, archivedb$Package)
    AinC  <- is.element(archivedb$Package, crandb$Package)
    npkgs <- c(pcrandb    = nrow(crandb),
               parchivedb = nrow(archivedb), 
               pfirst     = sum(!CinA),
               pupdated   = sum( CinA),
               premoved   = sum(!AinC)
               )
return(npkgs)
}

#' @export
#' @rdname archivedb
archivedb_pkgs <- function(archivedb = get("archivedb", envir = .GlobalEnv)) {
    pkgs_archivedb <- archivedb[, "Package"]
return(pkgs_archivedb)
}

#' @export
#' @rdname archivedb
archivedb_rempkgs <- function(archivedb = get("archivedb", envir = .GlobalEnv), 
                                 crandb = get("crandb", envir = .GlobalEnv)) {
    AinC         <- is.element(archivedb$Package, crandb$Package)
    pkgs_removed <- archivedb[!AinC, "Package"]
return(pkgs_removed)
}

#' @export
#' @rdname archivedb
archivedb_list <- function(archivedb = get("archivedb", envir = .GlobalEnv), 
                              crandb = get("crandb", envir = .GlobalEnv)) {
    CinA  <- is.element(crandb$Package, archivedb$Package)
    AinC  <- is.element(archivedb$Package, crandb$Package)
    npkgs <- c(pcrandb    = nrow(crandb),
               parchivedb = nrow(archivedb), 
               pfirst     = sum(!CinA),
               pupdated   = sum( CinA),
               premoved   = sum(!AinC)
               )
   lst <- list(pkgs_crandb    = crandb[, "Package"],
               pkgs_archivedb = archivedb[, "Package"],
               pkgs_first     = crandb[!CinA, "Package"],
               pkgs_updated   = crandb[ CinA, "Package"],
               pkgs_removed   = archivedb[!AinC, "Package"],
                dfr_crandb    = crandb[, c("Package", "Published")],
                dfr_archivedb = archivedb[, c("Package", "Archived")],
                dfr_first     = crandb[!CinA, c("Package", "Published")],
                dfr_updated   = crandb[ CinA, c("Package", "Published")],
                dfr_removed   = archivedb[!AinC, c("Package", "Archived")],
               npkgs          = npkgs
               )
    # message("Breakdown of packages in crandb and archivedb:")
    # print(npkgs)
return(lst)
}




