## p_down + trydownloadurl
## @include p_display.R


#' @title Download the Package Documentation in One Directory or in Several Subdirectories
#' @description
#' If \code{pkgs} is a vector of packages obtained from \code{\link{s_crandb}}, 
#' \code{p_down} downloads from CRAN and saves in the \code{dir} directory (by default 
#' the current directory) the index page, the manual, the vignettes, the README, NEWS,
#' ChangeLog, CRAN checks files, the source code in \emph{pkg_ver.tar.gz} format,
#' the binary code in \emph{pkg_ver.tgz} (Mac OSX) or \emph{pkg_ver.zip} (Windows) 
#' format and a minimal R-script of each package. The files that do not exist are
#' ignored, with no warning. 
#' 
#' If \code{pkgs} is a list of packages obtained from \code{\link{s_crandb_list}}, 
#' \code{p_down} saves the downloaded files in subdirectories named after the names 
#' of the list, e.g. the keywords used at the search step. The names are  
#' eventually modified with \code{gsub(".", "_", make.names(pkg), fixed = TRUE)}
#' to cope with Unix and Windows directory names.
#' 
#' \code{p_down0} calls \code{p_down} with different values for each argument. 
#' With the default configuration, this function downloads nothing. It is mostly 
#' used to download one specific item which has not been previously downloaded. 
#' 
#' \code{p_downarch} downloads from CRAN archive the tar.gz file of one or several 
#' packages, either the last version(s) with \code{Sys.Date()} or the version(s) 
#' before a given date. It combines 3 functions: \code{\link{p_archive_lst}} 
#' lists the packages stored in CRAN archive and their version numbers, 
#' \code{\link{l_targz}} extracts the appropriate version numbers available 
#' before a given date, \code{targz_down} downloads the tar.gz files in the selected 
#' directory (default is the current directory) and eventually decompresses it.
#' 
#' \code{targz_down} downloads the tar.gz files from CRAN archive to the selected
#' directory (default is the current directory) and eventually decompresses it.
#' If \code{url = "https://cran.r-project.org/src/contrib"}, \code{targz_down} 
#' will take the latest version of the package.
#' 
#' @param   ...        any format recognized by \code{\link{cnsc}}, including list.
#'                     A vector or packages or a named list of packages (with names 
#'                     being the keywords).
#' @param   char       (name to) a character vector or a list. Use this argument if 
#'                     \code{...} fails or if you call the function from another function. 
#'                     If used, argument \code{...} is ignored. 
#' @param   index      logical. Download the html index page of each package.
#' @param   manual     logical. Download the pdf manual.
#' @param   vignettes  logical. Download the html and pdf vignettes, if they exist.
#' @param   README     logical. Download the README file, if it exists.
#' @param   NEWS       logical. Download the NEWS file, if it exists.
#' @param   ChangeLog  logical. Download the ChangeLog file, if it exists.
#' @param   checks     logical. Download the CRAN checks file.
#' @param   targz      logical. Download the *.tar.gz source file.
#' @param   untar      logical. Decompress the downloaded tar.gz file.
#' @param   binary     logical. Download the *.tgz (Mac OSX) or *.zip (Windows) 
#'                     binary file, depending the \code{type} value.
#' @param   type       character. Either \code{"mac.binary"}, \code{"mac.binary.el-capitan"},   
#'                     or \code{"win.binary"}. The default, \code{"binary"}, 
#'                     automatically detects the local OS and the variants
#'                     between R-3.6.3, R-4.0.0 or (for Windows) gcc8. See the 
#'                     \code{type} section of \code{utils::install.packages}.
#' @param   script     logical. Create a mini-script and save it in a *.R file.
#' @param   dir        character. The directory in which the files are saved. 
#'                     Default value \code{"."} is the current directory.
#' @param   crandb     data.frame \code{crandb}. The data.frame of CRAN packages.
#' @param   repos      character. The address of your local CRAN.
#' @param   before     character which can be converted to a Date, for instance 
#'                     "2017-05-14". Extract from CRAN archive the package(s) 
#'                     available before this date. Can be synchronized with
#'                     the release dates of base-R versions listed at:  
#'                     \url{https://CRAN.R-project.org/src/contrib/} and 
#'                     \url{https://CRAN.R-project.org/package=rversions/readme/README.html}
#' @param   url        character. The url address of CRAN archive html file.
#' @param   ptargz     character. A vector of package(s) with their version number
#'                     and tar.gz extension stored in CRAN archive. These packages
#'                     can be identified with \code{\link{l_targz}}.
#' 
#' @examples
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' \donttest{
#' ## Download the documentation in the "dirpkgs" directory. Flat representation.
#' dir <- file.path(tempdir(), "dirpkgs")
#' p_down(RWsearch, pdfsearch, sos, dir = dir, repos = "https://cloud.r-project.org")
#' list.files(dir, recursive = TRUE, full.names = TRUE)
#' 
#' ## Download the documentation in subdirectories named after the keywords.
#' dir <- file.path(tempdir(), "dirpkgslist")
#' (lst <- s_crandb_list(thermodynamic, "chemical reaction"))
#' system.time(
#'   p_down(lst, dir = dir, repos = "https://cloud.r-project.org")
#' )
#' list.files(dir, recursive = TRUE, full.names = TRUE)
#' 
#' ## Download tar.gz files stored in CRAN archive. 
#' dir <- file.path(tempdir(), "targzip")
#' p_downarch(fitur, zmatrix, NotAPkg, before = "2017-05-14", dir = dir)
#' targz_down("SVN_1.0.tar.gz", dir = dir, untar = TRUE)
#' list.files(dir, recursive = TRUE, full.names = TRUE)
#' }
#' @export
#' @name p_down
p_down <- function(..., char = NULL, index = TRUE, manual = TRUE, vignettes = TRUE, 
                   README = TRUE, NEWS = FALSE, ChangeLog = FALSE, checks = FALSE, 
                   targz = FALSE, untar = FALSE, binary = FALSE, type = "binary", 
                   script = FALSE, dir = ".", 
                   crandb = get("crandb", envir = .GlobalEnv), 
                   repos = getOption("repos")[1]) {
    if (!isTRUE(capabilities("libcurl"))) {
        stop('p_down requires R compiled with libcurl. Run capabilities("libcurl")')
    }
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    wd   <- getwd()
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs) & !is.data.frame(pkgs)) {
        if (is.null(names(pkgs))) stop ("pkgs is a list with no names.")
        for (mot in names(pkgs)) {
            # dir2 <- file.path(dir, gsub(".", "", make.names(mot), fixed = TRUE))
            dir2 <- file.path(dir, gsub(".", "_", make.names(mot), fixed = TRUE))
            dir2 <- gsub("\\", "/", path.expand(dir2), fixed = TRUE)
            if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
            setwd(dir2)
            p_downh(pkgs = pkgs[[mot]], index = index, manual = manual, vignettes = vignettes, 
                    README = README, NEWS = NEWS, ChangeLog = ChangeLog, checks = checks, 
                    targz = targz, untar = untar, binary = binary, type = type, 
                    script = script, crandb = crandb, repos = repos)
            setwd(wd)
        }
    } else {
            dir2 <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
            if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
            setwd(dir2)
            p_downh(pkgs = pkgs, index = index, manual = manual, vignettes = vignettes, 
                    README = README, NEWS = NEWS, ChangeLog = ChangeLog, checks = checks, 
                    targz = targz, untar = untar, binary = binary, type = type, 
                    script = script, crandb = crandb, repos = repos)
            setwd(wd)
    }
}

#' @export
#' @rdname p_down
p_down0 <- function(..., char = NULL, index = FALSE, manual = FALSE, vignettes = FALSE, 
                    README = FALSE, NEWS = FALSE, ChangeLog = FALSE, checks = FALSE, 
                    targz = FALSE, untar = FALSE, binary = FALSE, type = "binary", 
                    script = FALSE, dir = ".", 
                    crandb = get("crandb", envir = .GlobalEnv), 
                    repos = getOption("repos")[1]) {
    if (!isTRUE(capabilities("libcurl"))) {
        stop('p_down requires R compiled with libcurl. Run capabilities("libcurl")')
    }
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    p_down(char = pkgs, index = index, manual = manual, vignettes = vignettes, 
           README = README, NEWS = NEWS, ChangeLog = ChangeLog, checks = checks, 
           targz = targz, untar = untar, binary = binary, type = type, 
           script = script, dir = dir, crandb = crandb, repos = repos) 
}

#' @export
#' @rdname p_down
p_downarch <- function(..., char = NULL, before = Sys.Date(), dir = ".", untar = FALSE,                
                url = "https://cran.r-project.org/src/contrib/Archive") {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    lst    <- p_archive_lst(char = pkgs, url = url)
    ptargz <- l_targz(lst, before = before)
    targz_down(ptargz, dir = dir, untar = untar, url = url)
}

#' @export
#' @rdname p_down
targz_down <- function(ptargz, dir = ".", untar = FALSE,
                url = "https://cran.r-project.org/src/contrib/Archive") {
    if (is.list(ptargz)) stop("ptargz cannot be a list.")
    if (length(ptargz) == 0) message("ptargz is empty. No file to download.") else {
        if (any(!endsWith(ptargz, "tar.gz"))) stop("tar.gz extension is missing.")
        dir2 <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
        if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
        for (pkgtargz in ptargz) {
            pkg <- substr(pkgtargz, 1, grepRaw("_", pkgtargz, fixed = TRUE)-1)
            cran_file <- if (endsWith(url, "Archive")) {
                   file.path(url, pkg, pkgtargz)
            } else file.path(url, pkgtargz)
            localfile <- file.path(dir2, pkgtargz)
            trdc      <- trydownloadurl(cran_file, localfile)
            if (trdc == 0) {
                if (untar) {utils::untar(localfile, exdir = dir2)
                    message(paste("Package", pkgtargz, "downloaded and extracted."))
                } else message(paste("Package", pkgtargz, "downloaded."))
            } else {
                message(paste("Package", pkgtargz, "not in CRAN Archive."))            
            }
        }
    }
}




## #' @title Try download a (vector of) url(s) and save it (them) as a file(s)
## #' @description
## #' \code{htmltable} uses the \emph{brew} package to print an html table. 
## #' @param   url        a (vector of) well-formed url(s).
## #' @param   destfile   character. A (vector of) filename(s) saved on the disk.
trydownloadurl <- function(url, destfile) {
    if (length(url) > 1 || length(destfile) > 1) {
        stopifnot(length(url) == length(destfile))
        res <- mapply(trydownloadurl, url, destfile, USE.NAMES = FALSE)
        invisible(res)
    } else {
        TC <- tryconurl(url)
        if (inherits(TC, "url")) {
            utils::download.file(url, destfile, method = "libcurl", 
                        quiet = TRUE, mode = "wb", cacheOK = FALSE)
            close(TC)
            invisible(0)
        } else {
            invisible(1)
        }
    }
}


pkglasttxt <- function(x, pkg, v = FALSE) {
    if (length(x) == 0) x else {
        pslash <- max(gregexpr("/", x)[[1]], 0, na.rm = TRUE)
        if (v) { 
            paste(pkg, "v", substring(x, 1+pslash), sep = "-") 
        } else { 
            paste(pkg, substring(x, 1+pslash), sep = "-")
        }
    }
}


p_downh <- function (pkgs, index, manual, vignettes, README, NEWS, ChangeLog, 
                     checks, targz, untar, binary, type, script, crandb, repos) {
    ospkgs <- crandb[!is.na(crandb[,"OS_type"]), c("Package","Version","OS_type")]
    for (i in seq_along(pkgs)) {
        pkg <- pkgs[i]
        if (is.element(pkg, crandb$Package)) {
        
            ## PREPARE
            purl <- file.path(repos, "web", "packages", pkg)
            iurl <- file.path(repos, "web", "packages", pkg, "index.html")
            murl <- file.path(repos, "web", "packages", pkg, paste0(pkg, ".pdf"))
            dest    <- tempfile()
            trydownloadurl(iurl, dest)
            links   <- XML::getHTMLLinks(dest)
            txtrme  <- grep("readme", links, ignore.case = TRUE, value = TRUE)
            txtrme  <- grep("ReadMe", txtrme, ignore.case = FALSE, value = TRUE, invert = TRUE)
            txtrme  <- grep("github.com", txtrme, ignore.case = TRUE, value = TRUE, invert = TRUE)[1]
            txtnews <- grep("NEWS", links, ignore.case = TRUE, value = TRUE)[1]
            txtvig  <- grep("vignettes", links, ignore.case = TRUE, value = TRUE)
            txtlog  <- grep("ChangeLog", links, ignore.case = TRUE, value = TRUE)[1]
            txtchk  <- grep("check_results", links, ignore.case = TRUE, value = TRUE)[1]
            urlrme  <- file.path(purl, txtrme)
            urlnews <- file.path(purl, txtnews) 
            urlvig  <- file.path(purl, txtvig)
            urllog  <- file.path(purl, txtlog)
            urlchk  <- file.path(purl, txtchk)
            txtrme2 <- pkglasttxt(txtrme, pkg)
            txtnews2<- pkglasttxt(txtnews, pkg)
            txtvig2 <- pkglasttxt(txtvig, pkg, v = TRUE)
            txtlog2 <- pkglasttxt(txtlog, pkg)
            txtchk2 <- pkglasttxt(txtchk, pkg)
            
            ## DOWNLOAD
            if (index)  trydownloadurl(iurl, paste0(pkg, ".html"))
            if (manual) trydownloadurl(murl, paste0(pkg, ".pdf"))
            if (vignettes && length(txtvig2) != 0) trydownloadurl(urlvig, txtvig2)
            if (README && length(txtrme2) != 0)    trydownloadurl(urlrme,  txtrme2)
            if (NEWS   && length(txtnews2) != 0)   trydownloadurl(urlnews, txtnews2)
            if (ChangeLog && length(txtlog2) != 0) trydownloadurl(urllog,  txtlog2)
            if (checks && length(txtchk2) != 0)    trydownloadurl(urlchk,  txtchk2)
            if (targz) {
                pkgver    <- crandb[crandb$Package == pkg, "Version"]
                localfile <- paste0(pkg, "_", pkgver, ".tar.gz")
                cran_file <- paste0(repos, "/src/contrib/", localfile)
                trdc      <- trydownloadurl(cran_file, localfile)
                if ((trdc == 0) & untar) {
                    utils::untar(localfile)
                    message(paste("Package", localfile, "extracted."))
                } 
            }
            if (binary) {
                utils::download.packages(pkg, destdir = ".", available = NULL,
                        repos = repos, contriburl = contrib.url(repos, type),
                        method = "libcurl", type = type)
            }
            if (script) {
                zz <- file(paste0(pkg, "-script.R"), "w") 
                cat("\n", file = zz)
                cat("## ===================", rep("=", nchar(pkg)), "\n", 
                  file = zz, sep = "")
                cat("## ", Sys.Date(), " PACKAGE ", pkg, "\n", 
                  file = zz, sep = "")
                cat("## ===================", rep("=", nchar(pkg)), "\n", 
                  file = zz, sep = "")
                cat("## Comments \n", file = zz)
                cat("\n", file = zz)
                cat("## Examples \n", file = zz)
                cat("library(", pkg, ")", file = zz, sep = "\"")
                cat("\n\n", file = zz)
                close(zz)
            }
        } else {
            warning(paste("Package", pkg, "is not in crandb."))
        }
        if (is.element(pkg, ospkgs[,"Package"])) {
            os <- ospkgs[ospkgs[,"Package"] == pkg, "OS_type"]
            message(paste0("Package ", pkg, " is OS dependant and only for ", 
                           tools::toTitleCase(os), "."))
        }
        close_libcurl()
    }
}



