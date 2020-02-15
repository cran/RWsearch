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
#' Visit \code{\link{p_downarch}} to download tar.gz file(s) from CRAN archive.
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
#' @param   binary     logical. Download the *.tgz (Mac OSX) or *.zip (Windows) 
#'                     binary file, depending the \code{type} value.
#' @param   type       character. Either \code{"mac.binary"}, \code{"mac.binary.el-capitan"},   
#'                     or \code{"win.binary"}. \code{"binary"} seems also accepted. 
#'                     See the \code{type} section of \code{utils::install.packages}.
#' @param   script     logical. Create a mini-script to test the package.
#' @param   dir        character. The directory in which the files are saved. 
#'                     Default value \code{"."} is the current directory.
#' @param   crandb     data.frame \code{crandb}. The data.frame of CRAN packages.
#' @param   repos      character. The address of your local CRAN.
#' @examples
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' \donttest{
#' ## Download the documentation in the "dirpkgs" directory. Flat representation.
#' p_down(pacman, pdfsearch, sos, dir = "dirpkgs", repos = "https://cloud.r-project.org")
#' 
#' ## Download the documentation in subdirectories named after the keywords.
#' (lst <- s_crandb_list(thermodynamic, "chemical reaction"))
#' p_down(lst, dir = "dirpkgslist", repos = "https://cloud.r-project.org") 
#' }
#' @export
#' @name p_down
p_down <- function(..., char = NULL, index = TRUE, manual = TRUE, vignettes = TRUE, 
                   README = TRUE, NEWS = FALSE, ChangeLog = FALSE, checks = FALSE, 
                   targz = FALSE, binary = FALSE, type = "binary", 
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
                    targz = targz, binary = binary, type = type, script = script, 
					crandb = crandb, repos = repos)
            setwd(wd)
        }
    } else {
            dir2 <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
            if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
            setwd(dir2)
            p_downh(pkgs = pkgs, index = index, manual = manual, vignettes = vignettes, 
                    README = README, NEWS = NEWS, ChangeLog = ChangeLog, checks = checks, 
                    targz = targz, binary = binary, type = type, script = script, 
					crandb = crandb, repos = repos)
            setwd(wd)
    }
}

#' @export
#' @rdname p_down
p_down0 <- function(..., char = NULL, index = FALSE, manual = FALSE, vignettes = FALSE, 
                    README = FALSE, NEWS = FALSE, ChangeLog = FALSE, checks = FALSE, 
                    targz = FALSE, binary = FALSE, type = "binary", 
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
           targz = targz, binary = binary, type = type, script = script,
           dir = dir, crandb = crandb, repos = repos) 
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
                     checks, targz, binary, type, script, crandb, repos) {
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
			txtrme  <- grep("readme", links, ignore.case = TRUE, value = TRUE)[1]
			txtrme  <- grep("ReadMe", txtrme, ignore.case = FALSE, value = TRUE, invert = TRUE)[1]
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
                trydownloadurl(cran_file, localfile)
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



