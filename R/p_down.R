## p_down + trydownloadurl
## @include p_display.R


#' @title Download Package Documentation in One Directory or Several Subdirectories
#' @description
#' If \code{pkgs} is a vector of packages obtained from \code{\link{s_crandb}}, 
#' \code{p_down} downloads from CRAN and saves in the \code{dir} directory (by default 
#' the current directory) the index page, the manual, the vignettes, the README, NEWS,
#' ChangeLog, CRAN checks files, the source code in \emph{pkg_ver.tar.gz} format and 
#' a minimal R-script of each package. The files that do not exist in CRAN are ignored, 
#' with no warning. 
#' 
#' If \code{pkgs} is a list of packages obtained from \code{\link{s_crandb_list}}, 
#' \code{p_down} saves the downloaded files in subdirectories named after the names 
#' of the list, e.g. the keywords used at the search step. The names are  
#' eventually modified with \code{gsub(".", "", make.names(keyword), fixed = TRUE)}
#' to cope with Unix and Windows directory names.
#' 
#' \code{p_down0} calls \code{p_down} with different values for each argument. 
#' With the default configuration, this function downloads nothing. It is mostly used 
#' to download one specific item which has not been previously downloaded. 
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
#' p_down(pacman, pdfsearch, sos, dir = "dirpkgs", repos = "https://cran.univ-paris1.fr")
#' 
#' ## Download the documentation in subdirectories named after the keywords.
#' (lst <- s_crandb_list(thermodynamic, "chemical reaction"))
#' p_down(lst, dir = "dirpkgslist", repos = "https://cran.univ-paris1.fr") 
#' }
#' @export
#' @name p_down
p_down <- function(..., char = NULL, index = TRUE, manual = TRUE, vignettes = TRUE, 
                   README = TRUE, NEWS = FALSE, ChangeLog = FALSE, checks = FALSE, 
                   targz = FALSE, script = FALSE, dir = ".", 
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
            dir2 <- file.path(dir, gsub(".", "", make.names(mot), fixed = TRUE))
            dir2 <- gsub("\\", "/", path.expand(dir2), fixed = TRUE)
            if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
            setwd(dir2)
            p_downh(pkgs = pkgs[[mot]], index = index, manual = manual, vignettes = vignettes, 
                    README = README, NEWS = NEWS, ChangeLog = ChangeLog, checks = checks, 
                    targz = targz, script = script, crandb = crandb, repos = repos)
            setwd(wd)
        }
    } else {
            dir2 <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
            if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
            setwd(dir2)
            p_downh(pkgs = pkgs, index = index, manual = manual, vignettes = vignettes, 
                    README = README, NEWS = NEWS, ChangeLog = ChangeLog, checks = checks, 
                    targz = targz, script = script, crandb = crandb, repos = repos)
            setwd(wd)
    }
}

#' @export
#' @rdname p_down
p_down0 <- function(..., char = NULL, index = FALSE, manual = FALSE, vignettes = FALSE, 
                    README = FALSE, NEWS = FALSE, ChangeLog = FALSE, checks = FALSE, 
                    targz = FALSE, script = FALSE, dir = ".", 
                    crandb = get("crandb", envir = .GlobalEnv), 
                    repos = getOption("repos")[1]) {
    if (!isTRUE(capabilities("libcurl"))) {
        stop('p_down requires R compiled with libcurl. Run capabilities("libcurl")')
    }
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    p_down(char = pkgs, index = index, manual = manual, vignettes = vignettes, 
           README = README, NEWS = NEWS, ChangeLog = ChangeLog, checks = checks, 
           targz = targz, script = script, dir = dir, 
           crandb = crandb, repos = repos) 
}




## #' @title Try to download an url and save it as a file
## #' @description
## #' \code{htmltable} uses the \emph{brew} package to print an html table. 
## #' @param   url        a well-formed url.
## #' @param   destfile   character. The name of the saved file.
## #' @param   open       character. How to open the connection. Here "r" or "rb". 
## #'                     "r" for most files and  
## #'                     "rb" for .gz, .bz2, .xz, .tgz, .zip, .rda, .rds, .RData.
## #'                     From version 4.1.1, open = "rb" is hard-coded.  
trydownloadurl <- function(url, destfile) {
    TC <- tryCatch(con <- url(url, open = "rb", method = "libcurl"),  
                   condition = function(cond) {})
    if (inherits(TC, "url")) {
        utils::download.file(url, destfile, method = "libcurl", 
                    quiet = TRUE, mode = "wb", cacheOK = FALSE)
        close(con)
        # Close both con and TC (magic)
    } else {
        try(close(con), silent = TRUE) 
    }
}


p_downh <- function (pkgs, index, manual, vignettes, README, NEWS, ChangeLog, 
                     checks, targz, script, crandb, repos) {
    ospkgs <- if ("OS_type" %in% colnames(crandb)) {
        crandb[!is.na(crandb[,"OS_type"]), c("Package","Version","OS_type")]
    } else {
        crandb[!is.na(crandb[,"OS.type"]), c("Package","Version","OS.type")]
    }
    for (i in seq_along(pkgs)) {
        pkg <- pkgs[i]
        if (is.element(pkg, crandb$Package)) {
            options("warn" = -1)
            if (index) trydownloadurl(
                file.path(repos, "web", "packages", pkg, "index.html"), 
                paste0(pkg, ".html")
            )
            if (manual) trydownloadurl(
                paste0(repos, "/web/packages/", pkg, "/", pkg, ".pdf"), 
                paste0(pkg, ".pdf")
            )
            if (vignettes) { 
                urvi  <- paste0(repos, "/web/packages/", pkg, "/vignettes/index.rds") 
                purv  <- paste0(repos, "/web/packages/", pkg, "/vignettes/") 
                TC <- tryCatch(con <- url(urvi, open = "rb", method = "libcurl"),  
                               condition = function(cond) {})
                if (inherits(TC, "url")) {
                    vigns   <- readRDS(gzcon(con))[,1]
                    close(con)
                    reposfiles <- paste0(purv, vigns)
                    localfiles <- paste0(pkg, "-v-", vigns)
                    mapply(utils::download.file, reposfiles, localfiles, 
                           MoreArgs = list(quiet = TRUE, mode = "wb", cacheOK = FALSE), 
                           SIMPLIFY = FALSE, USE.NAMES = FALSE)
                } else { try(close(con), silent = TRUE) }
            }
            if (README) {
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/README"), 
                    paste0(pkg, "-README")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/readme.html"), 
                    paste0(pkg, "-readme.html")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/README.html"), 
                    paste0(pkg, "-README.html")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/README.md"), 
                    paste0(pkg, "-README.md")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/readme/readme.html"), 
                    paste0(pkg, "-readme.html")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/readme/README.html"), 
                    paste0(pkg, "-README.html")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/readme/README.md"), 
                    paste0(pkg, "-README.md")
                )
            }
            if (NEWS) {
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/NEWS"), 
                    paste0(pkg, "-NEWS")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/news.html"), 
                    paste0(pkg, "-news.html")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/NEWS.html"), 
                    paste0(pkg, "-NEWS.html")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/news/news.html"), 
                    paste0(pkg, "-news.html")
                )
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/news/NEWS.html"), 
                    paste0(pkg, "-NEWS.html")
                )
            }
            if (ChangeLog) {
                trydownloadurl(
                    paste0(repos, "/web/packages/", pkg, "/ChangeLog"), 
                    paste0(pkg, "-ChangeLog")
                )
            }
            if (checks) {
                trydownloadurl(
                    paste0(repos, "/checks/check_results_", pkg, ".html"), 
                    paste0(pkg, "-check-results.html")
                )
            }
            if (targz) {
                pkgver    <- crandb[crandb$Package == pkg, "Version"]
                localfile <- paste0(pkg, "_", pkgver, ".tar.gz")
                cran_file <- paste0(repos, "/src/contrib/", localfile)
                trydownloadurl(cran_file, localfile) # , open = "rb"
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
            options("warn" = 0)
        } else {
            warning(paste("Package", pkg, "is not in crandb."))
        }
        if (is.element(pkg, ospkgs[,"Package"])) {
            os <- if ("OS_type" %in% colnames(ospkgs)) {
                ospkgs[ospkgs[,"Package"] == pkg, "OS_type"]
            } else {
                ospkgs[ospkgs[,"Package"] == pkg, "OS.type"]
            }
            message(paste0("Package ", pkg, " is OS dependant and only for ", tools::toTitleCase(os), "."))
        }
        closeAllConnections()
    }
}



