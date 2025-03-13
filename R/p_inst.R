## p_inst
## @include p_html.R


#' @title Immediate or delayed package installation
#'
#' @description
#' \code{p_inst} is a wrapper around \code{\link[utils]{install.packages}}. It tries
#' hard to select the most appropriate \code{lib} and stops in case of conflict.
#' Use \code{install.packages} if any additional argument is needed.
#'
#' \code{p_inst_batsh} writes one executable file \code{.sh} (Linux, macOS) or 
#' two files \code{.bat, .R} (Windows) that allow a delayed installation of old 
#' packages in a R --vanilla environment. This is useful if the default configuration
#' of R defined in \code{R/etc/Rprofile.site} launchs additional packages with 
#' compiled code, since these packages cannot be updated without modifying the 
#' \code{R/etc/Rprofile.site} file. 
#' 
#' \code{p_oldpkgs} is a wrapper around \code{\link[utils]{old.packages}}.
#' 
#' For the three above functions, if \code{\link{crandb}} loaded in \code{.GlobalEnv},
#' then the packages are sorted by their increasing number of dependencies. 
#' 
#' \code{...} allows a non-standard evaluation of unquoted packages separated by commas.
#'
#' @param  ...     any format recognized by \code{\link{cnsc}}, excluding list.
#'                 A vector of packages.
#' @param  char    (name to) a character vector or a list. Use this argument if
#'                 \code{...} fails or if you call the function from another function.
#'                 If used, argument \code{...} is ignored.
#' @param  lib     character. The directory where to install the packages, usually
#'                 one of the directories listed by \code{\link{.libPaths}}. If NULL,
#'                 select automatically the most relevant directory.
#' @param  lib.loc character. The directory where to search for old packages.
#' @param  repos   character. The address of your local CRAN.
#' @param  contriburl    character. The address of your private repository.
#' @param  dependencies  logical. \code{FALSE} skips the installation of dependencies. 
#'                 \code{NA} installs \code{c("Depends", "Imports", "LinkingTo")}
#'                 dependencies.
#' @param  type    character. Either \code{"source"}, \code{"both"}, \code{"binary"}
#'                 (or its variants \code{"mac.binary"}, \code{"mac.binary.el-capitan"},
#'                 \code{"win.binary"}).
#' @param  verbose Logical. Print information of the download process.
#' @param  dir     character. The directory where to write the files \code{.sh, .bat, .R}.
#' 
#' @examples
#' tmpdir <- tempdir()
#' pkgs <- p_oldpkgs(type = "source", repos = "https://cloud.r-project.org")
#' pkgs <- c("brew","fs","XML","RWsearch")
#' p_inst_batsh(pkgs, type = "source", dir = tmpdir, repos = "https://cloud.r-project.org")
#' if (.Platform$OS.type == "windows") {
#'     unlink(file.path(tmpdir, "installrpkgs.R"))
#'     unlink(file.path(tmpdir, "installrpkgs.bat")) 
#' } else unlink(file.path(tmpdir, "installrpkgs.sh"))
#' 
#' @export
#' @name p_inst
p_inst <- function (..., char = NULL, lib = NULL, repos = getOption("repos")[1],
                    contriburl = NULL, dependencies = NA, 
                    type = getOption("pkgType")) {
    pkgs <- if (is.null(char)) cnscinfun() else char
	if (is.null(pkgs)) return(NULL)
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
	if (exists("crandb", envir = .GlobalEnv)) {
		ldeps <- p_deps(char = pkgs, recursive = TRUE, verbose = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv))
		pkgs  <- pkgs[order(lengths(ldeps))]
	}

    ## Select lib/lib2
    LP     <- .libPaths()
    # if (length(LP) != 1L) LP <- LP[length(LP) -1]
    LP     <- LP[seq_len(max(1, length(LP) - 1))]
    funlib <- function(lp, pkgs) {pkgs %in% list.files(lp)}
    lstlib <- sapply(LP, funlib, pkgs, simplify = FALSE)
    vecsum <- sapply(lstlib, sum)
    veclib <- names(vecsum)[which(vecsum != 0)]
    lenlib <- length(veclib)
    if (lenlib >= 2) stop(
    "Packages are stored in several directories of .libPaths().
      Split your call and provide one single lib per call.")
    if (lenlib == 0) { lib2 <- if (is.null(lib)) LP else lib[1] }
    if (lenlib == 1) { lib2 <- if (is.null(lib)) veclib else lib[1] }

    if (is.null(contriburl)) {
        utils::install.packages(pkgs, lib = lib2, repos = repos,
            method = "libcurl", type = type)
    } else {
        utils::install.packages(pkgs, lib = lib2, repos = repos,
            contriburl = contriburl, method = "libcurl",
            depencies = dependencies, type = type)
    }
}

#' @export
#' @rdname p_inst
p_oldpkgs <- function(type = "both", lib.loc = .libPaths()[1], 
                      repos = getOption("repos")[1]) {
	insP <- installed.packages(lib.loc = lib.loc, priority = NA_character_,
                                noCache = TRUE)
	pkgs <- utils::old.packages(lib.loc = lib.loc, repos = repos, type = type, 
	                            instPkgs = insP, method = "libcurl")[,"Package"]
	if (is.null(pkgs)) return(NULL)
	if (exists("crandb", envir = .GlobalEnv)) {
		ldeps <- p_deps(char = pkgs, recursive = TRUE, verbose = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv))
		message("Packages were sorted by their number of dependencies.")
		pkgs  <- pkgs[order(lengths(ldeps))]
	} else message(
		"Packages were not sorted as crandb is not loaded in .GlobalEnv.")
	unname(pkgs)
}

#' @export
#' @rdname p_inst
p_inst_batsh <- function(..., char = NULL, type = "binary", verbose = TRUE, 
			dir = ".", lib = .libPaths()[1], repos = getOption("repos")[1]) {
    pkgs <- if (is.null(char)) cnscinfun() else char
	if (is.null(pkgs)) return(NULL)
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
	if (exists("crandb", envir = .GlobalEnv)) {
		ldeps <- p_deps(char = pkgs, recursive = TRUE, verbose = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv))
		pkgs  <- pkgs[order(lengths(ldeps))]
	}
	if (.Platform$OS.type == "windows") {
		fileR <- file.path(dir, "installrpkgs.R")
		conR  <- file(fileR, open = "w+", encoding = "UTF-8")
		writeLines("install.packages(", con = conR)
		writeLines(paste0(deparse1(pkgs), ","), con = conR)
		writeLines(paste0('lib = "', lib, '",'), con = conR)
		writeLines(paste0('repos = "', repos, '",'), con = conR)
		writeLines(paste0('type = "', type, '")'), con = conR)
		close(conR)
		#
		filebat <- file.path(dir, "installrpkgs.bat")
		conBAT  <- file(filebat, open = "w+", encoding = "UTF-8")
		bat0 <- paste0('"', file.path(R.home("bin"), "Rscript"), '"')
		bat1 <- if (verbose) { 
				' --vanilla --verbose --default-packages=utils "installrpkgs.R"'
			} else { 
				' --vanilla --default-packages=utils "installrpkgs.R"' 
			}
		writeLines(paste0(bat0, bat1), con = conBAT)
		close(conBAT)
	} else {
		fileSH <- file.path(dir, "installrpkgs.sh")
		conSH  <- file(fileSH, open = "w+", encoding = "UTF-8")
		shs0 <- file.path(R.home("bin"), "Rscript")
		shs1 <- if (verbose) {
				paste0("#! ", shs0, " --verbose --vanilla --default-packages=utils")
			} else {
				paste0("#! ", shs0, " --vanilla --default-packages=utils")
			}
		writeLines(shs1, con = conSH)
		writeLines("", con = conSH)
		writeLines("install.packages(", con = conSH)
		writeLines(paste0(deparse1(pkgs), ','), con = conSH)
		writeLines(paste0('lib = "', lib, '",'), con = conSH)
		writeLines(paste0('repos = "', repos, '",'), con = conSH)
		writeLines(paste0('type = "', type, '")'), con = conSH)
		close(conSH)
		Sys.chmod(fileSH)
	}
}


