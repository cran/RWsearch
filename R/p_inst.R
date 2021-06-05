## p_inst
## @include p_html.R


#' @title A simple wrapper around install.packages()
#' 
#' @description
#' \code{p_inst} is a wrapper around \code{\link[utils]{install.packages}} which tries
#' hard to select the most appropriate \code{lib} and stops in case of conflict. 
#' 
#' \code{...} allows a non-standard evaluation of unquoted packages separated by commas.
#' Use \code{install.packages} if any additional argument is needed. 
#' 
#' @param  ...     any format recognized by \code{\link{cnsc}}, excluding list.
#'                 A vector of packages.
#' @param  char    (name to) a character vector or a list. Use this argument if 
#'                 \code{...} fails or if you call the function from another function. 
#'                 If used, argument \code{...} is ignored.
#' @param  lib     character. The directory where to install the packages, usually
#'                 one of the directories listed by \code{\link{.libPaths}}. If NULL, 
#'                 select automatically the most relevant directory. 
#' @param  repos   character. The address of your local CRAN.
#' @param  contriburl   character. The address of your private repository.
#' @param  type    character. Either \code{"source"}, \code{"both"}, \code{"binary"}
#'                 (or its variants \code{"mac.binary"}, \code{"mac.binary.el-capitan"},   
#'                 \code{"win.binary"}). 
#' 
#' @export
#' @name p_inst
p_inst <- function (..., char = NULL, lib = NULL, repos = getOption("repos"), 
                    contriburl = NULL, type = getOption("pkgType")) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    
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
            contriburl = contriburl, method = "libcurl", type = type)
    }
}



