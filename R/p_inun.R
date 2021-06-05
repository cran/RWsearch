## p_inun
## @include p_inst.R


#' @title List of Installed, Uninstalled and Non-Existing Packages
#' @description
#' \code{p_incrandb} returns TRUE if all packages are listed in \code{crandb} and a vector 
#' of FALSE with the names of the packages not listed in \code{crandb}.
#' 
#' \code{p_inun} returns a list of packages installed or not installed in the computer.
#' 
#' \code{p_inun_crandb} checks if the packages exist or do not exist 
#' in \code{crandb} (see CRAN archives, Bioconductor, Github, your own packages).
#' 
#' The missing packages available on CRAN can be downloaded with \code{\link{p_down0}}, 
#' downloaded and checked (by R CMD check) with \code{xfun::rev_check} or installed 
#' with \code{install.packages}. The packages removed from CRAN but available in
#' CRAN archive can be downloaded with \code{\link{p_downarch}}. 
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, including list.
#'                    A vector or a list of packages.
#' @param   char      (name to) a character vector or a list. Use this argument if 
#'                    \code{...} fails or if you call the function from another function. 
#'                    If used, argument \code{...} is ignored. 
#' @param   crandb    data.frame \code{crandb}.
#' @examples 
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' ## Check if packages are installed or not, and exist or not in crandb
#' p_incrandb(RWsearch, NotAPkg1, pacman, NotAPkg2, sos) 
#' p_inun(RWsearch, NotAPkg1, pacman, NotAPkg2, sos) 
#' p_inun_crandb(RWsearch, NotAPkg1, pacman, NotAPkg2, sos)  
#' 
#' @name p_inun
NULL

#' @export
#' @rdname p_inun
p_incrandb <- function(..., char = NULL, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    TFpkgs <- (pkgs %in% crandb$Package) 
    names(TFpkgs) <- pkgs 
    if (all(TFpkgs)) all(TFpkgs) else TFpkgs[!TFpkgs]
}

#' @export
#' @rdname p_inun
p_inun <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        mapply(FUN = p_inun, pkgs, char = pkgs, SIMPLIFY = FALSE)
    } else {
        list(installed   = pkgs[ is.element(pkgs, list.files(.libPaths()))],
             uninstalled = pkgs[!is.element(pkgs, list.files(.libPaths()))])
    }
}

#' @export
#' @rdname p_inun
p_inun_crandb <- function(..., char = NULL, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        mapply(FUN = p_inun, pkgs, char = pkgs, 
               MoreArgs = list(crandb = crandb), SIMPLIFY = FALSE)
    } else {
        inpkgs <- pkgs[ is.element(pkgs, list.files(.libPaths()))]
        unpkgs <- pkgs[!is.element(pkgs, list.files(.libPaths()))]
        in_incrandb    <- inpkgs[ is.element(inpkgs, crandb$Package)]
        in_notincrandb <- inpkgs[!is.element(inpkgs, crandb$Package)]
        un_incrandb    <- unpkgs[ is.element(unpkgs, crandb$Package)]
        un_notincrandb <- unpkgs[!is.element(unpkgs, crandb$Package)]
        list(installed       = inpkgs,
             uninstalled     = unpkgs,
             in_incrandb     = in_incrandb,
             in_notincrandb  = in_notincrandb,
             un_incrandb     = un_incrandb,
             un_notincrandb  = un_notincrandb)
    }
}



