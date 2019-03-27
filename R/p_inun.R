## p_inun
## @include p_html.R


#' @title List of Installed, Uninstalled and Non-Existing Packages
#' @description
#' \code{p_inun} returns a list of packages installed or not installed in the computer.
#' 
#' In addition, \code{p_inun_cran} checks if the packages exist or do not exist in 
#' \code{crandb}. This latest information reminds you about all your private unpublished 
#' packages. 
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
#' p_inun(RWsearch, NotAPkg1, pacman, NotAPkg2, sos) 
#' p_inun_cran(RWsearch, NotAPkg1, pacman, NotAPkg2, sos)  
#' 
#' @export
#' @name p_inun
p_inun <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        lst <- mapply(FUN = p_inun, pkgs, char = pkgs, SIMPLIFY = FALSE)
    } else {
        lst <- list(installed   = pkgs[ is.element(pkgs, list.files(.libPaths()))],
                    uninstalled = pkgs[!is.element(pkgs, list.files(.libPaths()))])
    }
return(lst)
}

#' @export
#' @rdname p_inun
p_inun_cran <- function(..., char = NULL, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        lst <- mapply(FUN = p_inun, pkgs, char = pkgs, 
                      MoreArgs = list(crandb = crandb), SIMPLIFY = FALSE)
    } else {
        inpkgs <- pkgs[ is.element(pkgs, list.files(.libPaths()))]
        unpkgs <- pkgs[!is.element(pkgs, list.files(.libPaths()))]
        in_incran    <- inpkgs[ is.element(inpkgs, crandb$Package)]
        in_notincran <- inpkgs[!is.element(inpkgs, crandb$Package)]
        un_incran    <- unpkgs[ is.element(unpkgs, crandb$Package)]
        un_notincran <- unpkgs[!is.element(unpkgs, crandb$Package)]
        lst <- list(installed     = inpkgs,
                    uninstalled   = unpkgs,
                    in_incran     = in_incran,
                    in_notincran  = in_notincran,
                    un_incran     = un_incran,
                    un_notincran  = un_notincran)
    }
return(lst)
}



