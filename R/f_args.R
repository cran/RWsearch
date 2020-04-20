## f_args
## @include e_check.R


#' @title Names and Arguments of Functions
#' @description
#' 
#' \code{f_args} is a wrapper of the base function \code{\link{args}}.
#' 
#' \code{f_sig} prints the name and arguments of one or several functions in a readable 
#' style. It wraps the function \code{sig::sig}.
#' 
#' \code{p_sig} prints the name and arguments of the functions exported by one or 
#' several packages. It wraps the function \code{sig::list_sigs}.
#' 
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     A vector of unquoted "functions" or characters.
#' @param   char       (name to) a character vector. Use this argument if 
#'                     \code{...} fails or if you call the function from another function. 
#'                     If used, argument \code{...} is ignored. 
#' @param   pattern    a regular expression. See the example.
#' @examples
#' f_args(mean, p_display)
#' f_sig( mean, p_display)
#' 
#' library(brew)
#' library(sos)
#' p_sig(brew)
#' p_sig(RWsearch, sos, pattern = "^f")
#' 
#' @export
#' @name f_args
f_args <- function(..., char = NULL) {
    funs <- if (is.null(char)) cnscinfun() else char
    if (is.list(funs)) stop("... (or char) cannot be a list.")
    if (length(funs) == 1L) {
        args(funs)
    } else {
        sapply(funs, args, USE.NAMES = TRUE)        
    }
}

#' @export
#' @rdname f_args
f_sig <- function(..., char = NULL) {
    funs <- if (is.null(char)) cnscinfun() else char
    if (is.list(funs)) stop("... (or char) cannot be a list.")
    if (length(funs) == 1L) {
        if (is.function(funs)) {
            sig::sig(funs) 
        } else { 
            sig::sig(match.fun(funs), funs)
        }
    } else {
        lst <- as.list(funs)
        names(lst) <- funs
        lst <- lapply(lst, match.fun)
        Map(sig::sig, lst, funs)         
    }
}

#' @export
#' @rdname f_args
p_sig <- function(..., char = NULL, pattern = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    pkg3env <- function(pkg_name) {
        if(!pkg_name %in% .packages()) {
            if(pkg_name %in% .packages(TRUE)) {
                message("Loading package ", sQuote(pkg_name), ".")
                library(pkg_name, character.only = TRUE)
            } else {
                stop("The package ", sQuote(pkg_name), " is not available.")
            }
        }
        as.environment(paste0("package:", pkg_name))
    }
    fun_sig <- function(pkgs, pattern) sig::list_sigs(pkg3env(pkgs), pattern)
    if (length(pkgs) == 1L) {
        fun_sig(pkgs, pattern)
    } else {
        lst <- as.list(pkgs)
        names(lst) <- pkgs
        lapply(lst, fun_sig, pattern)
    }
}



