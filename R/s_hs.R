## s_hs
## @include s_crandb_tvdb.R


#' @title Search Packages and Functions in Installed Packages
#' @description
#' \code{s_hs} is a wrapper of the well known function \code{??} and its parent function
#' \code{help.search}. Visit the help page \code{\link[utils]{help.search}} for details 
#' on the various arguments. 
#' 
#' @param   ...       one single character string recognized by \code{\link{cnsc}}.
#'                    One and only one pattern
#' @param   char      (name to) a single character string. Use this argument if \code{...} 
#'                    fails or if you call the function from another function. If used, 
#'                    argument \code{...} is ignored. 
#' @param   fields      See \code{help.search}.
#' @param   apropos     See \code{help.search}.
#' @param   keyword     See \code{help.search}.
#' @param   whatis      See \code{help.search}.
#' @param   ignore.case See \code{help.search}.
#' @param   package     See \code{help.search}.
#' @param   agrep       See \code{help.search}.
#' @param   use_UTF8    See \code{help.search}.
#' 
#' @examples
#' \donttest{ 
#' s_hs("neural network")
#' }
#' @export
#' @name s_hs
s_hs <- function(..., char = NULL, fields = c("alias", "concept", "title"),
            apropos, keyword, whatis, ignore.case = TRUE,
            package = NULL, agrep = NULL, use_UTF8 = FALSE) {
    pattern <- if (is.null(char)) cnscinfun() else char
    if (is.list(pattern)) stop("... (or char) cannot be a list.")
utils::help.search(pattern, fields,
            apropos, keyword, whatis, ignore.case = ignore.case,
            package = package, agrep = agrep, use_UTF8 = use_UTF8)
}



