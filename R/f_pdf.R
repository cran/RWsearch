## f_pdf
## @include f_args.R


#' @title PDF Pages of Functions
#' @description
#'
#' \code{f_pdf} generates in the current directory the pdf pages of one or several
#' functions. The pdf pages are printed but not opened. Miktex or Texlive is required.
#' This function wraps \code{{utils::help}} with the pdf option activated.
#' Similar functions are available in the packages \emph{document} and \emph{sinew}.
#'
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     A vector of quoted "package::function".
#' @param   char       (name to) a character vector. Use this argument if
#'                     \code{...} fails or if you call the function from another function.
#'                     If used, argument \code{...} is ignored.
#' @examples
#' ## FALSE is here to avoid a NOTE in CRAN checks. Ignore this line.
#' if (FALSE) {
#'    f_pdf(c("RWsearch::cnsc", "RWsearch::p_inun"))
#' }
#' @export
#' @name f_pdf
f_pdf <- function(..., char = NULL) {
    funs <- if (is.null(char)) cnscinfun() else char
    if (is.list(funs)) stop("... (or char) cannot be a list.")
    lst   <- strsplit(funs, "::" )
    funwr <- function(vec) utils::help((vec[2]), (vec[1]), help_type = "pdf")
    lapply(lst, funwr)
}



