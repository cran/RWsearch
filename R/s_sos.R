## s_sos
## @include s_hs.R


#' @title Search Packages and Functions on R-Project Help pages and RDocumentation
#' @description
#' \code{s_sos} searchs in all R documentation packages and functions that contain
#' one or several keywords, open the default browser and display the results in a html page.
#' For one or two keywords, \code{s_sos} may find more results than \code{\link{s_crandb}}
#' as it goes deeper in the documentation, down to the function level. An internet
#' connection is required to reach the website maintained by the R-Project.
#' This service replaces the one maintained by the University of Pennsylvania up to 2021.
#'
#' \code{s_sos} is a minimal wrapper of the function \emph{sos::findFn}.
#' Use directly the package \emph{sos} and read its vignette for advanced search options.
#'
#' \code{s_man} and \code{s_Rman} display the results in the query's raw format.
#'
#' @param   ...       any format recognized by \code{\link{cnsc}}, except list.
#'                    One or several keywords.
#' @param   char      (name to) a character vector. Use this argument if \code{...} fails
#'                    or if you call the function from another function. If used,
#'                    argument \code{...} is ignored.
#' @seealso  \url{https://search.r-project.org} to search for functions rather than packages,
#'
#'           \url{https://CRAN.R-project.org/package=sos} (index and vignette).
#'
#' @export
#' @examples
#' ## Search using standard or non-standard content
#' ## and display the results in a browser.
#' if (interactive()) {
#' s_sos("chemical reaction")
#' (res <- s_sos(distillation))
#' tail(data.frame(res))
#' s_man("cran_incoming")
#' }
#' @name s_sos
s_sos <- function(..., char = NULL) {
    keywords <- if (is.null(char)) cnscinfun() else char
    if (is.list(keywords)) stop("... cannot be a list.")
    string <- paste(keywords, collapse = " ")
    sos::findFn(string = string)
}

#' @export
#' @rdname s_sos
s_man <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    if (is.list(words)) stop("... (or char) cannot be a list.")
    for (word in words) {
        url <- paste0("https://search.r-project.org/?P=", word)
        trybrowseURL(url)
    }
}

#' @export
#' @rdname s_sos
s_Rman <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    if (is.list(words)) stop("... (or char) cannot be a list.")
    for (word in words) {
        url <- paste0("https://search.r-project.org/?P=", word)
        trybrowseURL(url)
    }
}



