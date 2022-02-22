## h_ttp
## @include h_R.R


#' @title Open a Web Page in the Browser
#' @description
#' \code{h_ttp} opens the page coresponding to the mentionned address in the default browser.
#' @param   ...      any format recognized by \code{\link{cnsc}}, except list.
#'                   A regular web address.
#' @param   char     (name to) a character vector. Use this argument if
#'                   \code{...} fails or if you call the function from another function.
#' @param   https    logical. Use https or http.
#' @param   www      logical. Add www. to the address.
#' @examples
#' if (interactive()) {
#' h_ttp("www.r-project.org")
#' }
#' @export
#' @name h_ttp
h_ttp <- function(..., char = NULL, https = TRUE, www = FALSE) {
    address <- if (is.null(char)) cnscinfun() else char
    h <- if (https) "https://" else "http://"
    w <- if (www) "www." else ""
    url <- paste0(h, w, address)
    msT <- paste("Open in the browser:", url)
    trybrowseURL(url, msgT = msT)
}


