## h_R
## @include h_engine.R


#' @title Open a Web Page in the Browser
#' @description 
#' \code{h_R} opens the page \url{https://www.r-project.org}.
#' 
#' \code{h_cran} opens the page of you local CRAN.  
#' 
#' \code{h_crandate} opens the page of CRAN packages sorted by date of publication.
#' 
#' \code{h_crantv} opens the page of CRAN task views.
#' 
#' \code{h_cranberries}, \code{h_nabble}, \code{h_rbloggers}, \code{h_rdoc}, \code{h_rdoctv} 
#' (RDocumentation), \code{h_rdrr}, \code{h_rseek} open the pages of web sites related 
#' to R.
#' 
#' \code{h_gepuro} lists all (most) R packages available on GitHub.
#' 
#' @param   repos    character. The address of your local CRAN.
#' @param   ...      any format recognized by \code{\link{cnsc}}, except list.
#'                   A regular web address.
#' @param   char     (name to) a character vector. Use this argument if 
#'                   \code{...} fails or if you call the function from another function.
#' @examples
#' \donttest{
#' h_cranbydate()
#' h_cranberries()
#' }
#' @export
#' @name h_R
h_R <- function() {
    message("Open www.r-project.org in browser")
utils::browseURL("https://www.r-project.org")
}

#' @export
#' @rdname h_R
h_cran <- function(repos = getOption("repos")[1]) {
    message("Open CRAN in browser")
utils::browseURL(repos)
}

#' @export
#' @rdname h_R
h_cranbydate <- function(repos = getOption("repos")[1]) {
    z <- paste0(repos, "/web/packages/available_packages_by_date.html")
    message("Open CRAN by date in browser")
utils::browseURL(z)
}

#' @export
#' @rdname h_R
h_cranbyname <- function(repos = getOption("repos")[1]) {
    z <- paste0(repos, "/web/packages/available_packages_by_name.html")
    message("Open CRAN by name in browser")
utils::browseURL(z)
}

#' @export
#' @rdname h_R
h_crantv <- function(repos = getOption("repos")[1]) {
    z <- paste0(repos, "/web/views/index.html")
    message("Open CRAN task views in browser")
utils::browseURL(z)
}

#' @export
#' @rdname h_R
h_cranberries <- function() {
    message("Open Cranberries in browser")
utils::browseURL("http://dirk.eddelbuettel.com/cranberries")
}

#' @export
#' @rdname h_R
h_gepuro <- function() {
    message("Open Gepuro in browser")
utils::browseURL("http://rpkg.gepuro.net")
}

#' @export
#' @rdname h_R
h_nabble <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Nabble results for:", words)
    fbr("http://r.789695.n4.nabble.com/template/NamlServlet.jtp?macro=search_page&node=789695&query=", words)
}

#' @export
#' @rdname h_R
h_rbloggers <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("R-bloggers results for:", words)
    fbr("https://www.r-bloggers.com/?q=", words, encode = TRUE)
}
## Ne marche pas. Utilise Google cache

#' @export
#' @rdname h_R
h_rdoc <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Rdocumentation results for:", words)
    fbr("https://www.rdocumentation.org/search?q=", words)
}

#' @export
#' @rdname h_R
h_rdoctv <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Rdocumentation results for task views:", words)
    fbr("https://www.rdocumentation.org/taskviews#", words)
}

#' @export
#' @rdname h_R
h_rdrr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Rdrr results for:", words)
    fbr("https://rdrr.io/search?q=", words)
}
## probleme avec les accents

#' @export
#' @rdname h_R
h_rseek <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Rseek results for:", words)
    fbr("https://rseek.org/?q=", words)
}



