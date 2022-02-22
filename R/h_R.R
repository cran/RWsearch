## h_R
## @include h_engine.R


#' @title Open a Web Page in the Browser
#' @description
#' \code{h_R} opens the page \url{https://www.r-project.org}.
#' \code{h_Rml} opens the page dedicated to the mailing lists
#' \url{https://www.r-project.org/mail.html}.
#' \code{h_Rnews} opens the page \url{https://cran.r-project.org/doc/manuals/r-devel/NEWS.html}.
#' \code{h_Rversions} opens a page (from rversions package) that keeps a record
#' of all R versions and their release dates.
#'
#' \code{h_cran} opens the page of you local CRAN.
#'
#' \code{h_cranbydate} and \code{h_cranbyname} open the page of CRAN packages
#' sorted by date of publication and in alphabetical order.
#'
#' \code{h_cranchecks} and \code{h_crancheckwindows} open the pages related to
#' the checks of all packages listed by name, maintainers, dates, os. A special
#' page is dedicated to Windows packages with the results for the previous, the
#' current and the devel R versions.
#'
#' \code{h_crantv} opens the page of CRAN task views.
#'
#' \code{h_cranberries}, \code{h_nabble}, \code{h_rbloggers}, \code{h_rdoc}, \code{h_rdoctv}
#' (RDocumentation), \code{h_rdrr}, \code{h_rseek} open the pages of web sites related
#' to R.
#'
#' \code{h_gepuro} lists all (most) R packages available on GitHub. A huge file.
#'
#' @param   repos    character. The address of your local CRAN.
#' @param   ...      any format recognized by \code{\link{cnsc}}, except list.
#'                   A regular web address.
#' @param   char     (name to) a character vector. Use this argument if
#'                   \code{...} fails or if you call the function from another function.
#' @examples
#' if (interactive()) {
#' h_crantv(repos = "https://cloud.r-project.org")
#' h_cranberries()
#' }
#' @export
#' @name h_R
h_R <- function() {
    msg <- "Open www.r-project.org in the browser"
    url <- "https://www.r-project.org"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_Rml <- function() {
    msg <- "Open R mailing lists in browser"
    url <- "https://www.r-project.org/mail.html"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_Rnews <- function(repos = getOption("repos")[1]) {
    msg <- "Open R devel NEWS page in browser"
    url <- paste0(repos, "/doc/manuals/r-devel/NEWS.html")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_Rversions <- function(repos = getOption("repos")[1]) {
    msg <- "Open rversions README which keeps a record of R versions"
    url <- paste0(repos, "/web/packages/rversions/readme/README.html")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_cran <- function(repos = getOption("repos")[1]) {
    msg <- "Open CRAN in browser"
    trybrowseURL(repos, msgT = msg)
}

#' @export
#' @rdname h_R
h_cranbydate <- function(repos = getOption("repos")[1]) {
    msg <- "Open CRAN by date in browser"
    url <- paste0(repos, "/web/packages/available_packages_by_date.html")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_cranbyname <- function(repos = getOption("repos")[1]) {
    msg <- "Open CRAN by name in browser"
    url <- paste0(repos, "/web/packages/available_packages_by_name.html")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_cranchecks <- function(repos = getOption("repos")[1]) {
    msg <- "Open CRAN checks in browser"
    url <- paste0(repos, "/web/checks")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_crancheckwindows <- function(repos = getOption("repos")[1]) {
    msg <- "Open CRAN check Windows in browser"
    url <- paste0(repos, "/bin/windows/contrib/checkSummaryWin.html")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_crantv <- function(repos = getOption("repos")[1]) {
    msg <- "Open CRAN task views in browser"
    url <- paste0(repos, "/web/views/index.html")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_cranstatus <- function() {
    msg <- "Open CRAN mirror status in browser"
    url <- "https://cran.r-project.org/mirmon_report.html"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_cranberries <- function() {
    msg <- "Open Cranberries in browser"
    url <- "http://dirk.eddelbuettel.com/cranberries"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_gepuro <- function() {
    msg <- "Open Gepuro in browser"
    url <- "http://rpkg.gepuro.net"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_nabble <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Nabble results for:", words)
    url <- furl("http://r.789695.n4.nabble.com/template/NamlServlet.jtp?macro=search_page&node=789695&query=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_rbloggers <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("R-bloggers results for:", words)
    url <- furl("https://www.r-bloggers.com/?q=", words, encode = TRUE)
    trybrowseURL(url, msgT = msg)
}
## Ne marche pas. Utilise Google cache

#' @export
#' @rdname h_R
h_rdoc <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Rdocumentation results for:", words)
    url <- furl("https://www.rdocumentation.org/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_rdoctv <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Rdocumentation results for task views:", words)
    url <- furl("https://www.rdocumentation.org/taskviews#", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_rdrr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Rdrr results for:", words)
    url <- furl("https://rdrr.io/search?q=", words)
    trybrowseURL(url, msgT = msg)
}
## probleme avec les accents

#' @export
#' @rdname h_R
h_rseek <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Rseek results for:", words)
    url <- furl("https://rseek.org/?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_R
h_biocstats <- function() {
    msg <- "Open Bioconductor statistics page in browser"
    url <- "https://bioconductor.org/packages/stats"
    trybrowseURL(url, msgT = msg)
}



