## e_check
## @include cranmirrors.R


#' @title Check Results of Packages Identified by their Email Address
#' @description
#' \code{e_check} opens the browser and returns the "CRAN Check Results" page(s)
#' of the packages maintained by one or several maintainers identified by their
#' regular email addresses (but not the orphaned ones). An internet connection
#' is required. This function is a simplified version of the functions proposed
#' in the package \emph{foghorn}.
#'
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     A vector of quoted "e-mail adresses".
#' @param   char       (name to) a character vector. Use this argument if
#'                     \code{...} fails or if you call the function from another function.
#'                     If used, argument \code{...} is ignored.
#' @param   repos      character. The address of your local CRAN.
#' @examples
#' if (interactive()) {
#' e_check(c("rpackages@inmodelia.com", "christophe.dutang@ensimag.fr"),
#'         repos = "https://cloud.r-project.org")
#' }
#' @export
#' @name e_check
e_check <- function(..., char = NULL, repos = getOption("repos")[1]) {
    email <- if (is.null(char)) cnscinfun() else char
    if (is.list(email)) stop("... (or char) cannot be a list.")
    lapply(email, function(x) if (!grepl("\\@", x)) {
        stop("Malformed email address: ", sQuote(email), call. = FALSE)
    })
    email <- gsub("\\@", "_at_", tolower(email))
    email <- gsub("[^[:alnum:]_:.-]", "_", email)
    url   <- paste0(repos, "/web/checks/check_results_", email, ".html")
    names(sapply(url, trybrowseURL))
}



