## tvdb
## @include s_sos.R


#' @title Search Packages in Task Views
#' @description
#' \code{s_tvdb} searchs if one or several package(s) are referred in some task views
#' and lists these task views.
#'
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     The names of one or several task views.
#' @param   char       (name to) a character vector or a list. Use this argument if
#'                     \code{...} fails or if you call the function from another function.
#'                     If used, argument \code{...} is ignored.
#' @param   tvdb       list. The list of the task views.
#' @examples
#' ## In real life, download tvdb from CRAN or load it from your directory
#' ## with functions tvdb_down() or tvdb_load().
#' ## In this example, we use a small file.
#' tvdb_load(system.file("data", "ztvdb.rda", package = "RWsearch"))
#' tvdb_dfr()
#' s_tvdb(actuar, FatTailsR, MASS, zoo, NotAPkg)
#'
#' @export
#' @name s_tvdb
s_tvdb <- function(..., char = NULL, tvdb = get("tvdb", envir = .GlobalEnv)) {
    words <- if (is.null(char)) cnscinfun() else char
    if (is.list(words)) stop("... cannot be a list.")
    tvdbL <- tvdb_list()
    funW  <- function(word, tvdbL) {
        lst <- sapply(tvdbL, is.element, word)
        vecTF <- sapply(lst, any)
    vecTF[vecTF]
    }
    if (length(words) == 1L) {
        funW(words, tvdbL)
    } else {
        sapply(words, funW, tvdbL, simplify = FALSE)
    }
}


