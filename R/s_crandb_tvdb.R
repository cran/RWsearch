## s_crandb_tvdb
## @include s_crandb.R


#' @title Search For Recent Packages In crandb And In Task View
#' @description
#' This is a function for task view maintenance.
#' 
#' Search packages in a subset of \code{crandb} within dates \code{from} and \code{to} 
#' that contain one or several keywords in the columns "Package", "Title", "Description", 
#' 'Author" or "Maintainer", then verify if these packages are already refereed in one 
#' of the task views stored in \code{tvdb}.
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, except list.
#'                    One or several keywords. 
#' @param   char      (name to) a character vector. Use this argument if \code{...} fails 
#'                    or if you call the function from another function. If used, 
#'                    argument \code{...} is ignored. 
#' @param   tv        character. One task view among those listed in tvdb. 
#' @param   from      character representing a date earlier than date \code{to}. 
#'                    Or a negative integer representing the number of days preceeding 
#'                    the date \code{to}. 
#' @param   to        date. The upper date in the search.
#' @param   select    character vector. A sub-vector of \code{colnames(crandb)}. The 
#'                    short form "P", "T", "D", "PT", "PD", "TD", "PTD", "A", "M", "AM" 
#'                    describing the Package name, Title, Description, Author, Maintainer
#'                    or a combination of them is accepted.
#' @param   sensitive logical. \code{TRUE} forces the search to be case sensitive.
#' @param   perl      logical. Used only if \code{fixed = FALSE}. \code{TRUE} uses 
#'                    Perl-compatible regex. \code{FALSE} uses default regexps. 
#' @param   fixed     logical. \code{TRUE} matchs the keywords as is (and \code{sensitive} 
#'                    is forced to \code{TRUE}). \code{FALSE} allows grep or Perl regexps. 
#'                    See \code{\link[base]{grep}}. Not used by \code{agrep}.
#' @param   mode      character among "or", "and", "relax". The search mode. 
#'                    "relax" is for 3 words and more. It is an intermediate between 
#'                    "or" and "and" as it requires just 2 matching words: 
#'                    \code{("word1" AND "word2")} \code{OR} \code{("word1" AND "word3")} 
#'                    \code{OR} \code{("word1" AND "word3")}.
#' @param   agrep     logical. For approximate matching, use \code{\link[base]{agrep}} 
#'                    function rather than grep.
#' @param   max.distance    integer or numeric. See \code{\link[base]{agrep}}. 
#' @param   costs     NULL or list. See \code{\link[base]{agrep}}. 
#' @param   crandb    data.frame \code{crandb}.
#' @param   tvdb      list. The list of the task views. 
#' @return  
#' A list with the following vectors:
#' \itemize{
#'   \item{spkgs: }{The selected packages that contain the keyword(s).}
#'   \item{inTV: }{The packages that contain the keyword(s) already refereed in the task view.}
#'   \item{notinTV: }{The packages that contain the keyword(s) not (yet) refereed in the task view.}
#'   \item{inTV_in: }{Among the packages available in the task view, those installed in the computer.}
#'   \item{inTV_un: }{Among the packages available in the task view, those not installed in the computer.}
#'   \item{notinTV_in: }{Among the packages not refereed in the task view, those installed in the computer.} 
#'   \item{notinTV_un: }{Among the packages not refereed in the task view, those not installed in the computer.}
#' }
#' @examples
#' ### TASK VIEW MAINTENANCE (tvdb + crandb)
#' ## In real life, download crandb and tvdb from CRAN or load them from your directory 
#' ## with functions crandb_down(), crandb_load(), tvdb_down(), tvdb_load(). 
#' ## In this example, we use small files.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' tvdb_load(system.file("data", "ztvdb.rda", package = "RWsearch")) 
#' 
#' ## List the task views
#' tvdb_vec()
#' 
#' ## Search for the recent packages in crandb that contain the keyword 
#' ## and verify the packages already refereed in the task view.
#' (lst <- s_crandb_tvdb("distribution", tv = "Distributions", from = "2018-01-01"))
#' \donttest{ 
#' p_display7(lst[c("inTV", "notinTV")], dir = tempdir())
#' }
#' @export
#' @name s_crandb_tvdb
s_crandb_tvdb <- function(..., char = NULL, tv = "Distributions", 
                        from = -10, to = Sys.Date(), 
                        select = "PTD", mode = "or", sensitive = FALSE, 
                        perl = FALSE, fixed = FALSE, 
                        agrep = FALSE, max.distance = 0.1, costs = NULL,
                        crandb = get("crandb", envir = .GlobalEnv),
                        tvdb = get("tvdb", envir = .GlobalEnv)) {
    if (!is.list(tvdb)) stop("tvdb is not loaded.")
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    if (is.element(tv, tvdb_vec(tvdb))) {
        PTV  <- tvdb_pkgs(char = tv, tvdb = tvdb) 
    } else {
        stop(paste(tv, "is not a task view listed in tvdb."))
    }
    select  <- fcccrandb(select, crandb)
    keyword <- if (is.null(char)) cnscinfun() else char
    if (is.list(keyword)) stop("... (or char) cannot be a list.")
    pkgsFT <- crandb_fromto(from, to, crandb) 
    cranFT <- crandb[is.element(crandb$Package, pkgsFT), ]
    spkgs  <- s_crandb(char = keyword, select = select, 
                    mode = mode, sensitive = sensitive, agrep = agrep, 
                    max.distance = max.distance, costs = costs, 
                    crandb = cranFT)
    spkgsTV  <- is.element(spkgs, PTV)
    InTV     <- spkgs[ is.element(spkgs, PTV)]
    NotInTV  <- spkgs[!is.element(spkgs, PTV)]
    FLP      <- list.files(.libPaths())
    InTV_in  <- InTV[ is.element(InTV, FLP)]
    InTV_un  <- InTV[!is.element(InTV, FLP)]
    NotInTV_in <- NotInTV[ is.element(NotInTV, FLP)]
    NotInTV_un <- NotInTV[!is.element(NotInTV, FLP)]
    lst <- list(  "spkgs" = spkgs,
                   "inTV" = InTV, 
                "notinTV" = NotInTV, 
                "inTV_in" = InTV_in, 
                "inTV_un" = InTV_un, 
             "notinTV_in" = NotInTV_in, 
             "notinTV_un" = NotInTV_un)
lst
}



