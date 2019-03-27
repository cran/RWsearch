## s_crandb
## @include p_text2pdf.R


#' @title Search Packages by Keywords in data.frame crandb
#' @description
#' The most important functions in this package along with \code{\link{p_down}}.
#' 
#' Search packages in data.frame \code{crandb} that contain one or several keywords 
#' in the columns "Package", "Title", "Description", "Author" or "Maintainer". 
#' 
#' \code{s_crandb} returns a vector of the packages that contain the keywords. 
#' 
#' \code{s_crandb_list} returns a list where each element of the list is one of the 
#' keywords. 
#' 
#' \code{s_crandb_PTD} returns a list split by results in columns "Package", "Title" 
#' and "Description". Option \code{mode = "and", "relax"} is ignored.
#' 
#' \code{s_crandb_AM} returns a list split by results in columns "Author" and
#' "Maintainer". Option \code{mode = "and", "relax"} is ignored.
#' 
#' Use \code{\link{p_table2}} to print the results of \code{s_crandb} and
#' \code{s_crandb_list} in the console. 
#' Use \code{\link{p_text}} to send the results in txt, md or pdf files. 
#' Use \code{\link{p_display}} to visualize the results in html pages in the browser.
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, except list.
#'                    One or several keywords. 
#' @param   char      (name to) a character vector. Use this argument if \code{...} fails 
#'                    or if you call the function from another function. If used, 
#'                    argument \code{...} is ignored. 
#' @param   select    character vector. A sub-vector of \code{colnames(crandb)}. The 
#'                    short form "P", "T", "D", "PT", "PD", "TD", "PTD", "A", "M", "AM" 
#'                    describing the Package name, Title, Description, Author, Maintainer
#'                    or a combination of them is accepted.
#' @param   mode      character among "or", "and", "relax". The search mode. 
#'                    "relax" is for 3 words and more. It is an intermediate between 
#'                    "or" and "and" as it requires just 2 matching words: 
#'                    \code{("word1" AND "word2")} \code{OR} \code{("word1" AND "word3")} 
#'                    \code{OR} \code{("word1" AND "word3")}.
#' @param   sensitive logical. \code{TRUE} forces the search to be case sensitive.
#' @param   perl      logical. Used only if \code{fixed = FALSE}. \code{TRUE} uses 
#'                    Perl-compatible regex. \code{FALSE} uses default regexps. 
#' @param   fixed     logical. \code{TRUE} matchs the keywords as is (and \code{sensitive} 
#'                    is forced to \code{TRUE}). \code{FALSE} allows grep or Perl regexps. 
#'                    See \code{\link[base]{grep}}. Not used by \code{agrep}.
#' @param   agrep     logical. For approximate matching, use \code{\link[base]{agrep}} 
#'                    function rather than grep.
#' @param   max.distance    integer or numeric. See \code{\link[base]{agrep}}. 
#' @param   costs     NULL or list. See \code{\link[base]{agrep}}. 
#' @param   crandb    data.frame \code{crandb}.
#' @examples 
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' ## Search using standard or non-standard content
#' s_crandb(c("thermodynamic", "chemical reaction", "distillation"))
#' s_crandb_list(thermodynamic, "chemical reaction", distillation)
#' 
#' ## Search using the various options
#' s_crandb(pH, sensitive = TRUE)
#' s_crandb_PTD(pH, sensitive = TRUE)
#' s_crandb_PTD("C++", fixed = TRUE)
#' s_crandb(search, find, cran, web, select = "PD", mode = "and")
#' s_crandb(search, find, cran, web, select = "PD", mode = "relax")
#' s_crandb(search, find, cran, web, select = "PD", mode = "or")
#' 
#' ## Search for some authors using the various options
#' s_crandb_AM(Kiener, Dutang, ORPHANED)
#' 
#' ## Non-standard content can be unquoted words or objects in .GlobalEnv
#' ## They are transformed into character or are evaluated 
#' ## Here, the searched keywords are "find" and "search".
#' OTHER <- "search"
#' (lst <- s_crandb_list(find, OTHER, select = "P", sensitive = TRUE)) 
#' \donttest{
#' ## Display in the browser this list of packages
#' p_display5(lst, dir = tempdir())
#' }
#' 
#' @export
#' @name s_crandb
s_crandb <- function(..., char = NULL, select = "PTD", mode = "or", sensitive = FALSE, 
                        perl = FALSE, fixed = FALSE, 
                        agrep = FALSE, max.distance = 0.1, costs = NULL,
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    mode    <- match.arg(mode, choices = c("or", "and", "relax"))
    columns <- fcccrandb(select, crandb)
    words   <- if (is.null(char)) cnscinfun() else char
    if (is.list(words)) stop("... cannot be a list.")
    if (fixed) sensitive <- TRUE
    funcolumn <- function(column, word, sensitive, perl, fixed,
                          agrep, max.distance, costs, crandb) {
        if (agrep) {
            agrep(word, crandb[, column], max.distance = max.distance, 
                  costs = costs, ignore.case = !sensitive)
        } else {
            grep(word, crandb[, column], ignore.case = !sensitive, 
                 perl = perl, fixed = fixed)
        }
    }
    funword <- function(word, columns, sensitive, perl, fixed, 
                        agrep, max.distance, costs, crandb) {
        sort(unique(unlist(lapply(columns, funcolumn, word, sensitive, perl, fixed, 
                                  agrep, max.distance, costs, crandb))))
    }
    nums <- sort(unlist(lapply(words, funword, columns, sensitive, perl, fixed, 
                               agrep, max.distance, costs, crandb)))
    if ((mode == "and" || mode == "relax") && length(words) > 1L) {
            nums <- nums[duplicated(nums)]
        if (mode == "and" && length(words) > 2L) {
            nums <- nums[duplicated(nums)]
        }
    }
    pkgs <- crandb[sort(unique(nums)), "Package"]
return(pkgs)
}

#' @export
#' @rdname s_crandb
s_crandb_list <- function(..., char = NULL, select = "PTD", mode = "or", sensitive = FALSE, 
                        perl = FALSE, fixed = FALSE, 
                        agrep = FALSE, max.distance = 0.1, costs = NULL,
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    mode  <- match.arg(mode, choices = c("or", "and", "relax"))
    # match.arg(select, choices = c("P", "T", "D", "PT", "PD", "TD", "PTD", "A", "M", "AM"))
    select <- fcccrandb(select, crandb)
    words  <- if (is.null(char)) cnscinfun() else char
    if (is.list(words)) stop("... cannot be a list.")
    lst  <- vector("list", length(words))
    names(lst) <- words
    for (i in seq_along(words)) lst[[i]] <- s_crandb(char = words[i], 
               select = select, mode = mode, sensitive = sensitive, 
               perl = perl, fixed = fixed, agrep = agrep, max.distance = max.distance, costs = costs, 
               crandb = crandb)
return(lst)
}

#' @export
#' @rdname s_crandb
s_crandb_PTD <- function(..., char = NULL, mode = "or", sensitive = FALSE, 
                        perl = FALSE, fixed = FALSE, 
                        agrep = FALSE, max.distance = 0.1, costs = NULL,
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    mode  <- match.arg(mode, choices = c("or", "and", "relax"))
    words <- if (is.null(char)) cnscinfun() else char
    if (is.list(words)) stop("... cannot be a list.")
    funmot <- function(mot, mode, sensitive, perl, fixed, agrep, max.distance, costs, crandb) {
       list("Package" = s_crandb(char = mot, select = "P", 
                            mode = mode, sensitive = sensitive, 
                            perl = perl, fixed = fixed, agrep = agrep, 
                            max.distance = max.distance, costs = costs, 
                            crandb = crandb), 
              "Title" = s_crandb(char = mot, select = "T", 
                            mode = mode, sensitive = sensitive, 
                            perl = perl, fixed = fixed, agrep = agrep, 
                            max.distance = max.distance, costs = costs, 
                            crandb = crandb),
        "Description" = s_crandb(char = mot, select = "D", 
                            mode = mode, sensitive = sensitive, 
                            perl = perl, fixed = fixed, agrep = agrep, 
                            max.distance = max.distance, costs = costs, 
                            crandb = crandb))
    }
    lst <- if (length(words) == 1L) {
                funmot(words, mode, sensitive, perl, fixed, agrep, max.distance, 
                       costs, crandb)
            } else {
                sapply(words, funmot, mode, sensitive, perl, fixed, agrep, max.distance, 
                       costs, crandb, simplify = FALSE)
            }
return(lst)   
}

#' @export
#' @rdname s_crandb
s_crandb_AM <- function(..., char = NULL, mode = "or", sensitive = FALSE, 
                        perl = FALSE, fixed = FALSE, 
                        agrep = FALSE, max.distance = 0.1, costs = NULL,
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    mode  <- match.arg(mode, choices = c("or", "and", "relax"))
    words <- if (is.null(char)) cnscinfun() else char
    if (is.list(words)) stop("... cannot be a list.")
    funmot <- function(mot, mode, sensitive, perl, fixed, agrep, max.distance, costs, crandb) {
       list("Author" = s_crandb(char = mot, select = "A", 
                            mode = mode, sensitive = sensitive, 
                            perl = perl, fixed = fixed, agrep = agrep, 
                            max.distance = max.distance, costs = costs, 
                            crandb = crandb), 
        "Maintainer" = s_crandb(char = mot, select = "M", 
                            mode = mode, sensitive = sensitive, 
                            perl = perl, fixed = fixed, agrep = agrep, 
                            max.distance = max.distance, costs = costs, 
                            crandb = crandb)) 
    }
    lst <- if (length(words) == 1L) {
                funmot(words, mode, sensitive, perl, fixed, agrep, max.distance, 
                       costs, crandb)
            } else {
                sapply(words, funmot, mode, sensitive, perl, fixed, agrep, max.distance, 
                       costs, crandb, simplify = FALSE)
            }
return(lst)   
}



