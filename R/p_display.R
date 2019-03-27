## p_display + .htmltable
## @include h_ttp.R


#' @title Display Package Information in HTML Pages
#' @description
#' \code{p_display}, \code{p_display5} and \code{p_display7} open the default browser and
#' display the results of \code{p_table}, \code{p_table5} and \code{p_table7} in one or 
#' several html pages. If \code{...} (or \code{char}) is a list, several pages are opened. 
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, including list.
#'                    A vector or a list of packages. Or a vector or a list of data.frame 
#'                    produced by \code{p_table}.
#' @param   char      (name to) a character vector. Use this argument if \code{...} fails 
#'                    or if you call the function from another function. If used, 
#'                    argument \code{...} is ignored. 
#' @param   columns   character vector. A sub-vector of \code{colnames(crandb)}. The 
#'                    short form "P", "T", "D", "PT", "PD", "TD", "PTD", "A", "M", "AM" 
#'                    describing the Package name, Title, Description, Author, Maintainer
#'                    or a combination of them is accepted.
#' @param   dir       character. The directory in which the html file(s) is (are) saved.
#'                    \code{tempdir()} or \code{getwd()} are common paths.
#' @param   verbose   logical. List the generated html file(s).
#' @param   crandb    data.frame \code{crandb}. The data.frame of CRAN packages.
#' @examples
#' \donttest{
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' ## Vector => 1 page
#' p_display(pacman, pdfsearch, sos, dir = tempdir())
#' 
#' ## List with 3 items => 3 pages 
#' ## No package has the 'distillation' keyword. An empty table is returned.
#' (lst <- s_crandb_list("thermodynamic", "chemical reaction", "distillation"))
#' p_display5(lst, dir = tempdir())
#' }
#' @export
#' @name p_display
NULL

#' @export
#' @rdname p_display
p_display <- function(..., char = NULL, columns = c("Package", "Title", "Description"), 
                      dir = tempdir(), verbose = FALSE, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    columns <- fcccrandb(columns, crandb)
    pkgs    <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs) & !is.data.frame(pkgs)) {
        if (is.null(names(pkgs))) stop ("pkgs is a list with no names.")
        names(pkgs) <- gsub(".", "", make.names(names(pkgs)), fixed = TRUE)
        for (nom in names(pkgs)) p_display(char = pkgs[[nom]], columns = columns, 
                                   dir = dir, verbose = verbose, crandb = crandb)
    } else {
        if (is.null(dim(pkgs))) {
            .htmltable(p_table(char = pkgs, columns = columns, crandb = crandb),
                       dir = dir, verbose = verbose)
        } else {
            .htmltable(pkgs, dir = dir, verbose = verbose)
        }
    }
}

#' @export
#' @rdname p_display
p_display5 <- function(..., char = NULL, dir = tempdir(), verbose = FALSE, 
                       crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    p_display(char = pkgs, columns = c("Package", "Title", "Description", "Author", 
              "Maintainer"), 
              dir = dir, verbose = verbose, crandb = crandb)
}

#' @export
#' @rdname p_display
p_display7 <- function(..., char = NULL, dir = tempdir(), verbose = FALSE, 
                       crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    p_display(char = pkgs, columns = c("Package", "Version", "Published", "Title", 
              "Description", "Author", "Maintainer"), 
              dir = dir, verbose = verbose, crandb = crandb)
}




## #' @title Display an HTML Table With Brew
## #' @description
## #' \code{htmltable} uses the \emph{brew} package to print an html table. 
## #'  
## #' @param   df         a data.frame. Passed to the template
## #' @param   dir        character. The directory in which \code{filename} is written.
## #' @param   filename   character. The name of the generated html file.
## #' @param   page.title character. Passed to the template.
## #' 
## #' @export
## #' @name htmltable
.htmltable <- function(df, dir = tempdir(), filename = "Packages.html", 
                       page.title = "Packages", verbose = FALSE) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    assets <- list.files(system.file("assets", package = "RWsearch"))
    for (asset in assets)  {
      file.copy(file.path(system.file("assets", package = "RWsearch"), asset),
                file.path(dir, asset), overwrite = TRUE)
    }  
    if (file.exists(file.path(dir, filename))) { 
        num <- gsub(".", "", format(Sys.time(), format = "%OS4"), fixed = TRUE)
        filename <- paste0("Packages", num , ".html")
    }
    brew::brew(file = system.file("templates", "template.brew", package = "RWsearch"),
               output = file.path(dir, filename))
    utils::browseURL(file.path(dir, filename))
    filename2 <- normalizePath(file.path(dir, filename), winslash = "/", mustWork = FALSE)
if (verbose) filename2 else invisible(filename2)
}



