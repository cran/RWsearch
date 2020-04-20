## tvdb
## @include s_tvdb.R


#' @title Task Views (tvdb.rda)
#' @description
#' \code{tvdb_down} downloads from CRAN the file "Views.rds", a file refreshed every day that 
#' describes the task views available in CRAN for this day, rearranges the list in an 
#' alphabetical order and gives names to the list names, then loads in .GlobalEnv 
#' this list (of class ctvlist) under the name \code{tvdb} and saves it with the filename 
#' \code{tvdb.rda}.
#' 
#' \code{tvdb_load} loads the file \code{filename} in .GlobalEnv under the name \code{tvdb}. 
#' Equivalent to \code{load("tvdb.rda")}.
#' 
#' \code{tvdb_vec} displays the list of the task views. There are 36 task views in August 2018.
#' 
#' \code{tvdb_dfr} extracts from \code{tvdb} a data.frame \emph{version, name, topic} 
#' of the task views. 
#' 
#' \code{tvdb_list} extracts from \code{tvdb} the list of the task views and the referenced 
#' packages. 
#' 
#' \code{tvdb_pkgs} displays the packages referenced by one or several task views. 
#' 
#' Visit \code{\link{s_crandb_tvdb}} to conduct task view maintenance. 
#' 
#' @param   dir        character. The directory where "tvdb.rda" is saved. 
#'                     Default value \code{"."} is the current directory.
#' @param   repos      character. The address of your local CRAN.
#' @param   filename   character. The path to file "tvdb.rda". 
#'                     The default is to read in the current directory.
#' @param   tvdb       list. The list of the task views. 
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     The names of one or several task views. 
#' @param   char       (name to) a character vector or a list. Use this argument if 
#'                     \code{...} fails or if you call the function from another function. 
#'                     If used, argument \code{...} is ignored. 
#' @examples
#' ### DOWNLOAD AND VISUALIZE THE TASK VIEWS (tvdb)
#' ## In real life, download tvdb from CRAN or load it from your directory 
#' ## with functions tvdb_down() or tvdb_load(). 
#' ## In this example, we use a small file.
#' tvdb_load(system.file("data", "ztvdb.rda", package = "RWsearch")) 
#' length(tvdb)
#' 
#' ## List the task views
#' tvdb_vec()
#' tvdb_dfr()
#' tvdb_pkgs("Genetics")
#' lengths(tvdb_list())
#' 
#' ## Here, 'lst' is subsetted from the small crandb file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' '%in2%' <- function (x, y) x[match(x, y, nomatch = 0)  > 0]
#' lst     <- lapply(tvdb_list()[1:3], '%in2%', crandb$Package) ; lst
#' \donttest{
#' p_display7(lst, dir = file.path(tempdir(), "pdisp"), verbose = TRUE)
#' }
#' @name tvdb
NULL

#' @export
#' @rdname tvdb
tvdb_down <- function(dir = ".", repos = getOption("repos")[1]) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    urldb  <- paste0(repos, "/src/contrib/Views.rds")
    con    <- gzcon(url(urldb, open = "rb"))
    on.exit(close(con))
    tvdb <- readRDS(con)
    names(tvdb) <- ntv <- sapply(tvdb, function(x) x$name)
    tvdb <- tvdb[sort(ntv)]
    save(tvdb, file = file.path(dir, "tvdb.rda"))
    tvdb <<- tvdb
    txt1 <- "saved."
    txt2 <- "tvdb loaded."
    txt3 <- length(tvdb)
    txt4 <- "task views listed between"
    txt5 <- min(as.Date(sapply(tvdb, function(x) x$version)))
    txt6 <- "and"
    txt7 <- max(as.Date(sapply(tvdb, function(x) x$version)))
    message(paste("tvdb.rda", txt1, txt2, txt3, txt4, txt5, txt6, txt7))
}

#' @export
#' @rdname tvdb
tvdb_load <- function(filename = "tvdb.rda") {
    if (file.exists(filename)) {
        glf  <- get(load(filename, envir = .GlobalEnv))
        txt2 <- "tvdb loaded."
        txt3 <- length(glf)
        txt4 <- "task views listed between"
        txt5 <- min(as.Date(sapply(glf, function(x) x$version)))
        txt6 <- "and"
        txt7 <- max(as.Date(sapply(glf, function(x) x$version)))
        message(paste(txt2, txt3, txt4, txt5, txt6, txt7)) 
    } else {
        stop(paste("File", filename, "does not exist in this directory."))
    }
}

#' @export
#' @rdname tvdb
tvdb_vec <- function(tvdb = get("tvdb", envir = .GlobalEnv)) {
    if (!is.list(tvdb)) stop("tvdb is not loaded.")
return(names(tvdb)) 
}

#' @export
#' @rdname tvdb
tvdb_dfr <- function(tvdb = get("tvdb", envir = .GlobalEnv)) {
    if (!is.list(tvdb)) stop("tvdb is not loaded.")
    dfr <- data.frame(
        version = as.Date(sapply(tvdb, function(x) x$version)),
        npkgs   = sapply(tvdb, function(x) length(x$packagelist$name)),
        name    = sapply(tvdb, function(x) x$name),
        topic   = sapply(tvdb, function(x) x$topic),
        row.names = NULL, 
        stringsAsFactors = FALSE)
print(dfr, right = FALSE)
}

#' @export
#' @rdname tvdb
tvdb_list <- function(tvdb = get("tvdb", envir = .GlobalEnv)) {
    if (!is.list(tvdb)) stop("tvdb is not loaded.")
    lst <- lapply(tvdb, function(x) x$packagelist$name)
    names(lst) <- sapply(tvdb, function(x) x$name)
return(lst)
}

#' @export
#' @rdname tvdb
tvdb_pkgs <- function(..., char = NULL, tvdb = get("tvdb", envir = .GlobalEnv)) {
    if (!is.list(tvdb)) stop("tvdb is not loaded.")
    lst  <- tvdb_list(tvdb)
    tv   <- if (is.null(char)) cnscinfun() else char
    if (is.list(tv)) stop("... cannot be a list.")
    pkgs <- if (length(tv) == 1L) {
                lst[[tv]] 
            } else {
                sapply(tv, function(x, lst) lst[[x]], lst)
            }
return(pkgs)
}



