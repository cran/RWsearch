## p_deps
## @include p_check.R


#' @title Dependencies and Reverse Dependencies of Packages
#' @description
#' \code{p_deps} returns the (reverse) dependencies of a (vector of) package(s). 
#' It is a wrapper of the \code{tools::package_dependencies} function. A warning 
#' is issued for packages that are not in \code{crandb + .libPaths()} 
#' (for instance in CRAN archive, Bioconductor, Github or your own directories).
#' 
#' \code{p_depsrec} is a shortcut to \code{p_deps(recursive = TRUE)}. It returns 
#' the recursive dependencies (e.g. the list of all ancestors).
#' 
#' \code{p_depsrev} is a shortcut to \code{p_deps(reverse = TRUE)}. It returns 
#' the reverse dependencies (e.g. the children packages).
#' 
#' \code{p_deps_deps} returns a list with the recursive dependencies for the packages
#'  packages and every first level dependencies (including or excluding the ones 
#' in \code{.Library}.
#' 
#' \code{p_deps_ndeps} returns a vector of the number of dependencies for each 
#' package and their parent dependencies. With the argument \code{sort = TRUE}, 
#' the packages are listed from no dependency to the largest number of dependencies. 
#' 
#' \code{p_deps_count} counts the number of (recursive/reverse) dependencies for 
#' each package and returns a data.frame with 4 columns: 
#' \code{Parents1}, \code{ParentsN}, \code{Children1}, \code{ChildrenN}.
#' 
#' \code{p_deps_inpkgs} returns the package dependencies that are installed 
#' in the computer. 
#' 
#' \code{p_deps_unpkgs} returns the package dependencies that are not installed 
#' in the computer.
#' 
#' \code{p_deps_inun} combines \code{p_deps} and \code{p_inun_crandb}, then returns 
#' the status of all dependencies: installed or not installed in the computer, 
#' available or not available in the current crandb (see CRAN archives, Bioconductor,
#' Github, your own packages).
#' 
#' The missing packages available on CRAN can be downloaded with \code{\link{p_down0}}, 
#' downloaded and checked (by R CMD check) with \code{xfun::rev_check} or installed 
#' with \code{install.packages}. The packages removed from CRAN but available in
#' CRAN archive can be downloaded with \code{\link{p_downarch}}. 
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, excluding list.
#'                    A package or a vector of packages listed in \code{crandb} or in
#'                    \code{installed.packages()}.
#' @param   char      (name to) a character vector. Use this argument if 
#'                    \code{...} fails or if you call the function from another function. 
#'                    If used, argument \code{...} is ignored.  
#' @param   which     character vector. A sub-vector of 
#'                    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}. 
#'                    The short forms "D", "I", "L", "S", "N", "DL", "DI", "DIL", "DILS", 
#'                    "DILN", "DILSN", "SN" are accepted. "N" is for "Enhances" as the 
#'                    single letter "E" is used by R as a shortcut to EXPR, a reserved word.
#' @param   recursive logical. Search for (reverse) dependencies of (reverse) dependencies.
#' @param   reverse   logical. Search for reverse dependencies.
#' @param   verbose   logical. Print the message.
#' @param   crandb    data.frame \code{crandb}. Also accepted is \code{NULL} which will 
#'                    search in the local \code{installed.packages()}. This later form
#'                    allows (private) packages that are not listed in \code{crandb}.
#' @param   Library   logical. The default \code{FALSE} excludes the base and recommended 
#'                    packages stored in \code{.Library}. \code{TRUE} includes them.
#' @param   sort      logical. The default \code{TRUE} sorts the package by the number
#'                    of dependencies.
#' @examples 
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' p_deps(networkD3, visNetwork) 
#' p_deps(networkD3, visNetwork, recursive = TRUE)
#' p_deps(actuar, fitdistrplus, reverse = TRUE, which = "DILSN")
#' 
#' p_deps_inpkgs(RWsearch, canprot)
#' p_deps_unpkgs(RWsearch, canprot)
#' p_deps_inun(RWsearch, canprot, NotApkg)
#' 
#' p_deps_deps(actuar, networkD3, FatTailsR, RWsearch, NotApkg)
#' p_deps_ndeps(actuar, networkD3, FatTailsR, RWsearch, NotApkg)
#' p_deps_count(actuar, networkD3, FatTailsR, RWsearch, NotApkg)
#' 
#' @export
#' @name p_deps
p_deps <- function (..., char = NULL, which = "DIL", recursive = FALSE, 
                    reverse = FALSE, verbose = TRUE, 
                    crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    libpkgs    <- unlist(lapply(.libPaths(), list.files))
    crandbpkgs <- unique(c(libpkgs, crandb[, "Package"]))
    res <- is.element(pkgs, crandbpkgs)
    if (!all(res) && verbose) {
        if (sum(!res) == 1) {
            message(paste("Package", pkgs[!res], "is not in crandb + .libPaths()."))
        }
        else {
            txt <- paste(pkgs[!res], collapse = ", ")
            message(paste("Packages", txt, "are not in crandb + .libPaths()."))
        }
    }
    columns <- fccdeps(which)
    lst <- tools::package_dependencies(pkgs, db = crandb, which = columns,
           recursive = recursive, reverse = reverse, verbose = FALSE)   
    lst[pkgs]
}

#' @export
#' @rdname p_deps
p_depsrec <- function(..., char = NULL, which = "DIL", reverse = FALSE, 
                      verbose = TRUE, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (is.list(pkgs)) stop("... cannot be a list.")
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    p_deps(char = pkgs, which = which, recursive = TRUE, reverse = reverse, 
           verbose = verbose, crandb = crandb) 
}

#' @export
#' @rdname p_deps
p_depsrev <- function(..., char = NULL, which = "DIL", recursive = FALSE, 
                      verbose = TRUE, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (is.list(pkgs)) stop("... cannot be a list.")
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    p_deps(char = pkgs, which = which, recursive = recursive, reverse = TRUE, 
           verbose = verbose, crandb = crandb) 
}

#' @export
#' @rdname p_deps
p_deps_deps <- function(..., char = NULL, which = "DIL", Library = FALSE, 
                        verbose = TRUE, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... cannot be a list.")
    pkgs  <- unique(pkgs)
    deps  <- p_deps(char = pkgs, which = which, recursive = TRUE, 
                    verbose = verbose, crandb = crandb)
    deps2 <- unique(unlist(deps))
    deps3 <- if (Library) deps2 else deps2[!(deps2 %in% list.files(.Library))]
    deps3 <- unique(c(pkgs, deps3))
    deps4 <- p_deps(char = deps3, which = which, recursive = TRUE, 
                    verbose = verbose, crandb = crandb)
    deps5 <- lapply(deps4, function(pkg2) pkg2[!(pkg2 %in% list.files(.Library))])
    deps5
}

#' @export
#' @rdname p_deps
p_deps_ndeps <- function(..., char = NULL, which = "DIL", Library = FALSE, 
                         sort = TRUE, verbose = TRUE, 
                         crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... cannot be a list.")
    pkgs  <- unique(pkgs)
    deps  <- p_deps(char = pkgs, which = which, recursive = TRUE, 
                    verbose = verbose, crandb = crandb)
    deps2 <- unique(unlist(deps))
    deps3 <- if (Library) deps2 else deps2[!(deps2 %in% list.files(.Library))]
    deps3 <- unique(c(pkgs, deps3))
    deps4 <- p_deps(char = deps3, which = which, recursive = TRUE, 
                    verbose = verbose, crandb = crandb)
    deps5 <- lapply(deps4, function(pkg2) pkg2[!(pkg2 %in% list.files(.Library))])
    ndeps <- if (sort) sort(lengths(deps5)) else lengths(deps5)
    ndeps
}

#' @export
#' @rdname p_deps
p_deps_count <- function(..., char = NULL, which = "DIL", verbose = TRUE, 
                         crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... cannot be a list.")
    if (any(duplicated(pkgs))) message("Duplicated pkgs were ignored.")
    pkgs <- unique(pkgs)
    data.frame(
       "Parents1"  = lengths(p_deps(char = pkgs, which = which, 
                             recursive = FALSE, 
                             verbose = verbose, crandb = crandb)),
       "ParentsN"  = lengths(p_deps(char = pkgs, which = which, 
                             recursive = TRUE , 
                             verbose = verbose, crandb = crandb)),
       "Children1" = lengths(p_deps(char = pkgs, which = which, 
                             recursive = FALSE, reverse = TRUE, 
                             verbose = verbose, crandb = crandb)),
       "ChildrenN" = lengths(p_deps(char = pkgs, which = which, 
                             recursive = TRUE,  reverse = TRUE, 
                             verbose = verbose, crandb = crandb)),
       row.names = pkgs
    )
}

fccdeps <- function(columns) {
    funcolumn <- function(column) {  
        switch(column,
            "D"   = "Depends",
            "I"   = "Imports",
            "L"   = "LinkingTo",
            "S"   = "Suggests",
            "N"   = "Enhances",
            "DL"     = c("Depends", "LinkingTo"),
            "DI"     = c("Depends", "Imports"),
            "DIL"    = c("Depends", "Imports", "LinkingTo"),
            "DILS"   = c("Depends", "Imports", "LinkingTo", "Suggests"),
            "DILN"   = c("Depends", "Imports", "LinkingTo", "Enhances"),
            "DILSN"  = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
            "SN"     = c("Suggests", "Enhances"),
            column 
        )
    }
    args    <- unlist(lapply(columns, funcolumn))
    choices <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    match.arg(args, choices, several.ok = TRUE)
} 

#' @export
#' @rdname p_deps
p_deps_inpkgs <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
                        reverse = FALSE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... cannot be a list.")
    deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
                    reverse = reverse, crandb = crandb)
    deps2  <- sort(unique(unlist(deps)))
    lst    <- p_inun(char = deps2)
    inpkgs <- if (length(lst[[1]]) == 0) NA else sort(unique(lst[[1]]))
    inpkgs
}

#' @export
#' @rdname p_deps
p_deps_unpkgs <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
                        reverse = FALSE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... cannot be a list.")
    deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
                    reverse = reverse, crandb = crandb)
    deps2  <- sort(unique(unlist(deps)))
    lst    <- p_inun(char = deps2)
    if (length(lst[[2]]) == 0) {
        unpkgs <- NA 
        message("All dependencies are installed in this computer.")
    } else { 
        unpkgs <- sort(unique(lst[[2]]))
        nopkgs <- unpkgs[!(unpkgs %in% crandb$Package)]
        nopkg2 <- paste(nopkgs, collapse = ", ")
        if (length(nopkgs) == 1) message(paste("Package",  nopkg2, "is not in crandb."))
        if (length(nopkgs)  > 1) message(paste("Packages", nopkg2, "are not in crandb."))
        if (length(nopkgs) >= 1) message("Check CRAN archives, BioConductor, Github, your packages.")
    }
    unpkgs
}

#' @export
#' @rdname p_deps
p_deps_inun <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
                        reverse = FALSE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... cannot be a list.")
    deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
                     reverse = reverse, crandb = crandb)
    deps2  <- sort(unique(unlist(deps)))
    inunpkgs <- p_inun_crandb(char = deps2)
    inunpkgs
}



