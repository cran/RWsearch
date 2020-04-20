## p_deps
## @include p_check.R


#' @title Dependencies and Reverse Dependencies of Packages
#' @description
#' \code{p_deps} returns the (reverse) dependencies of a (vector of) package(s). 
#' It is a wrapper of the \code{tools::package_dependencies} function. A warning 
#' is issued for packages that are not in \code{crandb + .libPaths()} 
#' (for instance in CRAN archive, Bioconductor, Github or your own directories).
#' 
#' \code{p_depsrev} is a shortcut to \code{p_deps(reverse = TRUE)}. It returns 
#' the reverse dependencies (e.g. the children packages).
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
#' @param   crandb    data.frame \code{crandb}. Also accepted is \code{NULL} which will 
#'                    search in the local \code{installed.packages()}. This later form
#'                    allows (private) packages that are not listed in \code{crandb}.
#' @examples 
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' p_deps_count(actuar, networkD3, FatTailsR, RWsearch, NotApkg)
#' 
#' p_deps(networkD3, visNetwork) 
#' p_deps(networkD3, visNetwork, recursive = TRUE)
#' p_deps(actuar, fitdistrplus, reverse = TRUE, which = "DILSN")
#' 
#' p_deps_inpkgs(RWsearch, canprot)
#' p_deps_unpkgs(RWsearch, canprot)
#' p_deps_inun(RWsearch, canprot, NotApkg)
#' 
#' @export
#' @name p_deps
p_deps <- function (..., char = NULL, which = "DIL", recursive = FALSE, reverse = FALSE, 
					crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
    crandbpkgs <- unique(c(list.files(.libPaths()), crandb[, "Package"]))
    res <- is.element(pkgs, crandbpkgs)
    if (!all(res)) {
        if (sum(!res) == 1) {
            warning(paste("Package", pkgs[!res], "is not in crandb + .libPaths()."))
        }
        else {
            txt <- paste(pkgs[!res], collapse = ", ")
            warning(paste("Packages", txt, "are not in crandb + .libPaths()."))
        }
    }
    columns <- fccdeps(which)
    lst <- tools::package_dependencies(pkgs, db = crandb, which = columns,
           recursive = recursive, reverse = reverse, verbose = FALSE)   
return(lst[pkgs])
}

#' @export
#' @rdname p_deps
p_depsrev <- function(..., char = NULL, which = "DIL", recursive = FALSE, 
					  crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
    p_deps(char = pkgs, which = which, recursive = recursive, reverse = TRUE, 
           crandb = crandb) 
}

#' @export
#' @rdname p_deps
p_deps_count <- function(..., char = NULL, which = "DIL", 
					     crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
	data.frame(
	   "Parents1"  = lengths(p_deps(char = pkgs, which = which, 
							 recursive = FALSE, crandb = crandb)),
	   "ParentsN"  = lengths(p_deps(char = pkgs, which = which, 
							 recursive = TRUE , crandb = crandb)),
	   "Children1" = lengths(p_deps(char = pkgs, which = which, 
							 recursive = FALSE, reverse = TRUE, crandb = crandb)),
	   "ChildrenN" = lengths(p_deps(char = pkgs, which = which, 
							 recursive = TRUE,  reverse = TRUE, crandb = crandb))
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
    if (is.list(pkgs)) stop("... cannot be a list.")
	deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
					reverse = reverse, crandb = crandb)
	deps2  <- sort(unique(unlist(deps)))
	lst    <- p_inun(char = deps2)
	inpkgs <- if (length(lst[[1]]) == 0) NA else sort(unique(lst[[1]]))
return(inpkgs)
}

#' @export
#' @rdname p_deps
p_deps_unpkgs <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
						reverse = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
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
return(unpkgs)
}

#' @export
#' @rdname p_deps
p_deps_inun <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
						reverse = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
	deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
					 reverse = reverse, crandb = crandb)
	deps2  <- sort(unique(unlist(deps)))
	inunpkgs <- p_inun_crandb(char = deps2)
return(inunpkgs)
}



