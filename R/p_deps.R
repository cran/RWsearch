## p_deps
## @include n_graph.R


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
#' \code{p_network} returns the package dependencies as a network of nodes and links. 
#' It is called by \code{\link{n_graphF}} and \code{\link{n_graphS}}.
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
#' @param   verbose   logical. Returns additional information about the search.
#' @param   crandb    data.frame \code{crandb}. Also accepted is \code{NULL} which will 
#'                    search in the local \code{installed.packages()}. This later form
#'                    allows (private) packages that are not listed in \code{crandb}.
#' @param   exclpkgs  logical or character vector. \code{TRUE} excludes 
#'                    from the network of nodes and links the dependencies
#'                    \code{c("graphics", "grDevices", "methods", "stats", "tools", "utils")}.
#'                    \code{FALSE} includes them. 
#'                    You can provide your own vector of packages to exclude them 
#'                    from the network of nodes and links. 
#' @examples 
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' p_deps(canprot, FatTailsR) 
#' p_deps(canprot, FatTailsR, recursive = TRUE)
#' p_deps(canprot, FatTailsR, recursive = TRUE, which = "DIL")
#' p_deps(actuar, reverse = TRUE, which = "DILSN")
#' 
#' p_network(canprot, exclpkgs = FALSE)
#' 
#' p_deps_inpkgs(RWsearch, canprot)
#' p_deps_unpkgs(RWsearch, canprot)
#' p_deps_inun(RWsearch, canprot, donotexist)
#' 
#' @export
#' @name p_deps
p_deps <- function (..., char = NULL, which = "DIL", recursive = FALSE, reverse = FALSE, 
        verbose = getOption("verbose"), crandb = get("crandb", envir = .GlobalEnv)) {
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
           recursive = recursive, reverse = reverse, verbose = verbose)   
return(lst[pkgs])
}

#' @export
#' @rdname p_deps
p_depsrev <- function (..., char = NULL, which = "DIL", recursive = FALSE, reverse = FALSE, 
        verbose = getOption("verbose"), crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs  <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
    p_deps(char = pkgs, which = which, recursive = recursive, reverse = TRUE, 
           verbose = verbose, crandb = crandb) 
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
p_network <- function (..., char = NULL, which = "DIL", recursive = TRUE, reverse = FALSE,
                       exclpkgs = TRUE, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
    
    ### MANAGING EXCLUSIONS
    if (is.logical(exclpkgs)) {
        exclpkgs <- if (exclpkgs) {
                     c("graphics", "grDevices", "methods", "stats", "tools", "utils")
                   } else { "" }
    }
    pkgs    <- pkgs[!(pkgs %in% exclpkgs)]
    dfrDeps <- data.frame(Source = pkgs, Target = pkgs, stringsAsFactors = FALSE)
    
    ### DEPENDENCIES
    mpkgs <- unique(unlist(
             c(pkgs, p_deps(char = pkgs, which = which, recursive = recursive,
                            reverse = reverse, verbose = FALSE, crandb = crandb))))
    lst   <- p_deps(char = mpkgs, which = which, recursive = FALSE,
                    reverse = reverse, verbose = FALSE, crandb = crandb)
    lst2  <- lst[lengths(lst) != 0]
    if (length(lst2) != 0) {
        lst3 <- list()
        if (reverse) {
            for (i in seq_along(lst2)) lst3[[i]] <- data.frame(
                 Source = names(lst2)[i], Target = lst2[[i]], stringsAsFactors = FALSE)
        } else {
            for (i in seq_along(lst2)) lst3[[i]] <- data.frame(
                 Source = lst2[[i]], Target = names(lst2)[i], stringsAsFactors = FALSE)
        }
        dfr3    <- do.call("rbind", lst3)
        dfrDeps <- rbind(dfrDeps, dfr3)
        
    ### INTERMEDIATE DEPENDENCIES v4.5.9
        dfrDeps <- dfrDeps[!(dfrDeps$Source %in% exclpkgs),, drop=FALSE]
        dfrDeps <- dfrDeps[!(dfrDeps$Target %in% exclpkgs),, drop=FALSE]
        if (reverse) {
            while (!all(dfrDeps$Source %in% dfrDeps$Target)) {
                dfrDeps  <- dfrDeps[dfrDeps$Source %in% dfrDeps$Target,, drop=FALSE]
            }        
        } else {
            while (!all(dfrDeps$Target %in% dfrDeps$Source)) {
                dfrDeps  <- dfrDeps[dfrDeps$Target %in% dfrDeps$Source,, drop=FALSE]
            }
        }
    }
    
    ### NODES
    dfr4     <- if (reverse) { dfrDeps[!duplicated(dfrDeps[,"Target"]),] 
                      } else { dfrDeps[!duplicated(dfrDeps[,"Source"]),] }
    colnames(dfr4) <- if (reverse) { c("NodeGroup", "NodeName") 
                            } else { c("NodeName", "NodeGroup") }
    rownames(dfr4) <- NULL
    dfr4     <- transform(dfr4, NID = as.integer(seq_len(nrow(dfr4))-1))
    NSize    <-  1 + 49*(dfr4$NodeName %in% pkgs)  
    NGroup1  <- -1 + sapply(sapply(dfr4$NodeGroup, grep, dfr4$NodeName, 
                                   fixed = TRUE), function(x) x[1])
    NGroup2  <- sapply(NGroup1, function(x, y) ifelse(x >= y, y, x), y = length(pkgs))
    NGroup3  <- sapply(pmatch(dfr4$NodeName, dfr4$NodeGroup, nomatch = 0), 
                       min, length(pkgs)+1) 
    NGroup3  <- sapply(NGroup3-1, function(x, y) ifelse(x < 0, y, x), y = max(NGroup3))
    NCol1    <- "#999999"
    # names(NGroup1) <- names(NGroup2) <- names(NGroup3) <- names(NCol1) <- NULL
    dfrNodes <- data.frame(dfr4, NSize, NGroup1, NGroup2, NGroup3, 
                           NCol1, stringsAsFactors = FALSE)
    rownames(dfrNodes) <- NULL

    ### LINKS
    LSource  <- -1 + sapply(sapply(dfrDeps$Source, grep, dfrNodes$NodeName, 
                                   fixed = TRUE), function(x) x[1])
    LTarget  <- -1 + sapply(sapply(dfrDeps$Target, grep, dfrNodes$NodeName, 
                                   fixed = TRUE), function(x) x[1])
    LValue   <-  1 + if (reverse) { 0*(LSource < length(pkgs)) 
                           } else { 4*(LTarget < length(pkgs)) }
    LCol1    <- "#555555"
    SValue   <-  1
    dfrLinks <- data.frame(dfrDeps, LSource, LTarget, LValue, LCol1, 
                           SValue, stringsAsFactors = FALSE)
    rownames(dfrLinks) <- NULL

    ### LIST
    netw <- list(pkgs0 = pkgs, recursive = recursive, reverse = reverse, 
                 dfrNodes = dfrNodes, dfrLinks = dfrLinks)
    class(netw) <- c("pkgsnetwork", "list")
return(netw)
}

#' @export
#' @rdname p_deps
p_deps_inpkgs <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
						reverse = FALSE, verbose = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
	deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
					reverse = reverse, verbose = verbose, crandb = crandb)
	deps2  <- sort(unique(unlist(deps)))
	lst    <- p_inun(char = deps2)
	inpkgs <- if (length(lst[[1]]) == 0) NA else sort(unique(lst[[1]]))
return(inpkgs)
}

#' @export
#' @rdname p_deps
p_deps_unpkgs <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
						reverse = FALSE, verbose = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
	deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
					reverse = reverse, verbose = verbose, crandb = crandb)
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
						reverse = FALSE, verbose = FALSE, 
						crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs   <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
	deps   <- p_deps(char = pkgs, which = which, recursive = recursive, 
					 reverse = reverse, verbose = verbose, crandb = crandb)
	deps2  <- sort(unique(unlist(deps)))
	inunpkgs <- p_inun_crandb(char = deps2)
return(inunpkgs)
}



