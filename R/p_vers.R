## p_vers
## @include p_unload_all.R


#' @title Package Version and Number of Dependencies
#' @description
#' The information displayed by \code{p_vers} depends on the availability of
#' \code{crandb} and \code{binarydb} in \code{.GlobalEnv}.
#'
#' If \code{crandb} is not loaded in \code{.GlobalEnv}, \code{p_vers} returns a
#' data.frame with two columns: first column \code{nsloaded} (TRUE/FALSE) detects
#' (with \code{base::isNamespaceLoaded}) if the package namespaces are loaded.
#' Second column \code{version} is the version number of the installed packages.
#'
#' If \code{crandb} is loaded in \code{.GlobalEnv}, three columns are added.
#' Column \code{crandb} displays the version number of the source packages recorded
#' in the \code{crandb} file. Column \code{gcc} displays the nedd for a compilation.
#' Column \code{compare} compares this version number with the version installed
#' on the computer. Possible values are:
#' -2 for a package not installed on the computer (NA) but available in crandb ;
#' -1 for an installed package older than the source package available in CRAN ;
#'  0 for an installed package with the same version number than CRAN ;
#' +1 for a more recent package than the one available in CRAN ;
#' +2 for a package installed on the computer and not available in CRAN (NA) ;
#' +3 for a package not installed on the computer (NA) and not available in CRAN (NA).
#'
#' If \code{binarydb} is loaded in \code{.GlobalEnv}, two or three columns are added.
#' Column \code{binary} displays the version number of the binary packages recorded
#' in the \code{binarydb = available.packages, type = "binary")} matrix.
#' Column \code{difvb} compares the installed version on the computer with this
#' binary version and column \code{difbc} compares (if crandb is in \code{.GlobalEnv})
#' the binary version with the source package available in CRAN (which can differ
#' for recently updated packages, a matter of 1 to 3 days).
#' The numbering is identical to the one used for \code{crandb}.
#'
#' If \code{ndeps = TRUE}, two more columns are added with the number
#' of recursive dependencies per package. Column \code{tdeps} includes the base
#' and recommended packages. Column \code{ndeps} does not count them. This option
#' can take some time. Set it to \code{FALSE} if you need speed.
#'
#' \code{p_vers_deps} calculates the same information but includes the recursive
#' dependencies. Subsetting by \code{"compare < 0"} returns a shorter data.frame
#' with the uninstalled (-2) and the outdated (-1) packages. Packages marked with
#' \code{nsloaded = TRUE} must be detached and unloaded before any reinstallation.
#' Using this instruction before running \code{install.packages} or \code{p_inst}
#' is very useful as it detects packages that are locked and cannot be reinstalled.
#' The order provided by \code{p_vers_deps} is the best one for the reinstallation
#' of outdated packages.
#'
#' @param   ...       any format recognized by \code{\link{cnsc}}.
#'                    A (list of) vector of packages for \code{p_vers}.
#'                    A vector of packages for \code{p_vers_deps}
#' @param   char      (name to) a character vector. Use this argument if \code{...}
#'                    fails or if you call the function from another function.
#'                    If used, argument \code{...} is ignored.
#' @param   ndeps     logical. Calculate the number of recursive dependencies.
#'                    \code{crandb} in \code{.GlobalEnv} is required for this option.
#' @param   crandb    data.frame \code{crandb}. The data.frame of CRAN packages.
#' @param   subset    character. Subset the output data.frame on som columns.
#'                    The default \code{"compare < 4"} does not subset. Usual values
#'                    are \code{"compare < 0"} or \code{"compare < 0 && nsloaded == TRUE"}
#' @examples
#' ## In real life, download crandb from CRAN or load it from your directory
#' ## with functions crandb_down() or crandb_load().
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#'
#' ## macOS and Windows users can launch (no file stored in RWsearch)
#' # binarydb_down()
#'
#' pkgs <- cnsc(RWsearch, MASS, Matrix, NotAPkg, R)
#' p_vers(pkgs, ndeps = FALSE)
#' p_vers(p_deps(pkgs), ndeps = FALSE)
#'
#' p_vers_deps(pkgs) # dependencies can be visualized with p_graphF(pkgs)
#'
#' @export
#' @name p_vers
p_vers <- function(..., char = NULL, ndeps = TRUE) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        lst <- pkgs
        for (pkg in names(pkgs)) {
            lst[[pkg]] <- if (length(pkgs[[pkg]]) == 0) {
                data.frame()
            } else {
                p_vers(char = pkgs[[pkg]], ndeps = ndeps)
            }
        }
        lst
    } else {
        pkgs <- unique(pkgs)
        if (length(pkgs) == 0) {
            dfr <- data.frame()
        } else {
            funcomp <- function(a, b) {
                if (!is.na(a) && !is.na(b)) utils::compareVersion(a, b) else {
                    if (is.na(a) && !is.na(b)) -2 else {
                        if (!is.na(a) && is.na(b)) 2 else {
                            -3 }}}
            }
            funpkg <- function(pkg) {
                tryCatch({
                    if (pkg %in% c("r", "R")) as.character(getRversion())
                        else utils::packageDescription(pkg, fields = "Version")
                    },
                    condition = function(c) { NA_character_ }
                )
            }
            dfr <- data.frame(
                    "nsloaded" = sapply(pkgs, isNamespaceLoaded),
                    "version"  = sapply(pkgs, funpkg),
                    row.names  = pkgs,
                    stringsAsFactors = FALSE
            )
            if (exists("binarydb", envir = .GlobalEnv)) {
                binarydb  <- get("binarydb", envir = .GlobalEnv)
                funbinary <- function(pkg, binarydb) {
                    ver <- binarydb[binarydb[, "Package"] == pkg, "Version"]
                    if (length(ver) == 0) ver <- NA_character_
                    ver
                }
                bins <- sapply(rownames(dfr), funbinary, binarydb)
                dfr  <- data.frame(dfr, binary = bins, stringsAsFactors = FALSE)
            }
            if (exists("crandb", envir = .GlobalEnv)) {
                crandb  <- get("crandb", envir = .GlobalEnv)
                funcran <- function(pkg, crandb) {
                    vers <- crandb[crandb$Package == pkg, "Version"]
                    if (length(vers) == 0) vers <- NA_character_
                    vers
                }
                vers <- sapply(rownames(dfr), funcran, crandb)
                dfr  <- data.frame(dfr, crandb = vers, stringsAsFactors = FALSE)
            }
            if (exists("binarydb", envir = .GlobalEnv)) {
                difvb <- apply(dfr[, c("version", "binary")], 1, function(x) funcomp(x[1], x[2]))
                dfr   <- data.frame(dfr, difvb = difvb, stringsAsFactors = FALSE)
            }
            if (exists("binarydb", envir = .GlobalEnv) &&
                exists("crandb",   envir = .GlobalEnv)) {
                difbc <- apply(dfr[, c("binary", "crandb")], 1, function(x) funcomp(x[1], x[2]))
                dfr    <- data.frame(dfr, difbc = difbc, stringsAsFactors = FALSE)
            }
            if (exists("crandb",   envir = .GlobalEnv)) {
                difvc <- apply(dfr[, c("version", "crandb")], 1, function(x) funcomp(x[1], x[2]))
                dfr  <- data.frame(dfr, compare = difvc, stringsAsFactors = FALSE)
            }
            if (ndeps && exists("crandb", envir = .GlobalEnv)) {
                fungcc2 <- function(pkg, crandb) {
                    gcc2 <- crandb[crandb$Package == pkg, "NeedsCompilation"]
                    if (length(gcc2) == 0) gcc2 <- NA_character_
                    gcc2
                }
                crandb  <- get("crandb", envir = .GlobalEnv)
                gcc2  <- sapply(rownames(dfr), fungcc2, crandb)
                ldeps <- p_deps(char = rownames(dfr), recursive = TRUE,
                                verbose = FALSE, crandb = crandb)
                tdeps <- lengths(ldeps)
                ldep2 <- lapply(ldeps, function(pkg2) {
                                pkg2[!(pkg2 %in% list.files(.Library))] })
                ndeps <- lengths(ldep2)
                dfr   <- data.frame(dfr, gcc = gcc2, tdeps = tdeps, ndeps = ndeps,
                                    stringsAsFactors = FALSE)
                dfr   <- dfr[order(dfr$ndeps, dfr$tdeps, -dfr$compare),]
            }
        }
        dfr
    }
}

#' @export
#' @rdname p_vers
p_vers_deps <- function(..., char = NULL, ndeps = TRUE, subset = "compare < 4",
                        crandb = get("crandb", envir = .GlobalEnv)) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.null(pkgs)) return(NULL)
    if (length(pkgs) == 1) { if (is.na(pkgs)) return(NULL) }
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    pkgs  <- unique(pkgs)
    LF    <- list.files(.Library)
    pkgs  <- pkgs[!(pkgs %in% LF)]
    ldeps <- p_deps(char = pkgs, recursive = TRUE, verbose = FALSE, crandb = crandb)
    deps  <- unique(c(pkgs, unlist(ldeps)))
    deps  <- deps[!(deps %in% LF)]
    dfr   <- p_vers(char = deps, ndeps = ndeps)
    fun   <- paste0("subset(dfr, subset = ", subset, ")")
    eval(str2expression(fun))
}


### date d'installation des packages. voir base::file.info()
### p_instdate voir batata::since_packages.


