## p_vers
## @include p_unload_all.R


#' @title Package Version and Number of Dependencies
#' @description
#' The information displayed by \code{p_vers} depends on the availability of 
#' \code{crandb} in \code{.GlobalEnv}.
#' 
#' If \code{crandb} is not loaded in \code{.GlobalEnv}, \code{p_vers} returns a
#' data.frame with two columns: first column \code{nsloaded} (TRUE/FALSE) detects 
#' (with \code{base::isNamespaceLoaded}) if the package namespaces are loaded. 
#' Second column \code{version} is the version number of the installed packages. 
#' 
#' If \code{crandb} is loaded in \code{.GlobalEnv}, a third and a fourth columns 
#' display the version numbers recorded in \code{crandb} and a comparaison with
#' the installed version. Possible values for colummn \code{compare} are -2, -1, 
#' 0, 1, 2, 3. Values -2, 2 and 3 are for \code{NA} in the second, the third or
#' both columns. Value -1 is for an outdated package, 0 for packages with identical
#' version numbers, +1 for a more recent package than the one in CRAN. 
#' 
#' If \code{ndeps = TRUE}, a fifth and a sixth columns are added with the number 
#' of recursive dependencies per package. Column \code{tdeps} includes the base
#' and recommended packages. Column \code{ndeps} does not count them. This option
#' can take some time. Set it to \code{FALSE} if you need speed.
#' 
#' \code{p_vers_deps} calculates the same information but includes the recursive
#' dependencies. Subsetting by \code{"compare < 0"} returns a shorter data.frame 
#' with the uninstalled (-2) and the outdated (-1) packages. Packages marked with
#' \code{nsloaded = TRUE} must be detached and unloaded before any reinstallation.
#' Using this instruction before running \code{install.packages} or \code{p_inst}
#' is very useful as it detects packages that are locked and cannot be reinstalled
#' and gives the order for the reinstallation. 
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
            funpkg <- function(pkg) {
                tryCatch({
                    if (pkg %in% c("r", "R")) as.character(getRversion())
                        else utils::packageDescription(pkg, fields = "Version")
                    }, 
                    condition = function(c) { NA_character_ }
                )
            }
            vec <- sapply(pkgs, funpkg)
            nsl <- sapply(pkgs, isNamespaceLoaded)
            dfr <- data.frame("nsloaded" = nsl, "version" = vec, 
                              row.names = pkgs, stringsAsFactors = FALSE)
            if (exists("crandb", envir = .GlobalEnv)) {
                crandb  <- get("crandb", envir = .GlobalEnv)
                funcran <- function(pkg, crandb) {
                    ver <- crandb[crandb$Package == pkg, "Version"]
                    if (length(ver) == 0) ver <- NA_character_
                    ver
                }
                vers <- sapply(pkgs, funcran, crandb)
                dfr  <- data.frame(dfr, crandb = vers, stringsAsFactors = FALSE)
                funcomp <- function(a, b) {
                    if (is.na(a) && is.na(b)) 3 else {
                        if (is.na(a)) -2 else {
                            if (is.na(b)) 2 else {
                                utils::compareVersion(a, b)
                            }
                        }
                    }
                }
                comp <- apply(dfr, 1, function(x) funcomp(x[2], x[3]))
                dfr  <- data.frame(dfr, compare = comp, stringsAsFactors = FALSE)
                if (ndeps) {                
                    ldeps <- p_deps(char = pkgs, recursive = TRUE, 
                                    verbose = FALSE, crandb = crandb)
                    tdeps <- lengths(ldeps)
                    ldep2 <- lapply(ldeps, function(pkg2) {
                                    pkg2[!(pkg2 %in% list.files(.Library))] })
                    ndeps <- lengths(ldep2)
                    dfr   <- data.frame(dfr, tdeps, ndeps, stringsAsFactors = FALSE)
                    dfr   <- dfr[order(dfr$ndeps, dfr$tdeps, -dfr$compare),]
                }
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


