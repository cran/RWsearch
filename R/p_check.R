## p_html
## @include p_archive.R


#' @title Return CRAN Package Check Results
#' @description
#' 
#' \code{p_check} opens the default browser, connects to your local CRAN and displays 
#' for each package the CRAN Package Check Results or the last Check Results recorded in 
#' CRAN archive (with the date of the archive). An internet connexion is required.
#' 
#' \code{p_check_lst} reads the check results from the repository and print the
#'  results as a list in the console, with a message for the archived package(s).  
#' An internet connexion is required. If a large number of packages is to be analyzed,
#' a preload of \code{checkdb} is required before launching the instruction (via 
#' \code{\link{checkdb_down}} or \code{\link{checkdb_load}}). This preload speeds 
#' up significantly the analysis.
#' 
#' \code{p_checkdeps} and \code{p_checkdeps_lst} extend the analysis to the 
#' package dependencies. 
#' 
#' Comprehensive tables of the check results for package sources and Windows binaries
#' can be displayed with \code{\link{h_cranchecks}} and \code{\link{h_crancheckwindows}}.
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, except list.
#'                    A vector of packages.
#' @param   char      (name to) a character vector. Use this argument if 
#'                    \code{...} fails or if you call the function from another function. 
#'                    If used, argument \code{...} is ignored. 
#' @param   repos     character. The address of your local CRAN.
#' @param   npkgs     integer. The number of packages from which a preload of
#'                    \code{checkdb} is required (via \code{\link{checkdb_down}} 
#'                    or \code{\link{checkdb_load}}).
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
#' ## In real life, download crandb and checkdb from CRAN or load them 
#' ## with functions crandb_down(), crandb_load(), checkdb_down(), checkdb_load(). 
#' ## checkdb can be ignored if less than npkgs are explored. 
#' ## In these examples, we use two small files of 110 and 107 packages.
#' \donttest{
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' checkdb_load(system.file("aabb", "zcheck_results.rds", package = "RWsearch"))
#' 
#' p_check(RWsearch, zmatrix, NotApkg, repos = "https://cloud.r-project.org")
#' p_graphF(RWsearch)
#' p_check_lst(igraph, zmatrix, NotApkg, repos = "https://cloud.r-project.org") 
#' p_checkdeps_lst(igraph, zmatrix, NotApkg, repos = "https://cloud.r-project.org") 
#' }
#' @export
#' @name p_check
p_check <- function(..., char = NULL, repos = getOption("repos")[1]) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        urlc <- paste0(repos, "/web/checks/check_results_", pkg, ".html")
        urli <- paste0(repos, "/web/packages/", pkg, "/index.html")
        TCc  <- tryconurl(urlc)
        if (inherits(TCc, "url")) {
            ## check_results normal
            utils::browseURL(urlc)
            close(TCc)
        } else {
            TCi <- tryconurl(urli, open = "rt")
            if (inherits(TCi, "url")) {            
                txt      <- readLines(TCi)
                close(TCi)
                lineArch <- txt[grep("Archived on", txt)]
                subdate  <- substr(lineArch, 13, 22) 
                if (length(subdate) == 0) {
                    ## archived before 2018-02-04 without a recorded date
                    message(paste("Package", pkg, "was archived at an unknown date."))
                    utils::browseURL(urli)
                } else {
                    ## archived after 2018-02-04 with a recorded date
                    urld <- paste0("https://cran-archive.r-project.org/web/checks/", 
                                   substr(subdate, 1, 4), "/", subdate, "_check_results_", 
                                   pkg, ".html")
                    message(paste0("Package ", pkg, " is in CRAN Archive since ", subdate, "."))
                    utils::browseURL(urld)                
                }
            } else {
                ## pkg/index.html does not exist
                message(paste("Package", pkg, "does not exist in CRAN or CRAN Archive."))
            }    
        }
    }
}

#' @export
#' @rdname p_check
p_check_lst <- function (..., char = NULL, npkgs = 10, repos = getOption("repos")[1]) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    pkgslibr <- list.files(.Library)
    pkgslib2 <- pkgs[ (pkgs %in% pkgslibr)]
    pkgs     <- pkgs[!(pkgs %in% pkgslibr)]
    if (length(pkgs) >= npkgs & !exists("checkdb", where = .GlobalEnv)) 
        stop(paste0("Too many packages ", length(pkgs), " > ", npkgs,
                    ". Apply first checkdb_down() or checkdb_load()."))
    if (length(pkgslib2) > 0) 
        message(paste("The following packages are ignored:\n", 
                paste(pkgslib2, collapse = ", ")))    
    lst <- as.list(pkgs)
    names(lst) <- pkgs
    cn <- c("Flavor", "Version", "T_total", "Status")
    if (exists("checkdb", where = .GlobalEnv)) {
        checkdb2 <- get("checkdb", envir = .GlobalEnv)
        for (pkg in lst) {
            if (pkg %in% checkdb2$Package) 
                lst[[pkg]] <- data.frame(checkdb2[checkdb2$Package == pkg, cn],
                                         row.names = NULL)
        }
    }
    for (pkg in pkgs) {
        if (inherits(lst[[pkg]], "character")) {
            urlc <- paste0(repos, "/web/checks/check_results_", pkg, ".html")
            docc <- tempfile()
            trdl <- trydownloadurl(urlc, docc)
            if (trdl == 0) {
                TABX   <- XML::readHTMLTable(docc, header = TRUE, which = 1, 
                                             stringsAsFactors = FALSE)
                colnames(TABX) <- gsub(" ", "", colnames(TABX))
                colnames(TABX) <- gsub("Tinstall", "T_install", colnames(TABX))
                colnames(TABX) <- gsub("Tcheck", "T_check", colnames(TABX))
                colnames(TABX) <- gsub("Ttotal", "T_total", colnames(TABX))
                lst[[pkg]] <- TABX[, cn]
            } else {
                urli <- paste0(repos, "/web/packages/", pkg, "/index.html")
                TCi  <- tryconurl(urli, open = "rt")
                if (inherits(TCi, "url")) {
                    txt <- readLines(TCi)
                    close(TCi)
                    lineArch <- txt[grep("Archived on", txt)]
                    subdate  <- substr(lineArch, 13, 22)
                    if (length(subdate) == 0) {
                        message(paste("Package", pkg, "was archived at an unknown date. No checks available."))
                        lst[[pkg]] <- paste("Package", pkg, "was archived at an unknown date. No checks available.")
                    } else {
                        urld <- paste0("https://cran-archive.r-project.org/web/checks/", 
                                       substr(subdate, 1, 4), "/", subdate, 
                                              "_check_results_", pkg, ".html")
                        docd <- tempfile()
                        trdd <- trydownloadurl(urld, docd)
                        TABX <- XML::readHTMLTable(docd, header = TRUE, which = 1, 
                                                   stringsAsFactors = FALSE)
                        colnames(TABX) <- gsub(" ", "", colnames(TABX))
                        colnames(TABX) <- gsub("Tinstall", "T_install", colnames(TABX))
                        colnames(TABX) <- gsub("Tcheck", "T_check", colnames(TABX))
                        colnames(TABX) <- gsub("Ttotal", "T_total", colnames(TABX))
                        message(paste0("Package ", pkg, " is in CRAN Archive since ", subdate, "."))
                        lst[[pkg]] <- TABX[, cn]
                    }        
                } else {
                    message(paste("Package", pkg, "does not exist in CRAN or CRAN Archive."))
                    lst[[pkg]] <- paste("Package", pkg, "does not exist in CRAN or CRAN Archive.")
                }        
            }
        }
    }
    lst
}

#' @export
#' @rdname p_check
p_checkdeps <- function(..., char = NULL, 
                    which = "DIL", recursive = TRUE, reverse = FALSE, 
                    crandb = get("crandb", envir = .GlobalEnv),
                    repos = getOption("repos")[1]) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
    deps  <- p_deps(char = pkgs, which = which, recursive = recursive, 
                    reverse = reverse, crandb = crandb)
    mpkgs <- unique(unlist(c(pkgs, deps)))
    exclpkgs <- c("base", "compiler", "datasets", "grDevices", "graphics", 
                  "grid", "methods", "parallel", "splines", "stats", "stats4", 
                  "tcltk", "tools", "utils")
    mpkgs <- mpkgs[!(mpkgs %in% exclpkgs)]
    p_check(char = mpkgs, repos = repos)
}

#' @export
#' @rdname p_check
p_checkdeps_lst <- function(..., char = NULL, which = "DIL", 
                    recursive = TRUE, reverse = FALSE, npkgs = 10,
                    crandb = get("crandb", envir = .GlobalEnv),
                    repos = getOption("repos")[1]) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
    deps  <- p_deps(char = pkgs, which = which, recursive = recursive, 
                    reverse = reverse, crandb = crandb)
    mpkgs <- unique(unlist(c(pkgs, deps)))
    p_check_lst(char = mpkgs, npkgs =  npkgs, repos = repos)
}



