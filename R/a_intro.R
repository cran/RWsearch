## a_intro = RWsearch-package



#' @title Package RWsearch
#' @description
#' Search by keywords in R packages, task views, CRAN, the web and display the
#' results in the console or in txt, html or pdf files. Download the package
#' documentation (html index, README, NEWS, pdf manual, vignettes, source code,
#' binaries) with a single instruction. Visualize the package dependencies and
#' CRAN checks. Compare the package versions, unload and install the packages
#' and their dependencies in a safe order. Explore CRAN archives. Use the above
#' functions for task view maintenance. Access web search engines from the
#' console thanks to 80+ bookmarks. All functions accept standard and
#' non-standard evaluation. Inspired by the packages ctv, foghorn, latexpdf,
#' pacman and sos.
#'
#' @examples
#' ### THE W IN RWsearch: LAUNCH WEBSITES AND SEARCH ENGINES
#' if (interactive()) {
#' h_cranbydate(repos = "https://cloud.r-project.org")
#' h_yt("Serge Gainsbourg Ne dis rien")
#' h_osm("La Ferriere sous Jougne")
#' h_mw(recension)
#' h_lexilogos()
#' }
#'
#' ### A CONVENIENT FUNCTION FOR NON-STANDARD EVALUATION
#' ## Non-standard content (nsc1, nsc2), standard content ("stc3", "double word4")
#' ## and regular object (obj) stored in .GlobalEnv can be merged with cnsc()
#' obj <- c("obj5", "obj6")
#' cnsc(nsc1, nsc2, "stc3", "double word4", obj)
#'
#' ### DOWNLOAD CRANDB AND CHECKDB
#' ## In real life, download crandb and checkdb from CRAN or load them
#' ## with functions crandb_down(), crandb_load(), checkdb_down(), checkdb_load().
#' ## checkdb can be ignored if less than npkgs are explored.
#' ## In this example, we use two small files.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' checkdb_load(system.file("aabb", "zcheck_results.rds", package = "RWsearch"))
#'
#' ### PRINT THE LATEST PACKAGES UPLOADED INTO CRAN (LAST DATE = "2021-06-01")
#' crandb_fromto(from = "2021-03-01", to = Sys.Date())
#' crandb_fromto(from = -15, to = max(crandb$Published))
#'
#' ### SEARCH IN CRANDB
#' ## Search in crandb. Use standard or non-standard content.
#' ## Display the results in a vector or in a list.
#' s_crandb(search, find, cran, web)
#' s_crandb(search, find, cran, web, select = "PD", mode = "and")
#' s_crandb("^f", select = "P")
#' s_crandb(c("thermodynamic", "chemical reaction"))
#' (lst <- s_crandb_list(thermodynamic, "chemical reaction"))
#' (lst2 <- lapply(lst, function(x) x[1:2]))
#'
#' ### DISPLAY THE RESULTS
#' ## in the console, in (txt, md, pdf) files or in the browser.
#' if (interactive()) {
#' p_table2(lst)
#' p_table7pdf(lst, dir = file.path(tempdir(), "ptable"), cleantex = FALSE, openpdf = TRUE)
#' p_text(lst2, dir = file.path(tempdir(), "ptext1"), pager = TRUE,
#'            repos = "https://cloud.r-project.org")
#' p_text2pdf(lst, dir = file.path(tempdir(), "ptext1"), cleantex = FALSE,
#'            openpdf = TRUE, repos = "https://cloud.r-project.org")
#' p_display(lst, dir = file.path(tempdir(), "pdisp1"))
#' }
#'
#' ### VISUALIZE THE DOCUMENTATION (IN A BROWSER)
#' ## from the installed packages on your computer
#' ## or from the packages listed in https://search.r-project.org 
#' if (interactive()) {
#' p_html(brew, sig)
#' p_htmlweb(foghorn)
#' p_pdfweb(sos, repos = "https://cloud.r-project.org")
#' }
#'
#' ### VISUALIZE THE PACKAGE DEPENDENCIES AND THE INSTALLED VERSIONS
#' if (interactive()) {
#' p_graphF(actuar, fitdistrplus, reverse = TRUE) # Children
#' p_graphF(RWsearch)    # Parents
#' p_vers_deps(RWsearch) # Installed versions
#' }
#'
#' ### VISUALIZE THE PACKAGE CHECKS (USE checkdb FOR FASTER RESULTS)
#' if (interactive()) {
#' p_check(RWsearch, repos = "https://cloud.r-project.org")
#' res <- p_checkdeps_lst(RWsearch, repos = "https://cloud.r-project.org")
#' head(res, 3)
#' }
#'
#' ### DOWNLOAD THE DOCUMENTATION
#' ## Vector => download in the "docpkgs" directory ("." is the current directory)
#' ## List   => download in subdirectories named after the keywords
#' ## (non-standard content is accepted)
#' \donttest{
#' p_down(pacman, pdfsearch, sos, dir = file.path(tempdir(), "pdown"),
#'        repos = "https://cloud.r-project.org")
#' p_down(lst, dir = file.path(tempdir(), "pdown"), repos = "https://cloud.r-project.org")
#' }
#'
#' ### SEARCH WITH sos (ON R-PROJECT HELP PAGES), rdrr AND rdoc
#' if (interactive()) {
#' (res <- s_sos(distillation))
#' head(data.frame(res), 3)
#' tail(data.frame(res), 3)
#' h_rdrr(distillation)
#' h_rdoc(distillation)
#' }
#'
#' ### TASK VIEW MAINTENANCE
#' ## In real life, download crandb and tvdb from CRAN or load them from your directory
#' ## with functions crandb_down(), crandb_load(), tvdb_down(), tvdb_load().
#' ## In this example, we use small files.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' tvdb_load(system.file("data", "ztvdb.rda", package = "RWsearch"))
#'
#' ## List the task views
#' tvdb_vec()
#' tvdb_pkgs(gR, Genetics, Robust)
#'
#' ## Search for some packages in the task views
#' s_tvdb(actuar, FatTailsR, MASS, zoo, nopackage)
#'
#' ## Search for recent packages in crandb that contain the keyword
#' ## and verify if the packages are already referred in the task view.
#' ## from = "2017-01-01" and "2018-01-01" are selected for this small example.
#' s_crandb_tvdb("distribution", tv = "Distributions", from = "2017-01-01")
#' s_crandb_tvdb("distribution", tv = "Distributions", from = "2018-01-01")
#'
#' ### EXPLORE CRAN ARCHIVE AND DOWNLOAD OLD tar.gz FILES
#' ## In real life, download archivedb and crandb from CRAN
#' ## with the functions archivedb_down() and crandb_down().
#' ## In this example, we load two small files (50 and 43 packages).
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' archivedb_load(system.file("aabb", "zCRAN-archive.html", package = "RWsearch"))
#' archivedb_npkgs()
#' lapply(archivedb_list(), tail)
#'
#' ## Download the latest tar.gz version from CRAN archive
#' ## (this works for both both existing and removed packages).
#' \donttest{
#' p_downarch(fitur, zmatrix, dir = file.path(tempdir(), "pdownarch"))
#' }
#'
#' @import      tools
#' @import      utils
#' @importFrom  brew      brew
#' @importFrom  latexpdf  as.tabular  command
#' @importFrom  networkD3 forceNetwork  sankeyNetwork  saveNetwork
#' @importFrom  sig       list_sigs  sig
#' @importFrom  sos       findFn
#' @importFrom  XML       readHTMLTable
#' @aliases RWsearch
#' @name    RWsearch-package
#' @docType package
NULL






#' @title  File zcrandb.rda: A Subset of crandb Dataset
#' @description
#' The file \emph{zcrandb.rda} contains a data.frame named \code{crandb} of
#' dimension 110 x 64. It is is a subset of 110 packages of the large \code{crandb}
#' data.frame usually downloaded from CRAN by the function \code{\link{crandb_down}}.
#' The use of \code{zcrandb.rda} avoids inappropriate connections to CRAN and
#' increases the speed of the examples.
#'
#' @examples
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' @keywords datasets
#' @docType data
#' @name zcrandb
NULL



#' @title File ztvdb.rda: A Subset of tvdb Dataset
#' @description
#' The file \emph{ztvdb.rda} contains a list of 6 task views named \code{tvdb}.
#' It is a subset of the large file \emph{tvdb.rda} that contain 42 task views
#' usually downloaded from CRAN by the function \code{\link{tvdb_down}}.
#' The use of \emph{ztvdb.rda} avoids inappropriate connections to CRAN and
#' increases the speed of the examples.
#'
#' @examples
#' tvdb_load(system.file("data", "ztvdb.rda", package = "RWsearch"))
#' @keywords datasets
#' @docType data
#' @name ztvdb
NULL



## CLOSE SPURIOUS CLOSED CONNECTIONS
close_libcurl <- function() {
    SC <- showConnections(all = TRUE)
    TF <- SC[, "class"] == "url-libcurl" & SC[, "isopen"] == "closed"
    ii <- as.integer(row.names(SC[TF,, drop=FALSE]))
    if (length(ii) > 0) for (i in rev(seq_along(ii))) close(getConnection(ii[i]))
}


## TRY A CONNECTION TO AN URL
## #' @title Try to create a connection to an URL.
## #' @description
## #' Open a connection if the url exists or return NULL if the url does not exist.
## #' The open connection must be closed later in the code!
## #' @param   url    a well-formed url.
## #' @param   open   character. Either "r"(closed) "rb"(opened), "rt"(opened)
## #' @param   filename character. A single filename
tryconurl <- function(url, open = "rb") {
    tryCatch(url(url, open = open, method = "libcurl"),
             condition = function(cond) {
        SC <- showConnections(all = TRUE)
        TF <- SC[, "description"] == url & SC[, "isopen"] == "closed"
        ii <- as.integer(row.names(SC[TF,, drop=FALSE]))
        if (length(ii) > 0) for (i in rev(seq_along(ii))) close(getConnection(ii[i]))
        NULL})
}
tryconfile <- function(filename, open = "rb") {
    tryCatch(file(filename, open = open, method = "libcurl"),
             condition = function(cond) {
        SC <- showConnections(all = TRUE)
        TF <- SC[, "description"] == filename & SC[, "isopen"] == "closed"
        ii <- as.integer(row.names(SC[TF,, drop=FALSE]))
        if (length(ii) > 0) for (i in rev(seq_along(ii))) close(getConnection(ii[i]))
        NULL})
}


## #' @title Open a (vector of) url(s) in a browser
## #' @description
## #' Utilise pour les bookmarks.
## #' @param   url        a (vector of) well-formed url(s).
## #' @param   msgT       character. Print the message associated to the url.
## #'                     The default NA prints no message.
## #' @param   msF        character. Print the message if url is not valid.
## #'                     The default NA prints "URL does not exist: url".
## #' @param   browser    character. Passed to utils::browseURL
## #' @param   encodeIfNeeded    character. Passed to utils::browseURL
trybrowseURL <- function(url, msgT = NA, msgF = NA, browser = getOption("browser"),
                         encodeIfNeeded = FALSE) {
    TC <- tryconurl(url)
    if (inherits(TC, "url")) {
        if (!is.na(msgT)) message(msgT)
        utils::browseURL(url, browser = browser, encodeIfNeeded = FALSE)
        close(TC)
        invisible(0)
    } else {
        if (is.na(msgF)) message(paste("URL does not exist:", url)) else {
            message(msgF)
        }
        invisible(1)
    }
}


## #' @title Try download a (vector of) url(s) and save it (them) as a file(s)
## #' @description
## #' voir \code{targz_down} et  \code{mirrors_down} qui utilisent correctement
## #' le nouveau trydownloadurl().
## #' @param   url        a (vector of) well-formed url(s).
## #' @param   destfile   character. A (vector of) filename(s) saved on the disk.
## #' @param   msgTF      logical. print the url that cannot be downloaded
trydownloadurl <- function(url, destfile, msgTF = FALSE) {
    if (length(url) > 1 || length(destfile) > 1) {
        stopifnot(length(url) == length(destfile))
        res <- mapply(trydownloadurl, url, destfile,
                      MoreArgs = list(msgTF = msgTF), USE.NAMES = FALSE)
        invisible(res)
    } else {
        TC <- tryconurl(url)
        if (inherits(TC, "url")) {
            utils::download.file(url, destfile, method = "libcurl",
                        quiet = TRUE, mode = "wb", cacheOK = FALSE)
            close(TC)
            invisible(0)
        } else {
            if (msgTF) message(paste("URL does not exist:", url))
            invisible(1)
        }
    }
}


## #' @title Try download a (vector of) file(s) and save it (them) as a file(s)
## #' @description
## #' voir \code{targz_down} et  \code{mirrors_down} qui utilisent correctement
## #' le nouveau trydownloadurl().
## #' @param   filename  character. A (vector of) well-formed file name(s).
## #' @param   type      character. Either "NA", "pager" or "editor". "pager" if for
## #'                    all platforms, "editor" is currently available only on Windows,
##                       "NA" calls the default program on all platforms.
## #' @param   msgTF     logical. print the url that cannot be downloaded
tryopenfile <- function(filename, type = "auto", msgTF = TRUE) {
    match.arg(type, c("auto", "editor", "pager"))
    if (length(filename) > 1) {
        invisible(sapply(filename, tryopenfile, type = type, msgTF = msgTF,
                         USE.NAMES = FALSE))
    } else {
        TC <- tryconfile(filename, open = "rb")
        if (is.null(TC)) {
            if (msgTF) message(paste("FILE does not exist:", filename))
            invisible(1)
        } else {
            if (Sys.info()[["sysname"]] == "Windows"
                & tools::file_ext(filename) == ""
                & type == "auto") type <- "editor"
            switch(Sys.info()[["sysname"]],
                "Windows" = switch(type,
                    "pager"  = file.show(filename, header = filename, title = "Pager"),
                    "editor"  = file.edit(filename, title = filename, fileEncoding = ""),
                    shell.exec(filename)
                    ),
                "Darwin"  =  switch(type,
                    "pager"  = file.show(filename, header = filename, title = "Pager"),
                    # "editor" = system2(getOption("editor"), filename, wait = FALSE),
                    system2("open", filename, wait = FALSE)
                    ),
                switch(type,
                    "pager" = file.show(filename, header = filename, title = "Pager"),
                    # "editor" = system2(getOption("editor"), filename, wait = FALSE),
                    system2("xdg-open", filename, wait = FALSE)
                    )
            )
            close(TC)
            invisible(0)
        }
    }
}



