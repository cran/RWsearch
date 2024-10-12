## p_html
## @include p_graph.R


#' @title HTML Help Page, PDF Manual and Vignettes
#' @description
#'
#' \code{p_page} opens the default browser, connects to your local CRAN and displays the
#' home page of the package(s). An internet connexion is required.
#'
#' \code{p_html} and \code{p_html2} open the default browser and display the html help
#' page of the package, if it is installed. On Windows, \code{p_html} returns a local
#' server address \emph{http://127.0.0.1:*.html} and subfunctions listed in the page can
#' be explored whereas \code{p_html2} returns a file address \emph{file:///C:/*.html}
#' with no links to the subfunctions.
#'
#' \code{p_man} and its alias \code{p_htmlweb} open the default browser and display 
#' the html help pages stored by the R-project at \emph{https://search.r-project.org}. 
#' An internet connexion is required. A message is returned if the package does not 
#' exist in CRAN. In such case, use the function \code{\link{s_man}} for a deeper 
#' exploration.
#'
#' \code{p_pdf} displays in a pdf reader the pdf manual of the package, or generates
#' it on the fly
#' in the current directory if the package is installed. Miktex or Texlive is required.
#' This is a very fast function if the files already exist (and \code{overwrite= FALSE}) and
#' a (relatively) slow function if the files needs to be generated, usually much slower than:
#'
#' \code{p_pdfweb} downloads from you local CRAN the pdf manual of the package, saves it
#' in the current directory and opens it in the pdf application of your browser.
#' An internet connexion is required.
#'
#' \code{p_vig} is a wrapper of \code{utils::browseVignettes}. It opens the default browser
#' and displays a list of the vignettes related to a package, if they exist.
#'
#' \code{p_vig_all} wraps \code{utils::browseVignettes(NULL)}. It opens the default browser
#' and displays all vignettes available in the computer. This can be a very large html file.
#'
#' Use \code{\link{p_archive}} or \code{\link{p_archive}} to display in the browser
#' or in the console the package archives. An internet connexion is required.
#'
#'
#' @param   ...        any format recognized by \code{\link{cnsc}}, except list.
#'                     A vector of packages.
#' @param   char       (name to) a character vector. Use this argument if
#'                     \code{...} fails or if you call the function from another function.
#'                     If used, argument \code{...} is ignored.
#' @param   repos      character. The address of your local CRAN.
#' @param   overwrite  logical. Overwrite already existing file (and use LaTeX intensively).
#' @param   dir        character. The directory in which the files are read or written.
#'                     Default value \code{"."} is the current directory.
#' @examples
#' if (interactive()) {
#' p_page(RWsearch, sos, repos = "https://cloud.r-project.org")
#' p_html(RWsearch, sos)
#' p_man(RWsearch)
#' p_pdfweb(sos, repos = "https://cloud.r-project.org")
#'
#' ## Try
#' p_pdf(sos, dir = file.path(tempdir(), "ppdf"))
#' p_vig(RWsearch)
#' }
#' @export
#' @name p_html
NULL

#' @export
#' @rdname p_html
p_page <- function(..., char = NULL, repos = getOption("repos")[1]) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- file.path(repos, "web/packages", pkg, "index.html")
        msF <- paste("Package", pkg, "does not exist in CRAN.")
        trybrowseURL(url, msgF = msF)
    }
}

#' @export
#' @rdname p_html
p_html <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        file0 <- tryCatch(find.package(pkg),
                condition = function(c) {
                    message(paste("Package", pkg, "is not in .libPaths()"))
                }
        )
        if (!is.null(file0)) {
            utils::help(package = (pkg), help_type = "html")
        }
    }
}

#' @export
#' @rdname p_html
p_html2 <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        file0 <- tryCatch(find.package(pkg),
                condition = function(c) {
                    message(paste("Package", pkg, "is not in .libPaths()"))
                }
        )
        if (!is.null(file0)) {
            file1 <- file.path(file0, "html/00Index.html")
            tryopenfile(file1, msgTF = FALSE)
        }
    }
}

#' @export
#' @rdname p_html
p_htmlweb <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- file.path("https://search.r-project.org/CRAN/refmans",
                         pkg, "html/00Index.html")
        msF <- paste("Package", pkg, "does not exist in CRAN.")
        trybrowseURL(url, msgF = msF)
    }
}

#' @export
#' @rdname p_html
p_man <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- file.path("https://search.r-project.org/CRAN/refmans",
                         pkg, "html/00Index.html")
        msF <- paste("Package", pkg, "does not exist in CRAN.")
        trybrowseURL(url, msgF = msF)
    }
}

#' @export
#' @rdname p_html
p_pdf <- function(..., char = NULL, overwrite = FALSE, dir = ".") {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    WD   <- getwd()
    dir2 <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
    if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
    setwd(dir2)
    for (pkg in pkgs) {
        pkgpdf <- paste0(pkg, ".pdf")
        statut <- paste0(
            as.integer(overwrite),
            as.integer(is.element(pkg, list.files(.libPaths()))),
            as.integer(file.exists(pkgpdf))
        )
        switch(statut,
            "111" = {writepkg2pdf(pkg, dir = dir2)
                    message(paste("Package", pkg, "is installed. File",
                                             pkgpdf, "is overwritten."))},
            "110" = {writepkg2pdf(pkg, dir = dir2)
                    message(paste("Package", pkg, "is installed. File",
                                             pkgpdf, "is generated."))},
            "101" = {tryopenfile(pkgpdf)
                    message(paste("Package", pkg, "is not installed. Existing file",
                                             pkgpdf, "is used."))},
            "100" = message(paste("Package", pkg, "is not installed. File",
                                             pkgpdf, "does not exist.")),
            "011" = {tryopenfile(pkgpdf)
                    message(paste("Package", pkg, "is installed. Existing file",
                                             pkgpdf, "is used."))},
            "010" = {writepkg2pdf(pkg)
                    message(paste("Package", pkg, "is installed. File",
                                             pkgpdf, "is generated."))},
            "001" = {tryopenfile(pkgpdf)
                    message(paste("Package", pkg, "is not installed. Existing file",
                                             pkgpdf, "is used."))},
            "000" = message(paste("Package", pkg, "is not installed. File",
                                             pkgpdf, "does not exist."))
        )
    }
    setwd(WD)
}

#' @export
#' @rdname p_html
p_pdfweb <- function(..., char = NULL, repos = getOption("repos")[1]) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- paste0(repos, "/web/packages/", pkg, "/", pkg, ".pdf")
        msF <- paste("Package", pkg, "does not exist in CRAN.")
        trybrowseURL(url, msgF = msF)
    }
}

#' @export
#' @rdname p_html
p_vig <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    utils::browseVignettes(pkgs)
}

#' @export
#' @rdname p_html
p_vig_all <- function() {
    utils::browseVignettes(NULL)
}



## Generate the pdf manual of a package listed in .libPaths()
## Used by p_pdf
writepkg2pdf <- function(pkg, dir = ".") {
    path <- find.package(pkg)
    system(paste(shQuote(file.path(R.home("bin"), "R")),
                 "CMD Rd2pdf --force", shQuote(path)),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
    dir2 <- normalizePath(dir, winslash = "/")
    paste0(dir2, "/", pkg, ".pdf")
}


## Open a pdf file in the pdf viewer
## Used by p_pdf, table_pdfh, p_text2pdf
## Replaced by tryopenfile in RWsearch_4.9.9 and then not used anymore
openpdffile  <- function(pdffile) {
    if (Sys.info()[["sysname"]] == "Windows") {
        try(shell.exec(pdffile), silent = TRUE)
    } else {
        instruct <- paste(Sys.getenv("R_PDFVIEWER"), shQuote(pdffile))
        try(system(instruct, wait = FALSE), silent = TRUE)
    }
}



