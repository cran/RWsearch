## p_html
## @include p_down.R


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
#' \code{p_htmlweb} opens the default browser and displays the html help page of the package 
#' housed by the University of Pennsylvania. An internet connexion is required.
#' 
#' \code{p_pdf} displays in a pdf reader the pdf manual of the package, or generates it on the fly 
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
#' \code{p_check} opens the default browser, connects to your local CRAN and displays the 
#' CRAN Package Check Results page for the package(s). An internet connexion is required.
#'  
#' \code{p_archive} opens the default browser and displays the package archives.  
#' An internet connexion is required.
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
#' \donttest{
#' p_page(pacman, sos, repos = "https://cran.univ-paris1.fr") 
#' p_html(pacman, sos) 
#' p_htmlweb(pacman) 
#' p_check(pacman, repos = "https://cran.univ-paris1.fr") 
#' p_archive(pacman)
#' p_vig(pacman)
#' p_pdfweb(sos, repos = "https://cran.univ-paris1.fr")
#' p_pdf(sos, dir = file.path(tempdir(), "ppdf"))
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
        url <- paste0(repos, "/web/packages/", pkg, "/index.html")
        utils::browseURL(url)
    }
}

#' @export
#' @rdname p_html
p_html <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
for (pkg in pkgs) utils::help(package = (pkg), help_type = "html")
}

#' @export
#' @rdname p_html
p_html2 <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
         url <- file.path(find.package(pkg), "html/00index.html")
         utils::browseURL(url)
    }
}

#' @export
#' @rdname p_html
p_htmlweb <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- file.path("http://finzi.psych.upenn.edu/R/library",
                         pkg, "html/00Index.html")
        utils::browseURL(url)
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
            "111" = {writepkg2pdf(pkg)
                        message(paste("Package", pkg, "is installed. File", pkgpdf, "is overwritten."))},  
            "110" = {writepkg2pdf(pkg)
                        message(paste("Package", pkg, "is installed. File", pkgpdf, "is generated."))},  
            "101" = {openpdffile(pkgpdf)
                        message(paste("Package", pkg, "is not installed. Existing file", pkgpdf, "is used."))},
            "100" =     message(paste("Package", pkg, "is not installed. File", pkgpdf, "does not exist.")),
            "011" = {openpdffile(pkgpdf)
                        message(paste("Package", pkg, "is installed. Existing file", pkgpdf, "is used."))},
            "010" = {writepkg2pdf(pkg)
                        message(paste("Package", pkg, "is installed. File", pkgpdf, "is generated."))}, 
            "001" = {openpdffile(pkgpdf)
                        message(paste("Package", pkg, "is not installed. Existing file", pkgpdf, "is used."))},
            "000" =     message(paste("Package", pkg, "is not installed. File", pkgpdf, "does not exist.")) 
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
        utils::browseURL(url)
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

#' @export
#' @rdname p_html
p_check <- function(..., char = NULL, repos = getOption("repos")[1]) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- paste0(repos, "/web/checks/check_results_", pkg, ".html")
        utils::browseURL(url)
    }
}

#' @export
#' @rdname p_html
p_archive <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
    for (pkg in pkgs) {
        url <- paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/")
        utils::browseURL(url)
    }
}



## Generate the pdf manual of a package listed in .libPaths()
## Used by p_pdf
writepkg2pdf <- function(pkg) {
    path <- find.package(pkg)
    system(paste(shQuote(file.path(R.home("bin"), 
           "R")), "CMD Rd2pdf --force", shQuote(path)), 
            show.output.on.console = FALSE)  
}


## Open a pdf file in the pdf viewer
## Used by p_pdf and table_pdf
openpdffile  <- function(pdffile) {
    if (Sys.info()[["sysname"]] == "Windows") {
        shell.exec(pdffile)
    } else {
        try(system(pdffile), silent = TRUE)
        return(pdffile)
    }    
}



