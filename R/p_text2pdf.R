## p_text2pdf
## @include p_table2pdf.R


#' @title Download Package Documentation in Text Files
#' @description
#' \code{p_text} extracts from CRAN the most relevant information related to one or 
#' several packages and print them in a text file which can be tailored to various 
#' formats: *.txt, *.md, *.tex for further treatment. 
#' 
#' \code{p_text2md} has preset values for markdown files. 
#' 
#' \code{p_text2tex} has preset values for latex files. 
#' 
#' \code{p_text2pdf} has preset values for pdf files. 
#' 
#' @param   ...        any format recognized by \code{\link{cnsc}}, including list.
#'                     A vector or packages or a named list of packages (with names 
#'                     being the keywords).
#' @param   char       (name to) a character vector or a list. Use this argument if 
#'                     \code{...} fails or if you call the function from another function. 
#'                     If used, argument \code{...} is ignored. 
#' @param   filename   character. The file name with extension. If \code{...} (or \code{...})
#'                     is a list, the names of the list will be appended to \code{filename}. 
#' @param   dir        character. The directory in which the files are read or written. 
#'                     Default value \code{"."} is the current directory.
#' @param   beforetext character. The text written at the beginning of the file.
#' @param   f_maintext function name. The function used to extract the main text from 
#'                     \code{crandb} (supplied with no parenthesis).
#' @param   sep1       character. The symbols written just before each package name.
#' @param   sep2       character. The symbols written just after each package name. 
#'                     If used with with mardkdown, add two blank characters at the end  
#'                     to force a new line. 
#' @param   eol        character. The end of line for the main text (but not for the 
#'                     header and the footer). "\\n" for text, "  \\n" for rmarkdown, 
#'                     " \\\\ \\n" for latex. 
#' @param   README     logical. Write the line related to the README page, if it exists.
#' @param   NEWS       logical. Write the line related to the NEWS page, if it exists.
#' @param   vignettes  logical. Write the lines related to the vignette(s), if they exist.
#' @param   aftertext  character. The text written at the end of the file.
#' @param   editor     logical. Open the text file with \code{editor}.
#' @param   pager      logical. Open the text file with \code{pager}.
#' @param   verbose    logical. List the generated file(s).
#' @param   cleantex   logical. Remove the \code{.tex} file(s).
#' @param   openpdf    logical. Open the pdf files in the default pdf viewer.
#' @param   crandb     data.frame \code{crandb}. The data.frame of CRAN packages.
#' @param   repos      character. The address of your local CRAN.
#' @examples
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' ## Search in crandb
#' vec <- s_crandb(search, find, select = "PT")
#' lst <- s_crandb_list(thermodynamic, "chemical reaction")
#' \donttest{
#' ## Generate a txt file
#' p_text(vec, filename = "SearchFind.txt", dir = file.path(tempdir(), "ptext"), 
#'        repos = "https://cloud.r-project.org") 
#' 
#' ## Generate a markdown file (and use rmarkdown::render() for further treatment)
#' ## In sep1, replace the '=' sign by the sharp sign (rejected by R CMD check)
#' p_text(char = lst, filename = "Chemistry.md", beforetext = funheadermd(), 
#'        f_maintext = funmaintext, sep1 = "= ", sep2 = "  ", 
#'        dir = file.path(tempdir(), "ptext"), repos = "https://cloud.r-project.org") 
#' 
#' ## Generate 4 tex + 4 pdf files (40-60 seconds)
#' p_text2pdf(lst, dir = file.path(tempdir(), "ptext"), cleantex = FALSE, 
#'           openpdf = FALSE, repos = "https://cloud.r-project.org") 
#' }
#' @name p_text2pdf
NULL

#' @export
#' @rdname p_text2pdf
p_text <- function(..., char = NULL, filename = "txtpkgs.txt", dir = ".", beforetext = "", 
                   f_maintext = funmaintext, sep1 = "== ", sep2 = " ==", eol = "\n", 
                   README = TRUE, NEWS = TRUE, vignettes = TRUE, aftertext = "", 
                   editor = FALSE, pager = FALSE, verbose = TRUE, 
                   crandb = get("crandb", envir = .GlobalEnv), 
                   repos = getOption("repos")[1]) {
    if (!isTRUE(capabilities("libcurl"))) {
        stop('p_text requires R compiled with libcurl. Run capabilities("libcurl")')
    }
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    if (tools::file_ext(filename) == "") stop("filename must have an extension. Default is txt.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    wd   <- getwd()
    dir2 <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
    if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)    
    if (is.list(pkgs) & !is.data.frame(pkgs)) {
        if (is.null(names(pkgs))) names(pkgs) <- seq_along(pkgs)
        vecfiles        <- names(pkgs)
        names(vecfiles) <- vecfiles 
        for (nom in names(pkgs)) {
            setwd(dir2)
            filename2 <- tools::file_path_sans_ext(basename(filename))
            filename2 <- paste(filename2, make.names(nom), sep = "_")
            filename2 <- gsub(".", "_", filename2, fixed = TRUE)
            filename2 <- paste0(filename2, ".", tools::file_ext(filename))
            p_texth(pkgs[[nom]], filename2, beforetext, f_maintext, sep1, sep2, eol, 
                    README, NEWS, vignettes, aftertext, crandb, repos)
            vecfiles[nom] <- filename2 
            if (editor) { 
                if (.Platform$GUI == "RStudio") {    
                    utils::edit(name = NULL, file = filename2, title = filename2, 
                                encoding = "UTF-8")
                } else {
                    utils::file.edit(filename2, title = filename2, 
                                     fileEncoding = "UTF-8")
                }
            }
            if (pager) file.show(filename2, header = filename2, title = filename2, 
                                 encoding = "UTF-8") 
            setwd(wd)
        }
    } else {
            setwd(dir2)
            filename2 <- tools::file_path_sans_ext(basename(filename))
            filename2 <- gsub(".", "_", filename2, fixed = TRUE)
            filename2 <- paste0(filename2, ".", tools::file_ext(filename))
            p_texth(pkgs, filename2, beforetext, f_maintext, sep1, sep2, eol, 
                    README, NEWS, vignettes, aftertext, crandb, repos)
            vecfiles <- filename2
            if (editor) { 
                if (.Platform$GUI == "RStudio") {    
                    utils::edit(name = NULL, file = filename2, title = filename2, 
                                encoding = "UTF-8")
                } else {
                    utils::file.edit(filename2, title = filename2, 
                                     fileEncoding = "UTF-8")
                }
            }
            if (pager) file.show(filename2, header = filename2, title = filename2, 
                                 encoding = "UTF-8") 
            setwd(wd)
    }
    names(vecfiles) <- NULL
    vecfiles <- file.path(dir, vecfiles, fsep = "/")
    vecfiles <- normalizePath(vecfiles, winslash = "/", mustWork = FALSE)
if (verbose) vecfiles else invisible(vecfiles)
}


#' @export
#' @rdname p_text2pdf
p_text2md <- function(..., char = NULL, filename = "mdpkgs.md", dir = ".", 
                    beforetext = funheadermd(), 
                    f_maintext = funmaintext, sep1 = "# ", sep2 = "  ", eol = "  \n", 
                    README = TRUE, NEWS = TRUE, vignettes = TRUE, aftertext = "", 
                    editor = FALSE, pager = FALSE, verbose = TRUE, 
                    crandb = get("crandb", envir = .GlobalEnv), 
                    repos = getOption("repos")[1]) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    if (tools::file_ext(filename) != "md") stop("filename must have a .md extension.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    vecfiles <- p_text(char = pkgs, filename = filename, dir = dir, 
                    beforetext = beforetext, f_maintext = f_maintext, 
                    sep1 = sep1, sep2 = sep2, eol = eol, README = README, 
                    NEWS = NEWS, vignettes = vignettes, aftertext = aftertext, 
                    editor = editor, pager = pager, verbose = FALSE, 
                    crandb = crandb, repos = repos)
if (verbose) vecfiles else invisible(vecfiles)
}

#' @export
#' @rdname p_text2pdf
p_text2tex <- function(..., char = NULL, filename = "texpkgs.tex", dir = ".", 
                    beforetext = funheadertex(), 
                    f_maintext = funmaintex, sep1 = "\\section{", sep2 = "}", eol = " \\\\\n", 
                    README = TRUE, NEWS = TRUE, vignettes = TRUE, aftertext = funfootertex(), 
                    editor = FALSE, pager = FALSE, verbose = TRUE, 
                    crandb = get("crandb", envir = .GlobalEnv), 
                    repos = getOption("repos")[1]) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    if (tools::file_ext(filename) != "tex") stop("filename must have a .tex extension.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    vecfiles <- p_text(char = pkgs, filename = filename, dir = dir, 
                    beforetext = beforetext, f_maintext = f_maintext, 
                    sep1 = sep1, sep2 = sep2, eol = eol, README = README, 
                    NEWS = NEWS, vignettes = vignettes, aftertext = aftertext, 
                    editor = editor, pager = pager, verbose = FALSE, 
                    crandb = crandb, repos = repos)
if (verbose) vecfiles else invisible(vecfiles)
}

#' @export
#' @rdname p_text2pdf
p_text2pdf <- function(..., char = NULL, filename = "pdfpkgs.pdf", dir = ".", 
                    beforetext = funheadertex(), 
                    f_maintext = funmaintex, sep1 = "\\section{", sep2 = "}", eol = " \\\\\n", 
                    README = TRUE, NEWS = TRUE, vignettes = TRUE, aftertext = funfootertex(), 
                    cleantex = TRUE, openpdf = FALSE, verbose = TRUE, 
                    crandb = get("crandb", envir = .GlobalEnv), 
                    repos = getOption("repos")[1]) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    if (tools::file_ext(filename) != "pdf") stop("filename must have a .pdf extension.")
    pkgs     <- if (is.null(char)) cnscinfun() else char
    filename <- gsub(".pdf", ".tex", filename)
    wd       <- getwd()
    dir2     <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
    vecfiles <- p_text(char = pkgs, filename = filename, dir = dir, 
                    beforetext = beforetext, f_maintext = f_maintext, 
                    sep1 = sep1, sep2 = sep2, eol = eol, README = README, 
                    NEWS = NEWS, vignettes = vignettes, aftertext = aftertext, 
                    editor = FALSE, pager = FALSE, verbose = FALSE, 
                    crandb = crandb, repos = repos)
    vecfiles2 <- gsub(".tex", ".pdf", vecfiles, fixed = TRUE)
    setwd(dir2)
    for (vecfile in vecfiles) {
        tools::texi2pdf(vecfile, clean = TRUE)
        if (openpdf) openpdffile(gsub(".tex", ".pdf", vecfile, fixed = TRUE))
        if (cleantex) file.remove(vecfile)
    }
    setwd(wd)
if (verbose) vecfiles2 else invisible(vecfiles2)
}



## (v-4.6.5) REPLACE BRUTE FORCE p_texth, funreadme, funnews, funvignettes
## WITH xml PARSING => XML::getHTMLLinks(doc)

p_texth <- function(pkgs, filename, beforetext, f_maintext, sep1, sep2, eol, 
                    README, NEWS, vignettes, aftertext, crandb, repos) {
	eollatex <- grepl("\\\\", eol, fixed = TRUE)
	con <- file(filename, open = "w+", encoding = "UTF-8")
    if (beforetext != "") writeLines(enc2utf8(beforetext), con = con)
    for (pkg in pkgs) {
		iurl    <- file.path(repos, "web", "packages", pkg, "index.html")
		purl    <- file.path(repos, "web", "packages", pkg)
		doc     <- tempfile()
		trd     <- trydownloadurl(iurl, doc)
		links   <- if (trd == 0) XML::getHTMLLinks(doc) else " "
		txtrme  <- grep("readme", links, ignore.case = TRUE, value = TRUE)[1]
		txtrme  <- grep("ReadMe", txtrme, ignore.case = FALSE, value = TRUE, invert = TRUE)[1]
		txtnews <- grep("NEWS", links, ignore.case = TRUE, value = TRUE)[1]
		txtvig  <- grep("vignettes", links, ignore.case = TRUE, value = TRUE)
		if (!is.na(txtrme[1])) {
			txtrme  <- file.path(purl, txtrme)
			if (eollatex) txtrme <- paste0("\\url{", txtrme, "}")
		}
		if (!is.na(txtnews[1])) {
			txtnews <- file.path(purl, txtnews) 
			if (eollatex) txtnews <- paste0("\\url{", txtnews, "}")
		}
		if (!is.na(txtvig[1])) {
			txtvig  <- file.path(purl, txtvig)
			if (eollatex) txtvig <- paste0("\\url{", txtvig, "}")
		}		
        txtpky  <- f_maintext(pkg, sep1, sep2, eol, crandb, repos)
        writeLines(txtpky, con = con, sep = eol)
        if (README && !is.na(txtrme[1]))    writeLines(txtrme,  con = con, sep = eol)
        if (NEWS  && !is.na(txtnews[1]))    writeLines(txtnews, con = con, sep = eol)
        if (vignettes && !is.na(txtvig[1])) writeLines(txtvig,  con = con, sep = eol)
        close_libcurl()
    }
    writeLines("\n", con = con)
    if (aftertext != "") writeLines(enc2utf8(aftertext), con = con)
    writeLines("\n", con = con)
    close(con)
    filename
}



