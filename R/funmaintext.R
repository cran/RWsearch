## funmaintext
## @include f_pdf.R


#' @title Modify the Main Text and the Markdown Header in p_text Function
#' @description
#' Use \code{funmaintext} or \code{funmaintext2} to select the function that displays 
#' the main text. Usage is \code{f\_maintext = funmaintext} (without curly braces). 
#' 
#' Use \code{funheadermd} to insert markdown header in function \code{sep1 = funheadermd()} 
#' (with curly braces). 
#' 
#' See the example in \code{p\_text}. To create you own functions, use these functions 
#' as a pattern. The five parameters in \code{f\_maintext, funmaintext} are mandatory.
#' \code{funheadermd} can be freely modified. 
#' @param   pkg        character. The package name.
#' @param   sep1       character. The symbols written just before each package name.
#' @param   sep2       character. The symbols written just after each package name.
#' @param   eol        character. The end of line for the main text (but not for the 
#'                     header and the footer). \code{"\\n"} for text, \code{"  \\n"} 
#'                     for rmarkdown, \code{" \\\\ \\n"} for latex. 
#' @param   crandb     data.frame \code{crandb}. The data.frame of CRAN packages.
#' @param   repos      character. The address of your local CRAN.
#' @param   title      character. The title of the .md document (and then in the .pdf file).
#' @param   author     character. The author of the .md document.
#' @param   date       character. The date of the document. Any text format is accepted.
#' @param   keep_tex   character. "true" or "false".
#' @param   toc        character. "true" or "false".
#' @param   number_sections  character. "true" or "false".
#' @param   fontsize   character. Usually "10pt", "11pt", "12pt.
#' @param   papersize  character. The usual tex format. Example: "a4paper".
#' @param   margin     character. In inches, cm or mm. Example: "0.5in", "1.5cm", "25mm". 
#' @export
#' @name funmaintext
funmaintext <- function(pkg, sep1, sep2, eol, crandb, repos) {
    xl  <- crandb[crandb$Package == pkg,]
    txt <- paste0(
        "\n\n",
        sep1, xl$Package, sep2,        eol,
        xl$Package, ": ", xl$Title,    eol,
        xl$Description,                eol,
        "Depends:    ",  xl$Depends,   eol, 
        "Imports:    ",  xl$Imports,   eol, 
        "Suggests:   ",  xl$Suggests,  eol,
        "Version:    ",  xl$Version,   eol,
        "Published:  ",  xl$Published, eol,
        "Maintainer: ",  xl$Maintainer,eol,
        file.path(repos, "web/packages", xl$Package, "index.html"), eol,
        paste0(repos,  "/web/packages/", xl$Package, "/", xl$Package, ".pdf")
    )
txt
}

#' @export
#' @rdname funmaintext
funmaintex <- function(pkg, sep1, sep2, eol, crandb, repos) {
    xl  <- crandb[crandb$Package == pkg,]
    txt <- paste0(
        "\n\n",
        sep1, xl$Package, sep2,     "  \n",  
        xl$Package, ": ", xl$Title,    eol,
        fprotectTex(xl$Description),   eol,
        "Depends:    ",  xl$Depends,   eol, 
        "Imports:    ",  xl$Imports,   eol, 
        "Suggests:   ",  xl$Suggests,  eol,
        "Version:    ",  xl$Version,   eol,
        "Published:  ",  xl$Published, eol,
        "Maintainer: ",  fprotectTex(xl$Maintainer), eol,
        "\\url{", file.path(repos, "web/packages", xl$Package, "index.html"), "}", eol,
        "\\url{", paste0(repos, "/web/packages/", xl$Package, "/", xl$Package, ".pdf"), "}"
    )
txt
}

#' @export
#' @rdname funmaintext
funheadermd <- function(title = "TITLE", author = "AUTHOR", date = Sys.Date(), 
                  keep_tex = "false", toc = "false", number_sections = "true",
                  fontsize = "10pt", papersize = "a4paper", margin = "1in") {
    txt <- paste0(
        "---\n",
        "title: ", title, "\n",
        "author: ", author, "\n",
        "date: ", date, "\n", 
        "output:\n", 
        "  pdf_document:\n", 
        "    keep_tex: ", keep_tex, "\n",
        "    toc: ", toc, "\n", 
        "    number_sections: ", number_sections, "\n",
        "fontsize: ", fontsize, "\n",
        "papersize: ", papersize, "\n", 
        "geometry: margin=", margin, "\n", 
        "---\n\n"
    )
txt
}

#' @export
#' @rdname funmaintext
funheadertex <- function(fontsize = "10pt", papersize = "a4paper", margin = "1in") {
    txt <- paste0(
        "\\documentclass[", papersize, ",", fontsize, "]{article}\n",
        "\\usepackage[margin=", margin, "]{geometry}\n",
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage[T1]{fontenc}\n", 
        "\\usepackage{graphicx}\n", 
        "\\usepackage{url}\n", 
        "\\begin{document}\n", 
        "\\thispagestyle{empty}\n", 
        "\\pagestyle{empty}\n", 
        "\\urlstyle{same}\n\n"
    )
txt
}

#' @export
#' @rdname funmaintext
funfootertex <- function() {
    txt <- paste0(
        "\n\\end{document}\n"
    )
txt
}




