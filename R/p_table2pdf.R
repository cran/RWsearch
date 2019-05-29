## p_table2pdf + fprotectTex
## @include p_inun.R


#' @title Package Information in Console and PDF Files
#' @description
#' \code{p_table} returns a subset of \code{crandb} for the given packages and the selected  
#' columns, by default the Package name, the Title and the Description. 
#' 
#' \code{p_table2} has a preset value to 2 columns: "Package", "Title" and prints the 
#' results in the console with a left alignment.
#' 
#' \code{p_table5} has a preset value to 5 columns: "Package", "Title", "Description", 
#' "Author", "Maintainer".
#' 
#' \code{p_table7} has a preset value to 7 columns: "Package", "Version", "Published", 
#' "Title", "Description", "Author", "Maintainer".
#' 
#' \code{table_pdf} prints the results of \code{p_table}, \code{p_table5} or \code{p_table7} 
#' in pdf file(s). Miktex or Texlive is required. 
#' 
#' \code{p_table2pdf}, \code{p_table3pdf}, \code{p_table5pdf}, \code{p_table7pdf} combine the 
#' above functions.
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, including list.
#'                    A vector or a list of packages.
#' @param   char      (name to) a character vector. Use this argument if \code{...} fails 
#'                    or if you call the function from another function. If used, 
#'                    argument \code{...} is ignored. 
#' @param   columns   character vector. A sub-vector of \code{colnames(crandb)}. The 
#'                    short form "P", "T", "D", "PT", "PD", "TD", "PTD", "A", "M", "AM" 
#'                    describing the Package name, Title, Description, Author, Maintainer
#'                    or a combination of them is accepted.
#' @param   crandb    data.frame \code{crandb}. The data.frame of CRAN packages.
#' @param   x         (list of) data.frame produced by \code{p_table} (with 3 columns), 
#'                    \code{p_table5} (5 columns) or \code{p_table7} (7 columns). If \code{x} 
#'                    is a list, the names of the list will be appended to \code{filename}.
#' @param   filename  character. The file name (with or without extension). 
#' @param   dir       character. The directory in which the files are read or written. 
#'                    Default value \code{"."} is the current directory.
#' @param   texops    character vector. Options passed to instruction \code{documentclass} 
#'                    in *.tex file.
#' @param   pdf       logical. \code{FALSE} generates the *.tex file. 
#'                    \code{TRUE} generates both the *.tex and *.pdf files. 
#' @param   cleantex  logical. Remove the \code{.tex} file(s) (only if \code{pdf = TRUE}).
#' @param   openpdf   logical. Open the generated *.pdf file(s) in a pdf viewer (only if
#'                    \code{pdf = TRUE}).
#' @param   verbose   logical. Print the path(s) to the generated file(s).
#' @examples
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' ## Use a large console (useful for p_table2())
#' p_table2(pacman, pdfsearch, sos)
#' (lst <- s_crandb_list("thermodynamic", "chemical reaction", "distillation"))
#' p_table2(lst)
#' \donttest{
#' ## print the table(s) in pdf file(s) and open it (them) in a pdf viewer.
#' p_table5pdf(pacman, pdfsearch, sos, dir = file.path(tempdir(), "ptable"))
#' p_table7pdf(lst, dir = file.path(tempdir(), "ptable"), cleantex = FALSE, openpdf = TRUE)
#' }
#' 
#' @export
#' @name p_table2pdf
NULL

#' @export
#' @rdname p_table2pdf
p_table <- function(..., char = NULL, columns = c("Package", "Title", "Description"), 
                   crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    columns <- fcccrandb(columns, crandb)
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        mapply(FUN = p_table, pkgs, char = pkgs, 
               MoreArgs = list(columns = columns, crandb = crandb), SIMPLIFY = FALSE)
    } else {
        if (!all(pkgs %in% crandb[,"Package"])) stop("(one of) pkgs is not in crandb")
        subset(crandb, crandb$Package %in% pkgs, select = columns) 
    }
}

#' @export
#' @rdname p_table2pdf
p_table2 <- function(..., char = NULL, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    print(p_table(char = pkgs, columns = c("Package", "Title"), crandb = crandb), 
          right = FALSE) 
}

#' @export
#' @rdname p_table2pdf
p_table5 <- function(..., char = NULL, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    p_table(char = pkgs,
           columns = c("Package", "Title", "Description", "Author", "Maintainer"),
           crandb = crandb)
}

#' @export
#' @rdname p_table2pdf
p_table7 <- function(..., char = NULL, crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    p_table(char = pkgs, columns = c("Package", "Version", "Published", "Title", 
           "Description", "Author", "Maintainer"), crandb = crandb)
}

#' @export
#' @rdname p_table2pdf
table_pdf <- function(x, filename = "SelectedPkgs.tex", dir = ".", 
                      texops = "a4paper,landscape,10pt", 
                      pdf = TRUE, cleantex = TRUE, openpdf = TRUE, verbose = TRUE) {
    wd   <- getwd()
    dir2 <- gsub("\\", "/", path.expand(dir), fixed = TRUE)
    if (!dir.exists(dir2)) dir.create(dir2, recursive = TRUE)
    if (is.list(x) && !is.data.frame(x)) {
        if (is.null(names(x))) names(x) <- seq_along(x)
        vecfiles        <- names(x)
        names(vecfiles) <- vecfiles 
        for (nom in names(x)) {
            setwd(dir2)
            filename2 <- tools::file_path_sans_ext(basename(filename))
            # filename2 <- paste(filename2, nom, sep = "_")
            # filename2 <- gsub(".", "", make.names(filename2), fixed = TRUE)
            filename2 <- paste(filename2, make.names(nom), sep = "_")
            filename2 <- gsub(".", "_", filename2, fixed = TRUE)
            filename2 <- paste0(filename2, ".tex")
            vecfiles[nom] <- table_pdfh(x[[nom]], filename2, texops, pdf, cleantex, 
                                        openpdf)
            # vecfiles[nom] <- filename2 
            setwd(wd)
        }
    } else {
            setwd(dir2)
            filename2 <- tools::file_path_sans_ext(basename(filename))
            # filename2 <- gsub(".", "", make.names(filename2), fixed = TRUE)
            filename2 <- gsub(".", "_", filename2, fixed = TRUE)
            filename2 <- paste0(filename2, ".tex")
            vecfiles  <- table_pdfh(x, filename2, texops, pdf, cleantex, openpdf)
            # vecfiles <- filename2 
            setwd(wd)
    }
    names(vecfiles) <- NULL
    # if (pdf) vecfiles <- gsub(".tex", ".pdf", vecfiles, fixed = TRUE)
    vecfiles <- file.path(dir, vecfiles, fsep = "/")
    vecfiles <- normalizePath(vecfiles, winslash = "/", mustWork = FALSE)
if (verbose) vecfiles else invisible(vecfiles)
}

#' @export
#' @rdname p_table2pdf
p_table2pdf <- function(..., char = NULL, filename = "Selectedpkgs", dir = ".", 
                        texops = "a4paper,landscape,10pt", 
                        pdf = TRUE, cleantex = TRUE, openpdf = TRUE, verbose = TRUE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    dfr  <- p_table(char = pkgs, columns = c("Package", "Title"), crandb = crandb)
    table_pdf(dfr, filename = filename, dir = dir, texops = texops, 
              pdf = pdf, cleantex = cleantex, openpdf = openpdf, verbose = verbose)
}

#' @export
#' @rdname p_table2pdf
p_table3pdf <- function(..., char = NULL, filename = "Selectedpkgs", dir = ".", 
                        texops = "a4paper,landscape,10pt", 
                        pdf = TRUE, cleantex = TRUE, openpdf = TRUE, verbose = TRUE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    dfr  <- p_table(char = pkgs, columns = c("Package", "Title", "Description"), 
                   crandb = crandb)
    table_pdf(dfr, filename = filename, dir = dir, texops = texops, 
              pdf = pdf, cleantex = cleantex, openpdf = openpdf, verbose = verbose)
}

#' @export
#' @rdname p_table2pdf
p_table5pdf <- function(..., char = NULL, filename = "Selectedpkgs", dir = ".", 
                        texops = "a4paper,landscape,10pt", 
                        pdf = TRUE, cleantex = TRUE, openpdf = TRUE, verbose = TRUE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    dfr  <- p_table(char = pkgs,
                   columns = c("Package", "Title", "Description", "Author", "Maintainer"),
                   crandb = crandb)
    table_pdf(dfr, filename = filename, dir = dir, texops = texops, 
              pdf = pdf, cleantex = cleantex, openpdf = openpdf, verbose = verbose)
}

#' @export
#' @rdname p_table2pdf
p_table7pdf <- function(..., char = NULL, filename = "Selectedpkgs", dir = ".", 
                        texops = "a4paper,landscape,10pt", 
                        pdf = TRUE, cleantex = TRUE, openpdf = TRUE, verbose = TRUE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    dfr  <- p_table(char = pkgs, columns = c("Package", "Version", "Published", "Title", 
                   "Description", "Author", "Maintainer"), crandb = crandb)
    table_pdf(dfr, filename = filename, dir = dir, texops = texops, 
              pdf = pdf, cleantex = cleantex, openpdf = openpdf, verbose = verbose)
}



## #' @title Protect the LaTeX reserved characters
## #' @description
## #' Protect the LaTeX reserved characters with antislash(es) \code{\\} so that the 
## #' output can be read by LaTeX compiler.
## #' OTHER FUNCTIONS pixiedust::sanitize_latex, dplR::latexify
## #' @param   x       (list of) character, vector of characters, data.frame of characters.
## #' @examples 
## #' fprotectTex(c("a", "$", "cde", "{", "fgh_uv"))
## #' @export
## #' @name fprotectTex
fprotectTex <- function(x) {
    if (is.list(x) && !is.data.frame(x)) {
        z <- lapply(x, fprotectTex)
        names(z) <- names(x)
    } else {
        protectChar <- function(char) {
            if (char %in% c("#", "$", "%", "^", "&", "_", "{", "}", "~", "\\")) {
                paste0("\\", char)
            } else { char }
        }
        if (is.null(dim(x))) {
            z <- x
            for (i in seq_along(x)) {
                vec  <- unlist(strsplit(x[i], ""))
                z[i] <- paste(sapply(vec, protectChar), collapse = "")
            }
        } else {
            z <- x
            for (i in 1:NROW(x)) {
                for (j in 1:NCOL(x)) {
                    vec    <- unlist(strsplit(x[i,j], ""))
                    z[i,j] <- paste(sapply(vec, protectChar), collapse = "")
                }
            }
        }
    }
return(z)
}



## Generate the *.tex and the *.pdf files from the table
## Used by table_pdf()
table_pdfh <- function(x, filename, texops, pdf, cleantex, openpdf) {
    if (!is.data.frame(x)) stop("x is not a data.frame")
    if (!(ncol(x) == 2 || ncol(x) == 3 || ncol(x) == 5 || ncol(x) == 7)) {
        stop ("data.frame x must have 2, 3, 5 or 7 columns.")
    }
    if (ncol(x) == 2) colw <- c(45,205) 
    if (ncol(x) == 3) colw <- c(35,70,145) 
    if (ncol(x) == 5) colw <- c(25,55,90,45,35) 
    if (ncol(x) == 7) colw <- c(25,10,10,45,80,45,35)
    con      <- file(filename, open = "w+", encoding = "UTF-8")
    writeLines(c(
        latexpdf::command("documentclass", options=texops, args="article"),
        latexpdf::command("usepackage", options="margin=10mm", args="geometry"),
        latexpdf::command("usepackage", options="utf8", args="inputenc"), 
        latexpdf::command("usepackage", options="T1", args="fontenc"),
        latexpdf::command("usepackage", args="longtable"),
        latexpdf::command("begin", args="document"),
        latexpdf::command("thispagestyle", args="empty"),
        latexpdf::command("pagestyle", args="empty"),
        (if (utils::packageVersion("latexpdf") < "0.1.5") {
            message("latexpdf >= 0.1.5 is recommanded.")
            latexpdf::as.tabular(fprotectTex(x),  
                      rules = c(1,1), walls = c(1,1), 
                      grid = TRUE, colwidth = paste0(colw, "mm"),
                      tabularEnvironment = "longtable")
        } else {
            latexpdf::as.tabular(x,  
                      rules = c(1,1), walls = c(1,1), 
                      grid = TRUE, colwidth = paste0(colw, "mm"),
                      tabularEnvironment = "longtable")  # reserve = TRUE)
        }), 
        latexpdf::command("end", args="document")),
    con)
    close(con)
    if (pdf) {
        tools::texi2pdf(filename, clean = TRUE)
        if (openpdf)  openpdffile(gsub(".tex", ".pdf", filename, fixed = TRUE))
        if (cleantex) file.remove(filename)
        gsub(".tex", ".pdf", filename, fixed = TRUE)
    } else {
        filename
    }
}



