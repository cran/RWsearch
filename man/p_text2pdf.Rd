% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_text2pdf.R
\name{p_text2pdf}
\alias{p_text2pdf}
\alias{p_text}
\alias{p_text2md}
\alias{p_text2tex}
\title{Download Package Documentation in Text Files}
\usage{
p_text(..., char = NULL, filename = "txtpkgs.txt", dir = ".",
  beforetext = "", f_maintext = funmaintext, sep1 = "== ",
  sep2 = " ==", eol = "\\n", README = TRUE, NEWS = TRUE,
  ChangeLog = TRUE, vignettes = TRUE, RqmdRmd = FALSE, DOI = FALSE,
  aftertext = "", editor = FALSE, pager = FALSE, verbose = TRUE,
  crandb = get("crandb", envir = .GlobalEnv),
  repos = getOption("repos")[1])

p_text2md(..., char = NULL, filename = "mdpkgs.md", dir = ".",
  beforetext = funheadermd(), f_maintext = funmaintext, sep1 = "# ",
  sep2 = "  ", eol = "  \\n", README = TRUE, NEWS = TRUE,
  ChangeLog = TRUE, vignettes = TRUE, RqmdRmd = FALSE, DOI = FALSE,
  aftertext = "", editor = FALSE, pager = FALSE, verbose = TRUE,
  crandb = get("crandb", envir = .GlobalEnv),
  repos = getOption("repos")[1])

p_text2tex(..., char = NULL, filename = "texpkgs.tex", dir = ".",
  beforetext = funheadertex(), f_maintext = funmaintex,
  sep1 = "\\\\section{", sep2 = "}", eol = " \\\\\\\\\\n",
  README = TRUE, NEWS = TRUE, ChangeLog = TRUE, vignettes = TRUE,
  RqmdRmd = FALSE, DOI = FALSE, aftertext = funfootertex(),
  editor = FALSE, pager = FALSE, verbose = TRUE, crandb = get("crandb",
  envir = .GlobalEnv), repos = getOption("repos")[1])

p_text2pdf(..., char = NULL, filename = "pdfpkgs.pdf", dir = ".",
  beforetext = funheadertex(), f_maintext = funmaintex,
  sep1 = "\\\\section{", sep2 = "}", eol = " \\\\\\\\\\n",
  README = TRUE, NEWS = TRUE, ChangeLog = TRUE, vignettes = TRUE,
  RqmdRmd = FALSE, DOI = FALSE, aftertext = funfootertex(),
  cleantex = TRUE, openpdf = FALSE, verbose = TRUE,
  crandb = get("crandb", envir = .GlobalEnv),
  repos = getOption("repos")[1])
}
\arguments{
\item{...}{any format recognized by \code{\link{cnsc}}, including list.
A vector or packages or a named list of packages (with names
being the keywords).}

\item{char}{(name to) a character vector or a list. Use this argument if
\code{...} fails or if you call the function from another function.
If used, argument \code{...} is ignored.}

\item{filename}{character. The file name with extension. If \code{...} (or \code{...})
is a list, the names of the list will be appended to \code{filename}.}

\item{dir}{character. The directory in which the files are read or written.
Default value \code{"."} is the current directory.}

\item{beforetext}{character. The text written at the beginning of the file.}

\item{f_maintext}{function name. The function used to extract the main text from
\code{crandb} (supplied with no parenthesis).}

\item{sep1}{character. The symbols written just before each package name.}

\item{sep2}{character. The symbols written just after each package name.
If used with with mardkdown, add two blank characters at the end
to force a new line.}

\item{eol}{character. The end of line for the main text (but not for the
header and the footer). "\\n" for text, "  \\n" for rmarkdown,
" \\\\ \\n" for latex.}

\item{README}{logical. Write the line related to the README page, if it exists.}

\item{NEWS}{logical. Write the line related to the NEWS page, if it exists.}

\item{ChangeLog}{logical. Write the line related to the ChangeLog page, if it exists.}

\item{vignettes}{logical. Write the lines related to the vignette(s), if they exist.}

\item{RqmdRmd}{logical. Add to the vignettes the source files in R, Rmd or qmd format, if they exist.}

\item{DOI}{logical. Write the line related to the DOI link, if it exists.}

\item{aftertext}{character. The text written at the end of the file.}

\item{editor}{logical. Open the text file with \code{editor}.}

\item{pager}{logical. Open the text file with \code{pager}.}

\item{verbose}{logical. List the generated file(s).}

\item{crandb}{data.frame \code{crandb}. The data.frame of CRAN packages.}

\item{repos}{character. The address of your local CRAN.}

\item{cleantex}{logical. Remove the \code{.tex} file(s).}

\item{openpdf}{logical. Open the pdf files in the default pdf viewer.}
}
\description{
\code{p_text} extracts from CRAN the most relevant information related to one or
several packages and print them in a text file which can be tailored to various
formats: *.txt, *.md, *.tex for further treatment.

\code{p_text2md} has preset values for markdown files.

\code{p_text2tex} has preset values for latex files.

\code{p_text2pdf} has preset values for pdf files.
}
\examples{
## In real life, download crandb from CRAN or load it from your directory
## with functions crandb_down() or crandb_load().
## In this example, we use a small file.
crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))

## Search in crandb
vec <- s_crandb(search, find, select = "PT") ; vec
lst <- s_crandb_list(thermodynamic, "chemical reaction") ; lst
lst2 <- lapply(lst, function(x) x[1:2]) ; lst2
dir <- file.path(tempdir(), "ptext")

## Generate a txt file
\donttest{
p_text(vec[1:5], filename = "SearchFind.txt", dir = dir, 
       RqmdRmd = TRUE, DOI = TRUE, repos = "https://cloud.r-project.org")

## Generate 2 tex + 2 pdf files (10-20 seconds)
## Try the options cleantex = FALSE and openpdf = TRUE on lst
if (interactive()) {
p_text2pdf(lst2, dir = dir, cleantex = TRUE, openpdf = FALSE,
           RqmdRmd = TRUE, DOI = TRUE, repos = "https://cloud.r-project.org")
}
}
}
