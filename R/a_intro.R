## a_intro = RWsearch-package



#' @title Package RWsearch
#' @description
#' Search by keywords in R packages, task views, CRAN, the web and display the results in
#' console, txt, html or pdf pages. Within a single instruction, download the whole documentation 
#' (html index, pdf manual, readme, vignettes, source code, etc), either in a flat format 
#' or in subdirectories defined by the keywords. Several functions for task view maintenance. 
#' Quick links to more than 60 web search engines. 
#' Lazy evaluation of non-standard content is available throughout the package and eases 
#' the use of many functions. Packages RWsearch and pacman share the same syntax and 
#' complement each other. Inswpired by packages ctv, foghorn, latexpdf, packagefinder, 
#' pacman, sos, websearchr. 
#' 
#' @examples
#' ### NON-STANDARD CONTENT - NON-STANDARD EVALUATION
#' ## Non-standard content (nsc1, nsc2), standard content ("stc3", "double word4") 
#' ## and regular object (obj) stored in .GlobalEnv can be merged with cnsc()
#' obj <- c("obj5", "obj6")
#' cnsc(nsc1, nsc2, "stc3", "double word4", obj)
#' 
#' ### DOWNLOAD CRANDB
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' 
#' ### SEARCH IN CRANDB
#' ## Search in crandb. Use standard or non-standard content. 
#' ## Display the results in a vector or in a list.
#' s_crandb(search, find, cran, web)
#' s_crandb(search, find, cran, web, select = "PD", mode = "relax")
#' s_crandb(c("thermodynamic", "chemical reaction"))
#' (lst <- s_crandb_list(thermodynamic, "chemical reaction"))
#' \donttest{
#' ### DISPLAY THE RESULTS
#' ## in the console, in (txt, md, pdf) files or in the browser.
#' p_table2(lst)
#' p_table7pdf(lst, dir = file.path(tempdir(), "ptable"), cleantex = FALSE, openpdf = TRUE)
#' p_text(lst, dir = file.path(tempdir(), "ptext2"), editor = TRUE,  
#'            repos = "https://cran.univ-paris1.fr")
#' p_text2pdf(lst, dir = file.path(tempdir(), "ptext2"), cleantex = FALSE,  
#'            openpdf = TRUE, repos = "https://cran.univ-paris1.fr")
#' p_display(lst, dir = tempdir())
#'  
#' ### VISUALIZE THE DOCUMENTATION 
#' ## from installed packages or U. Pennsylviana in the browser
#' p_html(brew, sig) 
#' p_htmlweb(foghorn) 
#' p_pdfweb(sos, repos = "https://cran.univ-paris1.fr")
#' 
#' ### DOWNLOAD THE DOCUMENTATION
#' ## Vector => download in the "docpkgs" directory ("." is for current directory)
#' ## List   => download in subdirectories named after the keywords
#' ## (non-standard content is accepted)
#' p_down(pacman, pdfsearch, sos, dir = file.path(tempdir(), "pdown"),  
#'        repos = "https://cran.univ-paris1.fr")
#' p_down(lst, dir = file.path(tempdir(), "pdown"), repos = "https://cran.univ-paris1.fr")
#' 
#' ### SEARCH WITH sos (U. PENNSYLVANIA)
#' s_sos("chemical reaction")
#' res <- s_sos(distillation)
#' as.data.frame(res)
#' res
#' 
#' ### LAUNCH WEBSITES AND SEARCH ENGINES
#' h_ttp("www.r-project.org")
#' h_cranbydate(repos = "https://cran.univ-paris1.fr")
#' h_yt("Serge Gainsbourg Ne dis rien")
#' h_so(R, deep, neural, network)
#' h_osm("La Ferriere sous Jougne")
#' h_mw(recension)
#' h_lexilogos()
#' }
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
#' ## Search for the recent packages in crandb that contain the keyword 
#' ## and verify if the packages are already refereed in the task view.
#' ## from = "2017-01-01" and "2018-01-01" are selected for this small example.
#' s_crandb_tvdb("distribution", tv = "Distributions", from = "2017-01-01")
#' s_crandb_tvdb("distribution", tv = "Distributions", from = "2018-01-01")
#' 
#' @import  parallel
#' @import  utils
#' @importFrom  brew        brew
#' @importFrom  latexpdf    as.tabular  command
#' @importFrom  sig         sig   list_sigs
#' @importFrom  sos         findFn
#' @importFrom  tools       texi2pdf
#' @aliases RWsearch
#' @name    RWsearch-package
#' @docType package
NULL





#' @title  File zcrandb.rda: A Subset of crandb Dataset
#' @description
#' File \emph{zcrandb.rda} loads in .Globalenv as \code{crandb}, a data.frame of dim 50 x 65. 
#' It contains 50 packages that match the keywords used in the examples of this package. 
#' 
#' File \emph{zcrandb.rda}, 22 ko, acts as a replacement of the original but large file 
#' \emph{crandb.rda} to be downloaded from CRAN. The weight of \emph{crandb.rda} was 
#' 4.3 Mo with 13001 packages on August 31, 2018 and 6.7 Mo with 13902 packages on 
#' March 17, 2019 . The use of \code{zcrandb.rda} avoids inappropriate connections 
#' to CRAN and increases the speed in the examples.
#' @examples
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' @keywords datasets
#' @docType data
#' @name zcrandb
NULL



#' @title File ztvdb.rda: A Subset of tvdb Dataset
#' @description
#' File \emph{ztvdb.rda} is a small file of 5 ko that contains 6 task views and acts as 
#' a replacement of the large file \emph{tvdb.rda} downloaded from CRAN that contains  
#' 36 task views. It loads in .GlobalEnv as \code{tvdb}. The use of \emph{ztvdb.rda} 
#' avoids inappropriate connections to CRAN and increases the speed in the examples. 
#' @examples
#' tvdb_load(system.file("data", "ztvdb.rda", package = "RWsearch"))
#' @keywords datasets
#' @docType data
#' @name ztvdb
NULL



