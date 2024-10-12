## p_inun
## @include p_check.R


#' @title Packages Flagged by CRAN With a Deadline
#' @description
#' \code{p_deadline} returns a table with the packages that failed to R CMD check  
#' and are flagged by CRAN to make some changes before a certain deadline. This
#' feature has been introduced in the new version R_4.4.1 released mid-June 2024.
#' @param   crandb    data.frame \code{crandb}.
#'
#' @examples
#' ## In real life, download crandb with function crandb_down(), crandb_load().
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' dfr <- p_deadline() ; dfr
#' if (interactive() & nrow(dfr) > 0) {
#' p_check(dfr$Package, repos = "https://cloud.r-project.org")
#' }
#' @export
#' @name p_deadline
p_deadline <- function(crandb = get("crandb", envir = .GlobalEnv)) {
    tab <- crandb[!is.na(crandb$Deadline), c("Package","Deadline","Maintainer")]
	tab[order(tab$Deadline),]
}

