## p_vers
## @include p_text2pdf.R


#' @title Package Version
#' @description
#' Return a data.frame with the version number of each package. \code{NA} and 
#' a warning is issued if the package is not installed. A vectorized version of 
#' \code{utils::packageVersion}. 
#' 
#' @param   ...       any format recognized by \code{\link{cnsc}}, except list.
#'                    A vector of packages.
#' @param   char      (name to) a character vector. Use this argument if \code{...} 
#'                    fails or if you call the function from another function. 
#'                    If used, argument \code{...} is ignored. 
#' 
#' @examples
#' p_vers(RWsearch, MASS, Matrix, survival, R)
#' 
#' @export
#' @name p_vers
p_vers <- function(..., char = NULL) {
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... (or char) cannot be a list.")
	funpkg <- function(pkg) {
		tryCatch({
			if (pkg %in% c("r", "R")) as.character(getRversion())
				else as.character(utils::packageVersion(pkg))
			}, 
			error = function(e) {
				warning(paste("there is no package called", pkg))
				NA_character_
			}
		)
	}
	lst <- sapply(pkgs, funpkg) 
data.frame("version" = lst, stringsAsFactors = FALSE)
}



