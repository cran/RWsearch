## cranmirrors
## @include crandb.R


#' @title CRAN archive (CRAN-archive.html + archivedb)
#' @description
#' \code{cranmirrors_down} downloads the csv file of CRAN mirrors, modifies the 
#' "Maintainer" and "Host" columns, eventually saves the modified data.frame on 
#' the disk, loads this dat.frame in .GlobalEnv and print in the console a subset 
#' with the selected columns.
#' 
#' @param   filename   character. The path to file "CRAN-mirrors1.csv" (or equivalent). 
#' @param   dir        character. The directory where \code{filename} is saved.
#'                     Default value \code{"."} is the current directory.
#' @param   columns    a vector of integers or a vector of names. 
#'                     The column numbers or the column names. Allowed numbers 
#'                     are within 1:9. Allowed names are: "Name", "Country",
#'                     "City", "URL", "Host", "Maintainer", "OK", "CountryCode" 
#'                     and "Comment". 
#' @param   save       logical. Save the file. If \code{FALSE}, the default, the 
#'                     file is just loaded in .GlobalEnv and a subset is printed
#'                     in the console.
#' @param   url        character. The url address of the CRAN csv file.
#' 
#' @examples
#' cranmirrors_down(dir = tempdir(), save = TRUE)
#' 
#' @name cranmirrors
NULL

#' @export
#' @rdname cranmirrors
cranmirrors_down <- function(filename = "CRAN-mirrors1.csv", dir = ".",
						columns = c(1,3,7), save = FALSE,
						url = "ftp://cran.r-project.org/pub/R/CRAN_mirrors.csv") {
    TC <- tryCatch(url(url, open = "rt", method = "libcurl"),  
             condition = function(cond) {stop("url does not exist.")}
    )
    cranmirrors <- utils::read.csv(TC, stringsAsFactors = FALSE, encoding = "UTF-8")
    close(TC)
	dfr2 <- gsub(" # ", "@", cranmirrors[,"Maintainer"])
	dfr2 <- gsub(">|<", "", dfr2)
	lst  <- sapply(dfr2, strsplit, split = " ", fixed = TRUE, USE.NAMES = FALSE)
	vec  <- sapply(lst, function(x) x[length(x)])
	cranmirrors[, "Maintainer"] <- vec
	vec2 <- sapply(cranmirrors[, "Host"], function(x) substring(
				   x, 1, 76), USE.NAMES = FALSE)
	cranmirrors[, "Host"] <- vec2
    if (save) {
		if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
		destfile <- file.path(dir, filename)
		utils::write.csv(cranmirrors, file = destfile, row.names = FALSE, 
						 fileEncoding = "UTF-8")
    }
	cranmirrors <<- cranmirrors
	if (is.null(columns)) columns <- 0
	message(paste0("Modified cranmirrors loaded in .GlobalEnv."))
	message(paste0("Print columns ", paste0(columns, collapse = ", ")))
	cranmir <- if (columns[1] == 0) cranmirrors else cranmirrors[,columns]
if ("OK" %in% colnames(cranmir)) cranmir[order(cranmir$OK),] else cranmir
}



