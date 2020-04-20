## crandb + fcccrandb
## @include checkdb.R


#' @title CRAN Packages (crandb.rda)
#' @description
#' \code{crandb_down} downloads from CRAN the file \emph{packages.rds}, a file refreshed 
#' everyday that describes the packages available in CRAN for this day, rename (with 
#' \code{make.names}) the column names that are syntactically invalid, removes the 
#' duplicated lines located at the end of the file, cleans some bad characters in the 
#' Description column, loads the resulting data.frame in .GlobalEnv under the name 
#' \emph{crandb} and saves it in the current directory with the name \code{crandb.rda}. 
#' If \code{oldfile} is defined, the vector of packages between the two files is compared.
#' 
#' \code{crandb_load} loads the file \code{filename} in .GlobalEnv under the name 
#' \code{crandb}. Equivalent to \code{load("crandb.rda")}. Use this function if you are 
#' not connected to internet or do not want to refresh your file.
#' 
#' \code{crandb_pkgs} displays all packages listed in \code{crandb}. The number of 
#' packages is larger than the number obtained with \code{nrow(available.packages())} 
#' since packages for all OSes are counted.
#' 
#' \code{crandb_fromto} displays the packages published in CRAN between two dates.
#' 
#' @param   dir         character. The directory where "crandb.rda" is saved and the old
#'                      "crandb.rda" is read. Default value \code{"."} is the current 
#'                      directory.
#' @param   oldfile     character or NULL. The (path to an) old file that will be compared 
#'                      to a freshly downloaded version of "crandb.rda" or to \code{filename}. 
#'                      Set to \code{NULL} if no comparison is required. 
#' @param   verbose     logical. \code{TRUE} prints the result. \code{FALSE} keeps it invisible.
#' @param   bydate      logical. List the package by date of publication rather than by 
#'                      alphabetical order.
#' @param   rev         logical. Print in reverse order.
#' @param   repos       character. The address of your local CRAN.
#' @param   filename    character. The (path to a) file "crandb.rda" or an equivalent.
#' @param   addtxt      character. Internal use.
#' @param   crandb      data.frame \code{crandb}. The data.frame of CRAN packages.
#' @param   from        Negative integer or character representing a date. The number of   
#'                      days preceeding \code{to} or a date before \code{to}. 
#' @param   to          date. The upper date in the search.
#' @examples
#' ### In this example, we use a small file.
#' ## List the 110 packages of this file, the ones uploaded since 2020-01-01
#' ## and those uploaded in the last 15 days before the last date (2020-04-17)
#' 
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' crandb_pkgs()
#' dim(crandb)   
#' colnames(crandb) 
#' crandb$Published
#' crandb_fromto(from = "2020-01-01", to = Sys.Date())
#' pkgs <- crandb_fromto(from = -15, to = max(crandb$Published)) ; pkgs
#' p_table2(pkgs)   # Print in the console (better if full width)
#' \donttest{
#' p_display7(pkgs, dir = tempdir())   # Display in the browser
#' 
#' ### In the real life, we use a fresh file downloaded from CRAN (6 MB / 20").
#' ## Here, we retrieve the packages uploaded in the last 2 days.
#' # crandb_down(dir = tempdir(), repos = "https://cloud.r-project.org") 
#' # crandb_fromto(-2)
#' }
#' @name crandb
NULL

#' @export
#' @name crandb
crandb_down <- function(dir = ".", oldfile = "crandb.rda", verbose = TRUE, 
                        repos = getOption("repos")[1]) {
    pure_desc <- function(vec) {
        purify <- function(tt) {
            tt   <- gsub("<, ", "", tt)
            tt   <- gsub(", >", "", tt)
            tt   <- gsub("\t", " ", tt)
            tt   <- gsub("\n", " ", tt)
            tt   <- gsub("     ", " ", tt)
            tt   <- gsub("    ", " ", tt)
            tt   <- gsub("   ", " ", tt)
            tt   <- gsub("  ", " ", tt)
        return(tt)
        }
    sapply(vec, purify)
    }
    oldtest <- FALSE
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    if (file.exists(file.path(dir, oldfile))) { 
        oldtest <- TRUE
        addtxt  <- basename(oldfile)
        craold  <- get(load(file.path(dir, oldfile)))
    } else {
        if (oldfile != "crandb.rda") stop(paste(oldfile, "does not exist in this directory."))
    }
    urlrds <- paste0(repos, "/web/packages/packages.rds")
    con    <- gzcon(url(urlrds, method = "libcurl"))
    crandb <- as.data.frame(readRDS(con), stringsAsFactors = FALSE)
    close(con)
    colnames(crandb) <- make.names(colnames(crandb), unique = TRUE, allow_ = TRUE)
    crandb <- crandb[!duplicated(crandb$Package), ]
    crandb[,"Title"]       <- pure_desc(crandb[,"Title"])
    crandb[,"Description"] <- pure_desc(crandb[,"Description"])
    save(crandb, file = file.path(dir, "crandb.rda"), 
         compress = "xz", compression_level = 6)
    crandb <<- crandb
    if (oldtest) { 
        lst    <- crandb_comp(crandb, craold, addtxt = addtxt)
    } else {      
        pnew   <- crandb[,"Package"]
        tnew1  <- "crandb.rda saved and loaded."
        tnew3  <- length(unique(pnew))
        tnew4  <- "packages listed between"
        tnew5  <- min(as.Date(crandb[,"Published"]))
        tnew6  <- "and"
        tnew7  <- max(as.Date(crandb[,"Published"]))
        txtnew <- paste(tnew1, tnew3, tnew4, tnew5, tnew6, tnew7)
        lst    <- list("newfile" = txtnew)
    }
if (verbose) lst else invisible(lst)
}

#' @export
#' @rdname crandb
crandb_load <- function(filename = "crandb.rda") {
    if (file.exists(filename)) {
        glf    <- get(load(filename, envir = .GlobalEnv))
        txt2   <- "crandb loaded."
        txt3   <- length(unique(glf$Package))
        txt4   <- "packages listed between"
        txt5   <- min(as.Date(glf$Published))
        txt6   <- "and"
        txt7   <- max(as.Date(glf$Published))
        list("newfile" = paste(txt2, txt3, txt4, txt5, txt6, txt7))
    } else {
        stop(paste("File", basename(filename), "does not exist in this directory."))
    }
}

#' @export
#' @rdname crandb
crandb_comp <- function(filename = "crandb.rda", oldfile = "crandb-old.rda", addtxt = "") {
    if (is.data.frame(filename) & is.data.frame(oldfile)) {
        fnew  <- filename
        fold  <- oldfile
        tnew2 <- "crandb.rda saved and loaded."
        told2 <- if (addtxt == "") "oldfile" else addtxt
    } else {
        fnew  <- get(load(filename))
        fold  <- get(load(oldfile))
        tnew2 <- basename(filename)
        told2 <- basename(oldfile)
    }
    pnew    <- fnew[,"Package"]
    pold    <- fold[,"Package"]
    datnew  <- as.Date(fnew[,"Published"])
    datold  <- as.Date(fold[,"Published"])
    newpkgs <- pnew[!is.element(pnew, pold)]
    rempkgs <- pold[!is.element(pold, pnew)]
    
    tnew3  <- length(unique(pnew))
    tnew4  <- "packages listed between"
    tnew5  <- min(datnew)
    tnew6  <- "and"
    tnew7  <- max(datnew)
    txtnew <- paste(tnew2, tnew3, tnew4, tnew5, tnew6, tnew7)
    
    told3  <- length(unique(pold))
    told4  <- "packages listed between"
    told5  <- min(datold)
    told6  <- "and"
    told7  <- max(datold)
    txtold <- paste(told2, told3, told4, told5, told6, told7)
    
    if (told7 > tnew7) warning("oldfile is more recent than filename!")    
    xpold  <- pold[datold == told7]
    xpnew  <- pnew[datnew == told7]
    xpkgs  <- xpnew[!is.element(xpnew, xpold)]    
    dpkgs  <- pnew[datnew > told7]
    upkgs  <- sort(unique(c(xpkgs, dpkgs)))  

    lremp  <- length(rempkgs)
    lnewp  <- length(newpkgs)
    luplp  <- length(upkgs) 
    lrefp  <- luplp - lnewp
    trnru  <- paste(lremp, "removed,", lnewp, "new,", 
                    lrefp, "refreshed,", luplp, "uploaded packages.")
    txtne2 <- c(txtnew, trnru)
    
    lst <- list("newfile" = txtne2,
                "oldfile" = txtold,                
       "removed_packages" = rempkgs, 
           "new_packages" = newpkgs,
      "uploaded_packages" = upkgs)
return(lst)
}

#' @export
#' @rdname crandb
crandb_pkgs <- function(bydate = FALSE, rev = FALSE, 
                        crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (bydate) {
                unique(crandb[order(crandb$Published), "Package"])
            } else { 
                unique(crandb[, "Package"])
            }
    if (rev) pkgs <- rev(pkgs)
return(pkgs)
}

#' @export
#' @rdname crandb
crandb_fromto <- function(from = -10, to = Sys.Date(), 
                          crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    if (is.null(to)) to <- Sys.Date()
    if (is.infinite(to)) to <- Sys.Date()
    to <- as.Date(to)
    if (is.null(from) || is.infinite(from)) from <- as.Date("2005-01-01")
    from <- if (is.numeric(from)) {
                if (from > 0) {
                    stop(paste0("from (= ", from, ") must be a negative integer (or a date).")) 
                }
                to + from
            } else {
                as.Date(from)
            }
    if (from > to) {
        stop(paste0("from: ", from, " must be earlier than to: ", to))
    }
    if (from > Sys.Date()) {
        warning(paste0("from: ", from, " is posterior to Sys.Date(): ", Sys.Date(),
        "\n  Results should be empty."))
    }
    Date   <- as.Date(crandb[,"Published"])
    lignes <- (Date >= from & Date <= to)
    z      <- unique(crandb[lignes, "Package"])
return(z)
}



## #' Check match.arg 
fcccrandb <- function(columns, crandb = get("crandb", envir = .GlobalEnv), 
                      PTDAM.only = FALSE) {
    funcolumn <- function(column) {  
        switch(column,
            "P"   = "Package",
            "T"   = "Title",
            "D"   = "Description",
            "PT"  = c("Package", "Title"),
            "PD"  = c("Package", "Description"),
            "TD"  = c("Title", "Description"),
            "PTD" = c("Package", "Title", "Description"),
            "A"   = "Author",
            "M"   = "Maintainer",
            "AM"  = c("Author", "Maintainer"),
            "PTDAM"  = c("Package", "Title", "Description", "Author", "Maintainer"),
            if (PTDAM.only) { 
                stop("select/columns should be one of P, T, D, PT, PD, TD, PTD, A, M, AM.")
            } else {
                column 
            }
        )
    }
    columns <- unlist(lapply(columns, funcolumn))
match.arg(columns, choices = colnames(crandb), several.ok = TRUE)
} 



