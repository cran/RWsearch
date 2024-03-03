## a_intro = RWsearch-package


#' @title  File zcrandb.rda: A Subset of crandb Dataset
#' @description
#' The file \emph{zcrandb.rda} contains a data.frame named \code{crandb} of
#' dimension 110 x 64. It is is a subset of 110 packages of the large \code{crandb}
#' data.frame usually downloaded from CRAN by the function \code{\link{crandb_down}}.
#' The use of \code{zcrandb.rda} avoids inappropriate connections to CRAN and
#' increases the speed of the examples.
#'
#' @examples
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' @keywords datasets
#' @docType data
#' @name zcrandb
NULL



#' @title File ztvdb.rda: A Subset of tvdb Dataset
#' @description
#' The file \emph{ztvdb.rda} contains a list of 6 task views named \code{tvdb}.
#' It is a subset of the large file \emph{tvdb.rda} that contain 42 task views
#' usually downloaded from CRAN by the function \code{\link{tvdb_down}}.
#' The use of \emph{ztvdb.rda} avoids inappropriate connections to CRAN and
#' increases the speed of the examples.
#'
#' @examples
#' tvdb_load(system.file("data", "ztvdb.rda", package = "RWsearch"))
#' @keywords datasets
#' @docType data
#' @name ztvdb
NULL



## CLOSE SPURIOUS CLOSED CONNECTIONS
close_libcurl <- function() {
    SC <- showConnections(all = TRUE)
    TF <- SC[, "class"] == "url-libcurl" & SC[, "isopen"] == "closed"
    ii <- as.integer(row.names(SC[TF,, drop=FALSE]))
    if (length(ii) > 0) for (i in rev(seq_along(ii))) close(getConnection(ii[i]))
}


## TRY A CONNECTION TO AN URL
## #' @title Try to create a connection to an URL.
## #' @description
## #' Open a connection if the url exists or return NULL if the url does not exist.
## #' The open connection must be closed later in the code!
## #' @param   url    a well-formed url.
## #' @param   open   character. Either "r"(closed) "rb"(opened), "rt"(opened)
## #' @param   filename character. A single filename
tryconurl <- function(url, open = "rb") {
    tryCatch(url(url, open = open, method = "libcurl"),
             condition = function(cond) {
        SC <- showConnections(all = TRUE)
        TF <- SC[, "description"] == url & SC[, "isopen"] == "closed"
        ii <- as.integer(row.names(SC[TF,, drop=FALSE]))
        if (length(ii) > 0) for (i in rev(seq_along(ii))) close(getConnection(ii[i]))
        NULL})
}
tryconfile <- function(filename, open = "rb") {
    tryCatch(file(filename, open = open, method = "libcurl"),
             condition = function(cond) {
        SC <- showConnections(all = TRUE)
        TF <- SC[, "description"] == filename & SC[, "isopen"] == "closed"
        ii <- as.integer(row.names(SC[TF,, drop=FALSE]))
        if (length(ii) > 0) for (i in rev(seq_along(ii))) close(getConnection(ii[i]))
        NULL})
}


## #' @title Open a (vector of) url(s) in a browser
## #' @description
## #' Utilise pour les bookmarks.
## #' @param   url        a (vector of) well-formed url(s).
## #' @param   msgT       character. Print the message associated to the url.
## #'                     The default NA prints no message.
## #' @param   msF        character. Print the message if url is not valid.
## #'                     The default NA prints "URL does not exist: url".
## #' @param   browser    character. Passed to utils::browseURL
## #' @param   encodeIfNeeded    character. Passed to utils::browseURL
trybrowseURL <- function(url, msgT = NA, msgF = NA, browser = getOption("browser"),
                         encodeIfNeeded = FALSE) {
    TC <- tryconurl(url)
    if (inherits(TC, "url")) {
        if (!is.na(msgT)) message(msgT)
        utils::browseURL(url, browser = browser, encodeIfNeeded = FALSE)
        close(TC)
        invisible(0)
    } else {
        if (is.na(msgF)) message(paste("URL does not exist:", url)) else {
            message(msgF)
        }
        invisible(1)
    }
}


## #' @title Try download a (vector of) url(s) and save it (them) as a file(s)
## #' @description
## #' voir \code{targz_down} et  \code{mirrors_down} qui utilisent correctement
## #' le nouveau trydownloadurl().
## #' @param   url        a (vector of) well-formed url(s).
## #' @param   destfile   character. A (vector of) filename(s) saved on the disk.
## #' @param   msgTF      logical. print the url that cannot be downloaded
trydownloadurl <- function(url, destfile, msgTF = FALSE) {
    if (length(url) > 1 || length(destfile) > 1) {
        stopifnot(length(url) == length(destfile))
        res <- mapply(trydownloadurl, url, destfile,
                      MoreArgs = list(msgTF = msgTF), USE.NAMES = FALSE)
        invisible(res)
    } else {
        TC <- tryconurl(url)
        if (inherits(TC, "url")) {
            utils::download.file(url, destfile, method = "libcurl",
                        quiet = TRUE, mode = "wb", cacheOK = FALSE)
            close(TC)
            invisible(0)
        } else {
            if (msgTF) message(paste("URL does not exist:", url))
            invisible(1)
        }
    }
}


## #' @title Try download a (vector of) file(s) and save it (them) as a file(s)
## #' @description
## #' voir \code{targz_down} et  \code{mirrors_down} qui utilisent correctement
## #' le nouveau trydownloadurl().
## #' @param   filename  character. A (vector of) well-formed file name(s).
## #' @param   type      character. Either "NA", "pager" or "editor". "pager" if for
## #'                    all platforms, "editor" is currently available only on Windows,
##                       "NA" calls the default program on all platforms.
## #' @param   msgTF     logical. print the url that cannot be downloaded
tryopenfile <- function(filename, type = "auto", msgTF = TRUE) {
    match.arg(type, c("auto", "editor", "pager"))
    if (length(filename) > 1) {
        invisible(sapply(filename, tryopenfile, type = type, msgTF = msgTF,
                         USE.NAMES = FALSE))
    } else {
        TC <- tryconfile(filename, open = "rb")
        if (is.null(TC)) {
            if (msgTF) message(paste("FILE does not exist:", filename))
            invisible(1)
        } else {
            if (Sys.info()[["sysname"]] == "Windows"
                & tools::file_ext(filename) == ""
                & type == "auto") type <- "editor"
            switch(Sys.info()[["sysname"]],
                "Windows" = switch(type,
                    "pager"  = file.show(filename, header = filename, title = "Pager"),
                    "editor"  = file.edit(filename, title = filename, fileEncoding = ""),
                    shell.exec(filename)
                    ),
                "Darwin"  =  switch(type,
                    "pager"  = file.show(filename, header = filename, title = "Pager"),
                    # "editor" = system2(getOption("editor"), filename, wait = FALSE),
                    system2("open", filename, wait = FALSE)
                    ),
                switch(type,
                    "pager" = file.show(filename, header = filename, title = "Pager"),
                    # "editor" = system2(getOption("editor"), filename, wait = FALSE),
                    system2("xdg-open", filename, wait = FALSE)
                    )
            )
            close(TC)
            invisible(0)
        }
    }
}



