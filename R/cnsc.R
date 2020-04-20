## cnsc
## @include archivedb.R


#' @title Conversion of Non-Standard Content into a Character Chain
#' @description
#' \code{cnsc} converts standard content and non-standard content \code{...} into a 
#' character chain. Standard content, including \emph{call}s, is evaluated.
#' Non-standard content and non-existing objects (in \code{.GlobalEnv}) are converted into 
#' character chains. Regular lists are kept unchanged.
#' 
#' \code{cnscinfun} is intended to be used inside a function. It should not be 
#' used directly. If you find it appealing, copy the code of \code{cnscinfun}
#' in your package and use it as an hidden function. 
#' 
#' @param   ...     Character vectors, standard or non-standard, existing or non-existing 
#'                  R objects, regular call. Examples : \code{"word1"}; \code{c("word1 word2")}; 
#'                  \code{c("word1", "word2")};  \code{"word1", "word2", "word3"};
#'                  \code{word1, word2, c("word3", "word4")}.
#' @examples
#' ### cnsc
#' ## Non-standard content (nsc1, nsc2), standard content ("stc3", "double word4") 
#' ## and regular object (vec) stored in .GlobalEnv are merged.
#' vec <- c("obj5", "obj6")
#' cnsc(nsc1, nsc2, "stc3", "double word4", vec)
#' 
#' ## Lists, either name in .GlobalEnv or call, are evaluated.
#' lst <- list(A = c("txt1","txt2","txt3"), B = c("txt4", "txt5"))
#' cnsc(lst) 
#' cnsc(list(C = c("pkg1","pkg2","pkg3"), D = c("pkg4","pkg5")))
#' 
#' ### cnscinfun
#' fun <- function(...) cnscinfun()
#' fun(nsc1, nsc2, "stc3", "double word4", vec) 
#' fun(lst)
#' 
#' ### cnscinfun used in RWsearch: one line at the begining of each function.
#' ### An easy-to-use Non Standard Evaluation, mainly for characters.
#' funsort <- function(..., char = NULL) {
#'   words <- if (is.null(char)) cnscinfun() else char
#'   sort(words)
#'   # or more complex code 
#' }
#' funsort(nsc1, nsc2, "stc3", "double word4", vec) 
#' funsort(char = sample(vec, 5, replace = TRUE)) 
#' 
#' @export
#' @name cnsc
cnsc <- function(...) {
    MC <- match.call(definition = sys.function(sys.parent(1)),
                            call = sys.call(sys.parent(1)), 
                     expand.dots = FALSE,
                           envir = parent.frame(2L))
    if (length(MC) == 1L) { 
        z <- ""       # or z <- NULL
    } else {
        mconvert <- function(cla, res) {
            switch(cla,
            "character" = res,
            "call"      = eval(res), 
            "name"      = if (as.character(res) %in% ls(.GlobalEnv)) {
                              eval(res) } else { as.character(res) },
            "numeric"   = as.character(res),
            "integer"   = as.character(res),
            "complex"   = as.character(res)
            )
        }
        RES <- MC[[2]]
        CLA <- lapply(RES, class)
        z   <- if (length(RES) == 1L) {
            mconvert(CLA[[1]], RES[[1]])
        } else {
            unlist(mapply(mconvert, CLA, RES, SIMPLIFY = FALSE), recursive = FALSE) 
        }
    }
return(z)
}

#' @export
#' @rdname cnsc
cnscinfun <- function() {
    MC <- match.call(definition = sys.function(sys.parent(1)),
                            call = sys.call(sys.parent(1)), 
                     expand.dots = FALSE,
                           envir = parent.frame(2L))
    if (length(MC) == 1L) {
        z <- ""       # or z <- NULL
    } else {
        mconvert <- function(cla, res) {
            switch(cla,
            "character" = res,
            "call"      = eval(res), 
            "name"      = if (as.character(res) %in% ls(.GlobalEnv)) {
                              eval(res) } else { as.character(res) },
            "numeric"   = as.character(res),
            "integer"   = as.character(res),
            "complex"   = as.character(res)
            )
        }
        RES <- MC[[2]]
        CLA <- lapply(RES, class)
        z   <- if (length(RES) == 1L) {
            mconvert(CLA[[1]], RES[[1]])
        } else {
            unlist(mapply(mconvert, CLA, RES, SIMPLIFY = FALSE), recursive = FALSE) 
        }
    }
return(z)
}

#' @export
#' @rdname cnsc
cnscinfun2 <- function(...) {
    MC <- match.call(definition = sys.function(sys.parent(2)),
                            call = sys.call(sys.parent(2)), 
                     expand.dots = FALSE,
                           envir = parent.frame(3L))
    if (length(MC) == 1L) { 
        z <- ""  
        # or z <- NULL
    } else {
        mconvert <- function(cla, res) {
            switch(cla,
            "character" = res,
            "call"      = eval(res), 
            "name"      = if (as.character(res) %in% ls(.GlobalEnv)) {
                              eval(res) } else { as.character(res) },
            "numeric"   = as.character(res),
            "integer"   = as.character(res),
            "complex"   = as.character(res)
            )
        }
        RES <- MC[[2]]
        CLA <- lapply(RES, class)
        z   <- if (length(RES) == 1L) {
            mconvert(CLA[[1]], RES[[1]])
        } else {
            unlist(mapply(mconvert, CLA, RES, SIMPLIFY = FALSE), recursive = FALSE) 
        }
    }
return(z)
}



