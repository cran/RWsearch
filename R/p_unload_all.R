## p_unload_all
## @include p_text2pdf.R


#' @title Unload all non-base and non-recommended packages from the namespace
#' @description
#' \code{p_unload_all} unloads in a safe order all packages, except the base and
#' recommended packages, that are attached or loaded in the namespace, plus their
#' respective DLLs.
#'
#' It must be used before installing new versions of packages that are currently
#' loaded in the namespace and require some forced unloading, as revealed by the
#' first column of the data.frame produced by \code{\link{p_vers_deps}}. Warning:
#' this function also removes RWsearch and its dependencies. It is therefore
#' recommended to perform the installation with \code{\link{install.packages}}
#' rather than \code{\link{p_inst}}.
#'
#' This function is safe enough to reinstall locked packages but not safe enough
#' for a further use as it does not unload the S3 and S4 classes nor the documentation
#' which is detected as corrupted. It is therefore recommend to stop and restart R
#' after the installation of the fresh packages.
#'
#' @param  unload    logical. \code{FALSE} prints a list with 3 items: the loaded
#'                   packages, the packages in the namespace, the loaded DLLs.
#'                   \code{TRUE} removes the packages and their DLLs, then prints
#'                   the refreshed list.
#' @param  crandb    data.frame \code{crandb}.
#'
#' @examples
#' ## In real life, download crandb from CRAN or load it from your directory
#' ## with functions crandb_down() or crandb_load().
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#'
#' p_unload_all()
#' ## Then run p_unload_all(TRUE), but only for maintenance!
#'
#' @export
#' @name p_unload_all
p_unload_all <- function(unload = FALSE,
                         crandb = get("crandb", envir = .GlobalEnv)) {
    if (unload == TRUE) {
        dfr <- p_vers_deps(char = loadedNamespaces(), crandb = crandb)
        dfr <- dfr[dfr$nsloaded == TRUE, ]
        for (pkg in rev(rownames(dfr))) unloadNamespace(pkg)
        pkgs  <- sapply(.dynLibs(), function(lst) lst[[1]])
        dlls  <- sapply(.dynLibs(), function(lst) lst[[2]])
        pkgs2 <- pkgs[!(pkgs %in% list.files(.Library))]
        dlls2 <- dlls[!(pkgs %in% list.files(.Library))]
        for (i in rev(seq_along(pkgs2))) {
            rgr  <- grepRaw(pkgs2[i], dlls2[i])
            nch  <- nchar(pkgs2[i])
            path <- substr(dlls2[i], 1, -1 +rgr +nch)
            library.dynam.unload(pkgs2[i], libpath = path)
        }
    }
    list(search = search(),
         loadedNamespaces = loadedNamespaces(),
         dynLibs = .dynLibs())
}




