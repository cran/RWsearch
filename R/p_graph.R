## n_graph
## @include p_down.R


#' @title Network and Graphs of Package Dependencies
#' @description
#' \code{p_graphF} calculates the (recursive/reverse) dependencies of a (vector   
#' of) package(s) and displays in the default browser a standard graph (F/Force
#' in the networkD3 terminology) of the package dependencies. It combines the
#' \code{p_network} and \code{n_graphF} functions.  
#' 
#' \code{p_graphF} calculates the (recursive/reverse) dependencies of a (vector   
#' of) package(s) and displays in the default browser a Sankey graph 
#' (in the networkD3 terminology) of the package dependencies. It combines the
#' \code{p_network} and \code{n_graphS} functions.
#' 
#' \code{p_network} returns the (recursive/reverse) dependencies of a (vector   
#' of) package(s) as a network of nodes and links. 
#' 
#' \code{n_graphF} taks as input a network of package nodes and links and displays   
#' them in the default browser as a standard graph (F/Force in the networkD3 
#' terminology) representing the package dependencies. 
#' 
#' \code{n_graphF} taks as input a network of package nodes and links and displays   
#' them in the default browser as a Sankey graph (in the networkD3 terminology) 
#' representing the package dependencies. 
#' 
#' Remember that the option \code{exclpkgs = ...} whose default value \code{TRUE} 
#' is equivalent to 
#' \code{exclpkgs = c("graphics", "grDevices", "methods", "stats", "tools", "utils")}, 
#' can substantially modify the aspect of the graph, especially for \code{reverse = FALSE}. 
#' 
#' @param  ...          any format recognized by \code{\link{cnsc}}. Lists are accepted
#'                      for \code{p_graphF} and \code{p_graphS} (and will result in 
#'                      multiple html pages) but not in \code{p_network}.
#'                      A vector or a list of package(s) listed in \code{crandb} or in
#'                      \code{installed.packages()}.
#' @param  char         (name to) a character vector. Use this argument if 
#'                      \code{...} fails or if you call the function from another function. 
#'                      If used, argument \code{...} is ignored.  
#' @param  which        character vector. A sub-vector of 
#'                      \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}. 
#'                      The short forms "D", "I", "L", "S", "N", "DL", "DI", "DIL", "DILS", 
#'                      "DILN", "DILSN", "SN" are accepted. "N" is for "Enhances" as the 
#'                      single letter "E" is used by R as a shortcut to EXPR, a reserved word.
#' @param  recursive    logical. Search for (reverse) dependencies of (reverse) dependencies.
#' @param  reverse      logical. Search for reverse dependencies.
#' @param  exclpkgs     logical or character vector. \code{TRUE} excludes 
#'                      from the network of nodes and links the dependencies
#'                      \code{c("graphics", "grDevices", "methods", "stats", "tools", "utils")}.
#'                      \code{FALSE} includes them. 
#'                      You can provide your own vector of packages to exclude them 
#'                      from the network of nodes and links, for instance:
#'                      \code{exclpkgs = c("ggplot2", list.files(.Library))}.
#' @param  netw         a list of class "pkgsnetwork" produced by \code{\link{p_network}}
#'                      that describes the dependencies of one or several packages 
#'                      with nodes and links (a network).
#' @param  group        integer, currently 1, 2 or 3. The suffix of the "NGroup" column 
#'                      in \emph{netw}. Define a scheme for colouring the nodes. 
#' @param  fontFamily   character. Either "serif" or "sans-serif".
#' @param  fontSize     integer. The size of the font. 
#' @param  linkDistance integer. The minimal distance of a link between two nodes. 
#' @param  charge       integer. A repulsive value between two nodes.
#' @param  nodeWidth    integer. The width of the rectangular nodes in the Sankey graph.
#' @param  nodePadding  integer. The vertical space between two nodes in the same column 
#'                      of a Sankey graph. 
#' @param  crandb       data.frame \code{crandb}. Also accepted is \code{NULL} which will 
#'                      search in the local \code{installed.packages()}. This later form
#'                      allows (private) packages that are not listed in \code{crandb}. 
#' @examples 
#' \donttest{
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' lst <- as.list(cnsc(RWsearch, visNetwork)) ; lst
#' p_graphF(lst)
#' 
#' p_graphF(RWsearch, visNetwork)
#' p_graphS(RWsearch, visNetwork)
#' p_graphF(actuar, fitdistrplus, reverse = TRUE)
#' 
#' netw <- p_network(actuar, fitdistrplus, reverse = TRUE) ; netw
#' n_graphF(netw)
#' n_graphS(netw)
#' }
#' @name p_graph
NULL



#' @export
#' @rdname p_graph
p_graphF <- function(..., char = NULL, which = "DIL", recursive = TRUE, 
                     reverse = FALSE, exclpkgs = TRUE, 
                     group = 2, fontFamily = "serif", fontSize = 11, 
                     linkDistance = 50, charge = -100,
                     crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        mapply(FUN = p_graphF, pkgs, char = pkgs, 
               MoreArgs = list(which = which, recursive = recursive, 
                               reverse = reverse, exclpkgs = exclpkgs, 
                               group = group, fontFamily = fontFamily, 
                               fontSize = fontSize, linkDistance = linkDistance,
                               charge = charge, crandb = crandb), 
               SIMPLIFY = FALSE)
    } else {
        netw <- p_network(char = pkgs,  which = which, recursive = recursive, 
                          reverse = reverse, exclpkgs = exclpkgs, crandb = crandb)
        n_graphF(netw, group = group, fontFamily = fontFamily, fontSize = fontSize, 
                         linkDistance = linkDistance, charge = charge) 
    }
}


#' @export
#' @rdname p_graph
p_graphS <- function(..., char = NULL, which = "DIL", recursive = TRUE, 
                     reverse = FALSE,  exclpkgs = TRUE, 
                     group = 2, fontFamily = "serif", fontSize = 14, 
                     nodeWidth = 30, nodePadding = 10,
                     crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) {
        mapply(FUN = p_graphS, pkgs, char = pkgs, 
               MoreArgs = list(which = which, recursive = recursive, 
                               reverse = reverse, exclpkgs = exclpkgs, 
                               group = group, fontFamily = fontFamily, 
                               fontSize = fontSize, nodeWidth = nodeWidth, 
                               nodePadding = nodePadding, crandb = crandb), 
               SIMPLIFY = FALSE)
    } else {
        netw <- p_network(char = pkgs,  which = which, recursive = recursive, 
                          reverse = reverse, exclpkgs = exclpkgs, crandb = crandb)
        n_graphS(netw, group = group, fontFamily = fontFamily, fontSize = fontSize, 
                          nodeWidth = nodeWidth, nodePadding = nodePadding)  
    }
}

#' @export
#' @rdname p_graph
p_network <- function (..., char = NULL, which = "DIL", recursive = TRUE, 
                       reverse = FALSE, exclpkgs = TRUE, 
                       crandb = get("crandb", envir = .GlobalEnv)) {
    if (!is.data.frame(crandb)) stop("crandb is not loaded.")
    pkgs <- if (is.null(char)) cnscinfun() else char
    if (is.list(pkgs)) stop("... cannot be a list.")
    
    ### MANAGING EXCLUSIONS
    if (is.logical(exclpkgs)) {
        exclpkgs <- if (exclpkgs) {
                     c("graphics", "grDevices", "methods", "stats", "tools", "utils")
                   } else { "" }
    }
    pkgs    <- pkgs[!(pkgs %in% exclpkgs)]
    dfrDeps <- data.frame(Source = pkgs, Target = pkgs, stringsAsFactors = FALSE)
    
    ### DEPENDENCIES
    mpkgs <- unique(unlist(
             c(pkgs, p_deps(char = pkgs, which = which, recursive = recursive,
                            reverse = reverse, crandb = crandb))))
    lst   <- p_deps(char = mpkgs, which = which, recursive = FALSE,
                    reverse = reverse, crandb = crandb)
    lst2  <- lst[lengths(lst) != 0]
    if (length(lst2) != 0) {
        lst3 <- list()
        if (reverse) {
            for (i in seq_along(lst2)) lst3[[i]] <- data.frame(
                 Source = names(lst2)[i], Target = lst2[[i]], stringsAsFactors = FALSE)
        } else {
            for (i in seq_along(lst2)) lst3[[i]] <- data.frame(
                 Source = lst2[[i]], Target = names(lst2)[i], stringsAsFactors = FALSE)
        }
        dfr3    <- do.call("rbind", lst3)
        dfrDeps <- rbind(dfrDeps, dfr3)
        
    ### INTERMEDIATE DEPENDENCIES v4.5.9
        dfrDeps <- dfrDeps[!(dfrDeps$Source %in% exclpkgs),, drop=FALSE]
        dfrDeps <- dfrDeps[!(dfrDeps$Target %in% exclpkgs),, drop=FALSE]
        if (reverse) {
            while (!all(dfrDeps$Source %in% dfrDeps$Target)) {
                dfrDeps  <- dfrDeps[dfrDeps$Source %in% dfrDeps$Target,, drop=FALSE]
            }        
        } else {
            while (!all(dfrDeps$Target %in% dfrDeps$Source)) {
                dfrDeps  <- dfrDeps[dfrDeps$Target %in% dfrDeps$Source,, drop=FALSE]
            }
        }
    }
    
    ### NODES
    dfr4     <- if (reverse) { dfrDeps[!duplicated(dfrDeps[,"Target"]),] 
                      } else { dfrDeps[!duplicated(dfrDeps[,"Source"]),] }
    colnames(dfr4) <- if (reverse) { c("NodeGroup", "NodeName") 
                            } else { c("NodeName", "NodeGroup") }
    rownames(dfr4) <- NULL
    dfr4     <- transform(dfr4, NID = as.integer(seq_len(nrow(dfr4))-1))
    NSize    <-  1 + 49*(dfr4$NodeName %in% pkgs)  
    NGroup1  <- -1 + sapply(sapply(dfr4$NodeGroup, grep, dfr4$NodeName, 
                                   fixed = TRUE), function(x) x[1])
    NGroup2  <- sapply(NGroup1, function(x, y) ifelse(x >= y, y, x), y = length(pkgs))
    NGroup3  <- sapply(pmatch(dfr4$NodeName, dfr4$NodeGroup, nomatch = 0), 
                       min, length(pkgs)+1) 
    NGroup3  <- sapply(NGroup3-1, function(x, y) ifelse(x < 0, y, x), y = max(NGroup3))
    NCol1    <- "#999999"
    # names(NGroup1) <- names(NGroup2) <- names(NGroup3) <- names(NCol1) <- NULL
    dfrNodes <- data.frame(dfr4, NSize, NGroup1, NGroup2, NGroup3, 
                           NCol1, stringsAsFactors = FALSE)
    rownames(dfrNodes) <- NULL

    ### LINKS
    LSource  <- -1 + sapply(sapply(dfrDeps$Source, grep, dfrNodes$NodeName, 
                                   fixed = TRUE), function(x) x[1])
    LTarget  <- -1 + sapply(sapply(dfrDeps$Target, grep, dfrNodes$NodeName, 
                                   fixed = TRUE), function(x) x[1])
    LValue   <-  1 + if (reverse) { 0*(LSource < length(pkgs)) 
                           } else { 4*(LTarget < length(pkgs)) }
    LCol1    <- "#555555"
    SValue   <-  1
    dfrLinks <- data.frame(dfrDeps, LSource, LTarget, LValue, LCol1, 
                           SValue, stringsAsFactors = FALSE)
    rownames(dfrLinks) <- NULL

    ### LIST
    netw <- list(pkgs0 = pkgs, recursive = recursive, reverse = reverse, 
                 dfrNodes = dfrNodes, dfrLinks = dfrLinks)
    class(netw) <- c("pkgsnetwork", "list")
netw
}

#' @export
#' @rdname p_graph
n_graphF <- function(netw, group = 2, fontFamily = "serif", fontSize = 11, 
                     linkDistance = 50, charge = -100) {
    stopifnot(inherits(netw, "pkgsnetwork"))
    rpkgs0 <- seq_along(netw$pkgs0)
    networkD3::forceNetwork(
        Links = netw$dfrLinks[-rpkgs0,], Nodes = netw$dfrNodes, 
        Source = "LSource", Target = "LTarget", Value = "LValue", 
        NodeID = "NodeName", Nodesize = "NSize", 
        Group = paste0("NGroup", group), 
        linkColour = netw$dfrLinks[-rpkgs0, "LCol1"],
        # linkColour = "LCol1",
        fontFamily = fontFamily, fontSize = fontSize, 
        linkDistance = linkDistance, charge = charge,
        opacity = 1, zoom = TRUE, arrows = TRUE, opacityNoHover = 1)
}

#' @export
#' @rdname p_graph
n_graphS <- function(netw, group = 2, fontFamily = "serif", fontSize = 14, 
                     nodeWidth = 30, nodePadding = 10) {
    stopifnot(inherits(netw, "pkgsnetwork"))
    rpkgs0 <- seq_along(netw$pkgs0)
    networkD3::sankeyNetwork(
        Links = netw$dfrLinks[-rpkgs0,], Nodes = netw$dfrNodes, 
        Source = "LSource", Target = "LTarget", Value = "SValue", 
        NodeID = "NodeName", 
        # NodeGroup = "NCol1", LinkGroup = "LCol1", units = "", 
        fontSize = fontSize, fontFamily = fontFamily, 
        nodeWidth = nodeWidth, nodePadding = nodePadding, 
        margin = NULL, height = NULL, width = NULL, 
        iterations = 32, sinksRight = TRUE)
}



## VOIR package dodgr Distances on Directed Graphs



