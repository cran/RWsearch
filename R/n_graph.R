## n_graph
## @include h_ttp.R


#' @title Graphs of Dependencies
#' @description
#' These two functions display in the default browser a network of the package dependencies 
#' as a standard graph \code{n_graphF} (F for force in networkD3 catalogue) or as a Sankey 
#' graph \code{n_graphS}. The graphs are built (and can be saved) with the \emph{networkD3} 
#' package. 
#' 
#' Remember that the option \code{p_network(exclpkgs = ...} in \code{\link{p_network}}, 
#' whose default value \code{TRUE} is equivalent to 
#' \code{exclpkgs = c("graphics", "grDevices", "methods", "stats", "tools", "utils")}, 
#' can substantially modify the aspect of the graph, especially for \code{reverse = FALSE}. 
#' 
#' @param  netw         a list of class "pkgsnetwork" produced by \code{\link{p_network}}
#'                      that describes with nodes and links the dependencies of one or 
#'                      several packages (a network).
#' @param  group        integer, currently 1, 2 or 3. The suffix of the "NGroup" column 
#'                      in \emph{netw}. Define a scheme for colouring the nodes. 
#' @param  fontFamily   character. Either "serif" or "sans-serif".
#' @param  fontSize     integer. The size of the font. 
#' @param  linkDistance integer. The minimal distance of a link between two nodes. 
#' @param  charge       integer. A repulsive value between two nodes.
#' @param  nodeWidth    integer. The width of the rectangular nodes in the Sankey graph.
#' @param  nodePadding  integer. The vertical space between two nodes in the same column 
#'                      of a Sankey graph.  
#' @examples 
#' \donttest{
#' ## In real life, download crandb from CRAN or load it from your directory 
#' ## with functions crandb_down() or crandb_load(). 
#' ## In this example, we use a small file.
#' crandb_load(system.file("data", "zcrandb.rda", package = "RWsearch"))
#' netw <- p_network(stringr, methods, parallel, stats, utils, reverse = TRUE)
#' n_graphF(netw)
#' n_graphS(netw)
#' 
#' n_graphF(p_network(canprot, FatTailsR, actuar, exclpkgs = FALSE))
#' }
#' @name n_graph
NULL

#' @export
#' @rdname n_graph
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
#' @rdname n_graph
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



