# Node importance statistics and graph size

# Node importance stats --------

#' Summary of vertex importance statistics.
#'
#' @description
#' Computes a bunch of node importance statistics for vertices of a graph.
#'
#' @details
#' The statistics are degree (number of edges projecting from a vertex),
#' betweenness (number of shortest paths between vertex pairs passing through
#' the vertex of interest), hub score (eigenvector of the similarity matrix),
#' and transitivity (clustering tendency of a graph or a vertex).
#' The function is a handy wrapper around \code{\link[igraph]{degree}},
#' \code{\link[igraph]{betweenness}}, \code{\link[igraph]{hub_score}},
#' and \code{\link[igraph]{transitivity}}.
#'
#' @return a tibble with statistics specified in Details.
#'
#' @param object an `igraph` object.
#' @param transitivity_type type of transitivity to be calculated,
#' see \code{\link[igraph]{transitivity}} for details.
#' @param ... extra arguments passed to methods, currently none.
#'
#' @export

  summary.igraph <- function(object,
                             transitivity_type = c('local', 'global', 'weighted'),
                             ...) {

    stopifnot(inherits(object, 'igraph'))

    transitivity_type <- match.arg(transitivity_type[1],
                                   c('local', 'global', 'weighted'))

    ## vertex index and name

    attr_tbl <- get_vertex_attributes(object)

    attr_tbl <- attr_tbl[names(attr_tbl) %in% c('index', 'name')]

    ## node stats

    attr_tbl[['degree']] <- degree(object)
    attr_tbl[['betweenness']] <- betweenness(object)
    attr_tbl[['hub_score']] <- hub_score(object)$vector
    attr_tbl[['transitivity']] <-
      transitivity(object, type = transitivity_type)

    attr_tbl

  }

# Graph dimensions -----

#' Numbers of vertices and edges.
#'
#' @description
#' Extracts numbers of vertices and edges from an `igraph` object.
#'
#' @return a numeric vector with vertex and edge number.
#'
#' @param x an `igraph` object.
#'
#' @export

  dimensions <- function(x) {

    c(vertices = length(V(x)),
      edges = length(E(x)))

  }

# END -----
