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
#' the vertex of interest), hub score (eigenvector of the similarity matrix).
#'
#' @return a tibble with statistics specified in Details.
#'
#' @param object an `igraph` object.
#' @param ... extra arguments passed to methods, currently none.
#'
#' @export

  summary.igraph <- function(object, ...) {

    stopifnot(inherits(object, 'igraph'))

    ## vertex index and name

    attr_tbl <- get_vertex_attributes(object)

    attr_tbl <- attr_tbl[names(attr_tbl) %in% c('index', 'name')]

    ## node stats

    attr_tbl[['degree']] <- degree(object)
    attr_tbl[['betweenness']] <- betweenness(object)
    attr_tbl[['hub_score']] <- hub_score(object)$vector

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
