# Functions used for modification of the graph

# deleting low degree nodes -------

#' Delete low degree vertices.
#'
#' @description
#' The function `prune_degree()` removes vertices of the graph with degrees
#' equal or lower than a cutoff.
#'
#' @details
#' Internally, the function uses \code{\link[igraph]{delete_vertices}}.
#'
#' @param x a `igraph` class object.
#' @param cutoff degree cutoff. Vertices with degree lower or equal to `cutoff`
#' will be removed.
#'
#' @return an `igraph` object.
#'
#' @export

  prune_degree <- function(x, cutoff = 0) {

    ## input control -------

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    if(!is.numeric(cutoff)) {

      stop("'cutoff' has to be numeric.", call. = FALSE)

    }

    cutoff <- cutoff[1]

    ## pruning ------

    del_idx <- which(degree(x) <= cutoff)

    delete_vertices(x, del_idx)

  }

# deleting and selecting nodes by attributes -------

#' Delete or select graph nodes with logical expression applied to attributes.
#'
#' @description
#' The functions `prune_vertices()` and `select_vertices()` work in a manner
#' that resembles \code{\link[dplyr]{filter}} from `dplyr` package: the nodes
#' to be deleted (`prune_vertices()`) or kept (`select_vertices()`) are selected
#' by one or more logical operations on unquoted vertex attribute names.
#'
#' @return an `igraph` object.
#'
#' @param x a `igraph` class object.
#' @param ... one or more logical expressions to select vertices
#' by their attributes.
#'
#' @export

  prune_vertices <- function(x, ...) {

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    ## selection of vertices -------

    attr_tbl <- get_vertex_attributes(x)

    attr_tbl <- filter(attr_tbl, ...)

    idx <- attr_tbl$index

    ## pruning ---------

    delete_vertices(x, idx)

  }

#' @rdname prune_vertices
#' @export

  select_vertices <- function(x, ...) {

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    ## selection of vertices -------

    attr_tbl <- get_vertex_attributes(x)

    select_tbl <- filter(attr_tbl, ...)

    keep_idx <- select_tbl$index

    all_idx <- attr_tbl$index

    idx <- all_idx[!all_idx %in% keep_idx]

    ## pruning ---------

    delete_vertices(x, idx)

  }

# splitting into subgraphs by levels of a node attribute ------

#' Split a graph into subgraphs by levels of a vertex attribute.
#'
#' @description
#' The function splits a graph into subgraphs by factors of a vertex attribute
#' or a combination of vertex attributes.
#'
#' @return a list of `igraph` objects.
#'
#' @param x an `igraph` object.
#' @param ... one or more unquoted attributes used for splitting.
#' @param .drop logical, should empty levels of the vector be skipped from the
#' output? Defaults to TRUE.
#'
#' @export

  split_vertices <- function(x, ..., .drop = TRUE) {

    ## entry control --------

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    stopifnot(is.logical(.drop))

    ## selection data frame --------

    attr_tbl <- get_vertex_attributes(x)

    sel_frame <- select(attr_tbl, ...)

    split_vec <- interaction(as.list(sel_frame), drop = .drop)

    ## splitting the attribute data frame -------

    attr_split <- split(attr_tbl, f = split_vec, drop = .drop)

    all_idx <- attr_tbl$index

    split_idx <- map(attr_split, ~.x$index)

    del_idx <- map(split_idx, ~all_idx[!all_idx %in% .x])

    ## splitting the graph --------

    map(del_idx, delete_vertices, graph = x)

  }

# END -------
