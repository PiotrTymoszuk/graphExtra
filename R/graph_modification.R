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

# extraction of neighborhood ----------

#' Extraction of the neighborhood.
#'
#' @description Functions `neighbor_graph()` and `neighbor_attr()` extract,
#' respectively, the neighborhood of a given vertex as a graph and attributes
#' of edges and nodes  for the neighbors of the given.
#' As such, the functions are handy supplements to
#' \code{\link[igraph]{neighbors}}.
#'
#' @return `neighbor_graph()` returns an `igraph` class object,
#' `neighbor_attr()` returns a data frame with the neighbor indexes,
#' names (if specified), attributes, and weights of the edges between
#' the neighbors and the node of interest.
#'
#' @param x an `igraph` object.
#' @param v index of the vertex of interest or NULL. One of `v` or `name`
#' must be specified. If `v` is specified, `name` is ignored.
#' @param name name of the index of interest or NULL.  One of `v` or `name`
#' must be specified.
#'
#' @export

  neighbor_graph <- function(x, v = NULL, name = NULL) {

    ## entry control --------

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    if(is.null(v) & is.null(name)) {

      stop("At least one of 'v' or 'name' must be specified.", call. = FALSE)


    }

    attr_tbl <- get_vertex_attributes(x)

    all_idx <- attr_tbl[['index']]

    if(!'name' %in% names(attr_tbl) & is.null(v)) {

      stop("'name' attribute absent, please select the vertex by its index.",
           call. = FALSE)

    }

    if(is.null(v)) {

      v <- attr_tbl[['index']][attr_tbl[['name']] == name]

    }

    ## selection of the neighbor indexes and pruning--------

    idx <- c(v, as.integer(neighbors(x, v)))

    del_idx <- all_idx[!all_idx %in% idx]

    delete_vertices(x, del_idx)

  }

#' @rdname neighbor_graph
#' @export

  neighbor_attr <- function(x, v = NULL, name = NULL) {

    ## entry control --------

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    if(is.null(v) & is.null(name)) {

      stop("At least one of 'v' or 'name' must be specified.", call. = FALSE)


    }

    attr_tbl <- get_vertex_attributes(x)

    all_idx <- attr_tbl[['index']]

    if(!'name' %in% names(attr_tbl) & is.null(v)) {

      stop("'name' attribute absent, please select the vertex by its index.",
           call. = FALSE)

    }

    if(is.null(v)) {

      v <- attr_tbl[['index']][attr_tbl[['name']] == name]

    }

    index <- NULL

    ## selection of the neighbor indexes and edge weights--------

    idx <- c(v, as.integer(neighbors(x, v)))

    attr_tbl <- filter(attr_tbl, index %in% idx)

    edge_wt <- x[v, idx[-1]]

    edge_wt <- tibble(index = idx[-1],
                      weight = edge_wt)

    left_join(attr_tbl, edge_wt, by = 'index')

  }

# END -------
