# Generics and methods for generation of `igrph` objects from a range of
# similarity and distance inputs.

#' Create `igraph` objects.
#'
#' @description
#' The generic function `as_iGraph()` takes a range of similarity, distance,
#' matrix, or data frame objects to generate `igraph` class graph objects based
#' on similarity or correlation between the features.
#'
#' @details
#' The function internally uses
#' \code{\link[igraph]{graph_from_adjacency_matrix}}.
#' The user can specify a similarity or correlation cutoff to select pairwise
#' associations of particular strength which will be used for the
#' graph construction.
#' If the function is provided with a numeric data frame or matrix, the
#' similarity between columns or rows will be calculated with an user-provided
#' function `fun` (\code{\link[stats]{cor}} by default).
#' For a matrix input, the user can specify if it should be treated as a
#' numeric, data-storing matrix or a similarity matrix.
#' If `dist` class objects are provided, they will be converted to similarity
#' matrices by a user-provided function `fun` (`1 - x` by default).
#' Any `NA` or negative values in the similarity matrix after application of the
#' similarity cutoff will raise warnings, any generation of the `igraph` object
#' may fail.
#' For matrices and data frames, row and column names are mandatory.
#'
#' @return a non-directional object of class `igraph`,
#' which may be processed, evaluated and visualized by tools of `igraph` package.
#' The methods \code{\link{summary.igraph}} and `plot()` which
#' allow for, respectively, computation of network node statistics and
#' visualization, are overwritten and allow for a richer output
#' and customization.
#'
#' @param x an object: a numeric matrix, numeric data frame or a `dist`
#' class object.
#' @param input_type specifies if a matrix should be treated as a data-storing
#' matrix (`input_type = 'data'`, default) or
#' a similarity matrix (`input_type = 'similarity'`).
#' @param feature_type specifies if features, i.e. vertices of the graph are
#' located in the rows or columns. Ignored for `dist` objects and
#' similarity matrices.
#' @param fun a function used to calculate similarity between features of a data
#' frame or matrix (\code{\link[stats]{cor}} by default) or to convert distances
#' to similarities (`1 - x` by default). Ignored for `dist` objects and
#' similarity matrices.
#' @param cutoff a numeric value that defines the minimal similarity between
#' features. Only feature pairs with similarity larger or equal to `cutoff` will
#' become the graph's edges. If `NULL` (default), the all pairwise similarities
#' are used.
#' @param weighted a logical or character that specifies if and how the graph's
#' edges should be weighted by similarity between the features. Please consult
#' \code{\link[igraph]{graph_from_adjacency_matrix}} for details.
#' @param diag a logical that specifies if the diagonal of the similarity matrix
#' should be included the graph object. In most cases this results in
#' self-connecting edges. Defaults to `FALSE`.
#'
#' @param ... extra arguments passed to the methods or to the `fun` function.
#'
#' @export

  as_iGraph <- function(x, ...)  UseMethod('as_iGraph')

#' @rdname as_iGraph
#' @export as_iGraph.matrix
#' @export

  as_iGraph.matrix <- function(x,
                               input_type = c('data', 'similarity'),
                               feature_type = c('columns', 'rows'),
                               fun = stats::cor,
                               cutoff = NULL,
                               weighted = TRUE,
                               diag = FALSE, ...) {

    ## entry control -------

    err_txt <-
      "'x' has to be a numeric matrix with row and column names."

    if(!is.matrix(x)) stop(err_txt, call. = FALSE)

    if(!is.numeric(x)) stop(err_txt, call. = FALSE)

    if(is.null(rownames(x))) stop(err_txt, call. = FALSE)

    if(is.null(colnames(x))) stop(err_txt, call. = FALSE)

    input_type <- match.arg(input_type[1], c('data', 'similarity'))

    feature_type <- match.arg(feature_type[1], c('columns', 'rows'))

    if(!is.function(fun)) stop("'fun' has to be a function.", call. = FALSE)

    if(!is.null(cutoff)) {

      if(!is.numeric(cutoff)) {

        stop("'cutoff' has to be numeric.", call. = FALSE)

      }

      cutoff <- cutoff[1]

    }

    stopifnot(is.logical(diag))

    ## similarity matrix and its checks --------

    if(input_type == 'similarity') {

      dims <- dim(x)

      if(dims[[1]] != dims[[2]]) {

        stop("The similatity matrix 'x' is not square.", call. = FALSE)

      }

      name_initersect <- intersect(colnames(x), rownames(x))

      if(length(name_initersect) != dims[[1]]) {

        stop("Column and row names of teh similarity matrix 'x' must match.",
             call. = FALSE)

      }

      simil_mtx <- x

    } else {

      if(feature_type == 'rows') x <- t(x)

      simil_fun <- function(x) fun(x, ...)

      simil_mtx <- simil_fun(x)

    }

    if(!is.null(cutoff)) {

      simil_mtx <- ifelse(simil_mtx < cutoff, 0, simil_mtx)

    }

    na_check <- sum(is.na(simil_mtx))

    if(na_check > 0) {

      warning(paste("There are",
                    na_check,
                    "NA values in the similarity matrix."),
              call. = FALSE)

    }

    negative_check <- sum(simil_mtx < 0, na.rm = TRUE)

    if(negative_check > 0) {

      warning(paste("There are",
                    na_check,
                    "negative values in the similarity matrix."),
              call. = FALSE)

    }

    ## the output ---------

    graph_from_adjacency_matrix(adjmatrix = simil_mtx,
                                mode = 'undirected',
                                weighted = weighted,
                                diag = diag)

  }

#' @rdname as_iGraph
#' @export as_iGraph.data.frame
#' @export

  as_iGraph.data.frame <- function(x,
                                   feature_type = c('columns', 'rows'),
                                   fun = stats::cor,
                                   cutoff = NULL,
                                   weighted = TRUE,
                                   diag = FALSE, ...) {

    ## input control --------

    err_txt <- "'x' has to be a numeric data frame with row and column names."

    if(!is.data.frame(x)) stop(err_txt, call. = FALSE)

    if(is.null(rownames(x))) stop(err_txt, call. = FALSE)

    if(is.null(colnames(x))) stop(err_txt, call. = FALSE)

    numeric_check <- map_lgl(x, is.numeric)

    if(any(!numeric_check)) stop(err_txt, call. = FALSE)

    feature_type <- match.arg(feature_type[1], c('columns', 'rows'))

    if(!is.function(fun)) stop("'fun' has to be a function.", call. = FALSE)

    if(!is.null(cutoff)) {

      if(!is.numeric(cutoff)) {

        stop("'cutoff' has to be numeric.", call. = FALSE)

      }

      cutoff <- cutoff[1]

    }

    stopifnot(is.logical(diag))

    ## similarity matrix --------

    simil_fun <- function(x) fun(x, ...)

    if(feature_type == 'rows') {

      x <- t(x)

    } else {

      x <- as.matrix(x)

    }

    simil_mtx <- simil_fun(x)

    if(!is.null(cutoff)) {

      simil_mtx <- ifelse(simil_mtx < cutoff, 0, simil_mtx)

    }

    na_check <- sum(is.na(simil_mtx))

    if(na_check > 0) {

      warning(paste("There are",
                    na_check,
                    "NA values in the similarity matrix."),
              call. = FALSE)

    }

    negative_check <- sum(simil_mtx < 0, na.rm = TRUE)

    if(negative_check > 0) {

      warning(paste("There are",
                    na_check,
                    "negative values in the similarity matrix."),
              call. = FALSE)

    }

    ## the output ---------

    graph_from_adjacency_matrix(adjmatrix = simil_mtx,
                                mode = 'undirected',
                                weighted = weighted,
                                diag = diag)

  }

#' @rdname as_iGraph
#' @export as_iGraph.dist
#' @export

  as_iGraph.dist <- function(x,
                             fun = function(x) x - 1,
                             cutoff = NULL,
                             weighted = TRUE,
                             diag = FALSE, ...) {

    ## input control -------

    if(!inherits(x, 'dist')) {

      stop("'x' has to be a 'dist' object.", call. = FALSE)

    }

    if(!is.function(fun)) stop("'fun' has to be a function.", call. = FALSE)

    if(!is.null(cutoff)) {

      if(!is.numeric(cutoff)) {

        stop("'cutoff' has to be numeric.", call. = FALSE)

      }

      cutoff <- cutoff[1]

    }

    stopifnot(is.logical(diag))

    ## similarity matrix --------

    x <- as.matrix(x)

    dims <- dim(x)

    if(is.null(rownames(x))) {

      rownames(x) <- paste0('observation_', 1:dims[[1]])

    }

    if(is.null(colnames(x))) {

      colnames(x) <- paste0('observation_', 1:dims[[2]])

    }

    simil_fun <- function(x) fun(x, ...)

    simil_mtx <- simil_fun(x)

    if(!is.null(cutoff)) {

      simil_mtx <- ifelse(simil_mtx < cutoff, 0, simil_mtx)

    }

    na_check <- sum(is.na(simil_mtx))

    if(na_check > 0) {

      warning(paste("There are",
                    na_check,
                    "NA values in the similarity matrix."),
              call. = FALSE)

    }

    negative_check <- sum(simil_mtx < 0, na.rm = TRUE)

    if(negative_check > 0) {

      warning(paste("There are",
                    na_check,
                    "negative values in the similarity matrix."),
              call. = FALSE)

    }

    ## the output ---------

    graph_from_adjacency_matrix(adjmatrix = simil_mtx,
                                mode = 'undirected',
                                weighted = weighted,
                                diag = diag)

  }

# END ---------
