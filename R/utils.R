# Non-exported utilities

# NA removal from a symmetric matrix ---------

#' Row/column-wise removal of observations from a symmetric matrix.
#'
#' @description
#' Removes rows and columns of a symmetric matrix with at least one NA value.
#'
#' @param x a symmetric matrix.
#'
#' @return a matrix.

  mtx_na_remove <- function(x) {

    stopifnot(is.matrix(x))

    ## removal of columns/rows with NA values at the diagonal

    complete_diag_idx <- !is.na(diag(x))

    x <- x[complete_diag_idx, complete_diag_idx]

    ## removal of columns/rows with NA values only in non-diagonal fields

    all_complete_rows <-
      !map_lgl(1:nrow(x), ~all(is.na(x[.x, -.x])))

    x <- x[all_complete_rows, all_complete_rows]

    ## removal of columns/rows with single NA values

    any_complete_rows <-
      !map_lgl(1:nrow(x), ~any(is.na(x[.x, -.x])))

    x[any_complete_rows, any_complete_rows]

  }

# Similarity matrix to a graph -------

#' Graph from a similarity matrix.
#'
#' @description
#' Builds an `igraph` object at the top of a similarity matrix.
#' Internally, the function uses \code{\link[igraph]{graph_from_adjacency_matrix}}.
#'
#' @details
#' The `na_action` argument specifies, how `NA` values in the similarity matrix
#' are handled.
#' The default setting `na_action = "ignore"` determines that no action is taken,
#' and if there are any `NA` values the function stops and returns an error.
#' If `na_action = "pad"`, all `NA` values in the similatity matrix are replaced
#' with the numeric value provided with the `na_pad_value` argument.
#' Finally, if `na_action = "remove"`, rows and columns of the similarity
#' matrix with at least one `NA` are removed. '
#'
#' @return
#' a non-directional graph object of class `igraph`
#'
#' @param simil_mtx a symmetric similarity matrix.
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
#' @param na_action specifies how `NA` values are handled, see __Details__.
#' Defaults to `"ignore"`.
#' @param na_pad_value a numeric value used to replace all `NA` values in the
#' similarity matrix. Used only when `na_action = "pad"`.
#' @param ... extra arguments passed to \code{\link[igraph]{graph_from_adjacency_matrix}}

  simil2graph <- function(simil_mtx,
                          cutoff = NULL,
                          weighted = TRUE,
                          diag = FALSE,
                          na_action = c("ignore", "pad", "remove"),
                          na_pad_value = 0, ...) {

    ## basic input control ------

    stopifnot(is.matrix(simil_mtx))

    if(!isSymmetric(simil_mtx)) {

      stop("The matrix is not symmetric.", call. = FALSE)

    }

    na_action <- match.arg(na_action[1], c("ignore", "pad", "remove"))

    if(na_action == "pad" & !is.numeric(na_pad_value)) {

      stop("'na_pad_value' has to be a numeric value.", call. = FALSE)

    }

    na_pad_value <- na_pad_value[1]

    ## handling of missing values ---------

    na_check <- sum(is.na(simil_mtx))

    if(na_check > 0) {

      if(na_action == "ignore") {

        stop("There are NA values in the similarity matrix.", call. = FALSE)

      } else if(na_action == "pad") {

        warning(paste("There are",
                      na_check,
                      "NA values in the similarity matrix.",
                      "They are padded with", na_pad_value),
                call. = FALSE)

        simil_mtx <- ifelse(is.na(simil_mtx), na_pad_value, simil_mtx)

      } else {

        nrow_init <- nrow(simil_mtx)

        simil_mtx <- mtx_na_remove(simil_mtx)

        if(nrow_init - nrow(simil_mtx) == nrow_init) {

          stop("No complete rows/column in the similarity matrix.",
               call. = FALSE)

        }

        warning(paste("There are",
                      na_check,
                      "NA values in the similarity matrix.",
                      "They amount to", nrow_init - nrow(simil_mtx),
                      "incomplete rows, which will be removed."),
                call. = FALSE)

      }

    }

    ## handling of negative values and the cutoff --------

    negative_check <- sum(simil_mtx < 0, na.rm = TRUE)

    if(negative_check > 0) {

      message(paste("There are",
                    negative_check,
                    "negative values in the similarity matrix."))

    }

    if(!is.null(cutoff)) {

      below_cutoff <- sum(simil_mtx < cutoff)

      message(paste("There are",
                    below_cutoff,
                    "values below the cutoff in the similarity matrix."))

      simil_mtx <- ifelse(simil_mtx < cutoff, 0, simil_mtx)

    }

    ## graph object ---------

    graph_from_adjacency_matrix(adjmatrix = simil_mtx,
                                mode = 'undirected',
                                weighted = weighted,
                                diag = diag, ...)


  }

# END --------
