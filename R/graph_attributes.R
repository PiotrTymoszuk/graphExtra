# Getting and setting vertex attributes by vertex name

# Getting vertex attributes -------

#' Retrieve vertex attributes.
#'
#' @description
#' Extracts all vertex attributes in a handy data frame. The first two columns
#' specify the vertex index and name.
#'
#' @return a tibble with vertex attributes.
#'
#' @param x an `igraph` object.
#'
#' @export

  get_vertex_attributes <- function(x) {

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    attr_tbl <- as_tibble(vertex_attr(x))

    attr_tbl[['index']] <- 1:nrow(attr_tbl)

    index <- NULL
    name <- NULL

    if('name' %in% names(attr_tbl)) {

      attr_tbl <- relocate(attr_tbl, index, name)

    } else {

      attr_tbl <- relocate(attr_tbl, index)

    }

    attr_tbl

  }

# Setting vertex attributes by vertex name -------

#' Set vertex attributes.
#'
#' @description
#' Sets vertex attributes given a data frame by vertex names or indices.
#'
#' @return an `igraph` object.
#'
#' @param x an `igraph` object.
#' @param lexicon a data frame that contains vertex identifiers in the first
#' column and the vertex attributes to be specified in the remaining columns.
#' The new attributes will be named after the corresponding `lexicon` columns.
#' @param index_type type of the index used for assignment of the attributes:
#' names (default) of indexes.
#'
#' @export

  set_vertex_attributes <- function(x,
                                    lexicon,
                                    index_type = c('name', 'index')) {

    ## input control -------

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    if(!is.data.frame(lexicon)) {

      stop("'lexicon' has to be a data frame.", call. = FALSE)

    }

    if(ncol(lexicon) < 2) {

      stop("'lexicon' has to have at least two columns.", call. = FALSE)

    }

    index_type <- match.arg(index_type[1], c('name', 'index'))

    ## setting the attributes -------

    id_column <- lexicon[[1]]

    attr_names <- names(lexicon)[-1]

    if(index_type == 'index') {

      if(!is.numeric(id_column)) {

        stop(paste("The first column of 'lexicon' with vertex",
                   "indexes has to be numeric."),
             call. = FALSE)

      }

      id_column <- as.integer(id_column)

      for(i in attr_names) {

        x <- set_vertex_attr(x,
                             name = i,
                             index = id_column,
                             value = lexicon[[i]])

      }

    } else {

      vertex_names <- names(V(x))

      for(i in attr_names) {

        naming_vector <- set_names(lexicon[[i]], id_column)

        attribute_values <- naming_vector[vertex_names]

        x <- set_vertex_attr(x,
                             name = i,
                             value = unname(attribute_values))

      }

    }

    x

  }

# END -------
