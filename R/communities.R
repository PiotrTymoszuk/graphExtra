# Editing and graph-merging from community objects

# Extract community assignment --------

#' Extract assignment of graph vertices to communities.
#'
#' @description
#' Retrieves assignment of graph nodes to communities in a data frame.
#' The first column specifies the vertex index. The remaining columns are
#' name (if present) and community identifier.
#'
#' @details
#' If no community assignment can be retrieved from an `igraph` object,
#' `NULL` is returned with a warning.
#'
#' @return a tibble.
#'
#' @param x an object of class `communities` or `igraph`.
#' @param ... extra arguments passed to methods.
#'
#' @export

  assignment <- function(x, ...) UseMethod('assignment')

#' @rdname assignment
#' @export

  assignment.communities <- function(x, ...) {

    stopifnot(inherits(x, 'communities'))

    members <- x$members

    index <- NULL

    comm_tbl <- tibble(index = 1:length(members))

    if('names' %in% names(x)) {

      comm_tbl[['name']] <- x$names

    }

    comm_tbl[['community_id']] <- members

    comm_tbl

  }

#' @rdname assignment
#' @export

  assignment.igraph <- function(x, ...) {

    stopifnot(inherits(x, 'igraph'))

    attr_tbl <- get_vertex_attributes(x)

    if(!'community_id' %in% names(attr_tbl)) {

      warning('No community assignment was found.', call. = FALSE)

      return(NULL)
    }

    attr_tbl[names(attr_tbl) %in% c('index', 'name', 'community_id')]

  }

# Recoding of community identifiers --------

#' Recode/rename community identifiers.
#'
#' @description
#' Changes identifiers of communities with a vector. The identifiers are stored
#' as factors with the levels specified by names of the re-coding vector.
#'
#' @details
#' The re-coding character vector has a general form
#' `c(new_name_1 = old_name_1, new_name_2 = old_name_1, ...)`.
#' The community identifiers will be specified by a vector with levels
#' `c(new_name_1, new_name_2, ...)`.
#'
#' @return an object of class `communities`.
#'
#' @param x an object of class `communities`.
#' @param new_names the re-coding vector as described in Details.
#'
#' @export

  comm_recode <- function(x, new_names) {

    ## entry control ------

    stopifnot(inherits(x, 'communities'))

    err_txt <- "'new_names' has to be a named character vector."

    if(!is.character(new_names)) stop(err_txt, call. = FALSE)

    if(is.null(names(new_names))) stop(err_txt, call. = FALSE)

    ## recoding --------

    old_ids <- x$membership

    if(!is.factor(old_ids)) {

      old_ids <- factor(old_ids)

    }

    old_levels <- levels(old_ids)
    new_levels <- names(new_names)

    new_ids <- fct_recode(old_ids, !!!new_names)

    new_ids <- factor(new_ids, c(new_levels, old_levels))

    new_ids <- droplevels(new_ids)

    x$membership <- new_ids

    x

  }

# Community size -------

#' Size of communities.
#'
#' @description
#' Extracts size of communities from a `communities` class object.
#'
#' @return a `table`.
#'
#' @param communities a `communities` class object.
#'
#' @export

  sizes <- function(communities) {

    m <- communities$membership

    table(`Community sizes` = m)

  }

# Lumping of small communities ---------

#' Lump small communities together.
#'
#' @description
#' Collapses communities with size lower or equal to a cutoff into a common
#' category (e.g. 'other').
#'
#' @return a `communities` class object.
#'
#' @param x a `communities` class object.
#' @param cutoff community size cutoff.
#' @param other_name name of the common category that groups all
#' small communities.
#'
#' @export

  comm_lump <- function(x, cutoff, other_name = 'other') {

    ## entry control -------

    stopifnot(inherits(x, 'communities'))

    if(!is.numeric(cutoff)) {

      stop("'cutoff' has to be numeric.", call. = FALSE)

    }

    cutoff <- as.integer(cutoff[1])

    if(!is.character(other_name)) {

      stop("'other_name' has to be character.", call. = FALSE)

    }

    other_name <- other_name[1]

    ## re-coding -------

    old_ids <- x$membership

    if(!is.factor(old_ids)) {

      old_ids <- factor(old_ids)

    }

    new_ids <- fct_lump_min(old_ids,
                            min = cutoff + 1,
                            other_level = other_name)

    x$membership <- new_ids

    x

  }

# Appending graph with community information --------

#' Append graph vertices with community assignment.
#'
#' @description
#' Appends vertices of a graph with community identifiers.
#'
#' @details
#' The community identifiers are stored as a vertex attribute called
#' `community_id`. The community assignment may be re-named or re-coded by
#' calling \code{\link{comm_recode}} or \code{\link{comm_lump}}.
#'
#' @return an `igraph` object.
#'
#' @param x an `igraph` object.
#' @param communities an object of class `communities`.
#' @param index_type type of the index used for assignment of the community
#' identifier attribute: names (default) of indexes.
#'
#' @export

  add_communities <- function(x,
                              communities,
                              index_type = c('name', 'index')) {

    ## entry control -------

    if(!inherits(x, 'igraph')) {

      stop("'x' has to be an 'igraph' object.", call. = FALSE)

    }

    if(!inherits(communities, 'communities')) {

      stop("'communities' has to be a 'communities' object.", call. = FALSE)

    }

    index_type <- match.arg(index_type[1], c('name', 'index'))

    ## community assignment -------

    comm_tbl <- assignment(communities)

    if(index_type == 'name' & (!'name' %in% names(comm_tbl))) {

      warning('No vertex name was retrieved, community assignment by index.',
              call. = FALSE)

      index_type <- 'index'

    }

    comm_tbl <- comm_tbl[c(index_type, 'community_id')]

    ## attribute setting --------

    set_vertex_attributes(x,
                          lexicon = comm_tbl,
                          index_type = index_type)

  }

# END ------
