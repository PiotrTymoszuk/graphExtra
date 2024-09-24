# Visualization of the graphs in form of a network plots.

#' Network visualization of a graph.
#'
#' @description
#' Draws a network diagram of an `igraph` object using the tool set of
#' `ggnetwork` package. The plots may be easily customized by the user
#' by specifying color, fill, and alpha scales, as well as the plot theme
#' via functions of `ggplot2` package.
#'
#' @details
#' Technically, the `plot()` method for `igraph` objects uses the toolbox of
#' `ggnetwork` package.
#'
#' @return a `ggplot` object.
#'
#' @param x an `igraph` object.
#' @param vertex_fill_variable name of an attribute coding for vertex symbol fill.
#' @param vertex_color_variable name of an attribute coding for vertex
#' symbol color.
#' @param vertex_shape_variable name of an attribute coding for vertex
#' symbol shape.
#' @param weighting_order order (power) of similarity weights between
#' the vertices, where similarity codes for edge alpha and width.
#' If `weighting_order = 0`, all edges will be plotted with the same width
#' and alpha.
#' @param vertex_fill fill of vertex symbols;
#' used only if `vertex_fill_variable = NULL`.
#' @param vertex_color color of vertex symbols;
#' used only if `vertex_color_variable = NULL`.
#' @param vertex_alpha alpha of vertex symbols.
#' @param vertex_size size of vertex symbols.
#' @param edge_color color of edges.
#' @param edge_alpha alpha of edges; used only if `weighting_order = 0`.
#' @param edge_width width of edges; used only if `weighting_order = 0`.
#' @param label_vertices logical: should labels of vertices be displayed?
#' @param label_edges logical; should labels with edge weights
#' (i.e. similarity) should be displayed?
#' @param vertex_label_variable name of an attribute storing the vertex labels;
#' by default this is the vertex name.
#' @param vertex_txt_color_variable name of an attribute coding for vertex
#' label text color.
#' @param vertex_txt_size size of vertex label text.
#' @param vertex_txt_color color of vertex label text; used only if
#' `vertex_txt_color_variable = NULL`.
#' @param vertex_txt_face font face of the vertex labels.
#' @param vertex_labeller a function that takes vertex labels and
#' transforms them.
#' @param edge_txt_size size of edge label text.
#' @param edge_txt_color color of edge label text.
#' @param edge_txt_face font face of edge label text.
#' @param edge_txt_signif significant digits used for rounding of the edge
#' weights presented in the edge labels.
#' @param cust_theme a custom `ggplot` theme.
#' @param plot_title plot title.
#' @param plot_subtitle plot_subtitle. If `NULL`, numbers of nodes and edges
#' are displayed there.
#' @param seed random number generator seed. If `NULL`, no seed is set.
#' @param ... additional arguments passed to
#' \code{\link[ggnetwork]{geom_nodetext_repel}}.
#'
#' @export

  plot.igraph <- function(x,

                          vertex_fill_variable = NULL,
                          vertex_color_variable = vertex_fill_variable,
                          vertex_shape_variable = NULL,
                          weighting_order = 0,

                          vertex_fill = 'steelblue2',
                          vertex_color = 'steelblue4',
                          vertex_alpha = 1,
                          vertex_size = 2,

                          edge_color = 'gray60',
                          edge_alpha = 1,
                          edge_width = 0.25,

                          label_vertices = FALSE,
                          label_edges = FALSE,

                          vertex_label_variable = 'name',
                          vertex_txt_color_variable = NULL,
                          vertex_txt_size = 2.75,
                          vertex_txt_color = 'black',
                          vertex_txt_face = 'plain',
                          vertex_labeller = identity,

                          edge_txt_size = 2.75,
                          edge_txt_color = 'black',
                          edge_txt_face = 'plain',
                          edge_txt_signif = 2,

                          cust_theme = ggplot2::theme_void(),

                          plot_title = NULL,
                          plot_subtitle = NULL,

                          seed = NULL, ...) {

    ## entry control -------

    stopifnot(inherits(x, 'igraph'))

    attr_tbl <- get_vertex_attributes(x)

    attr_names <- names(attr_tbl)

    if(!is.null(vertex_fill_variable)) {

      if(!vertex_fill_variable %in% attr_names) {

        stop("No attribute matched by 'vertex_fill_variable'", call. = FALSE)

      }

    }

    if(!is.null(vertex_color_variable)) {

      if(!vertex_color_variable %in% attr_names) {

        stop("No attribute matched by 'vertex_color_variable'", call. = FALSE)

      }

    }

    if(!is.null(vertex_shape_variable)) {

      if(!vertex_shape_variable %in% attr_names) {

        stop("No attribute matched by 'vertex_shape_variable'", call. = FALSE)

      }

    }

    if(!is.null(vertex_txt_color_variable)) {

      if(!vertex_txt_color_variable %in% attr_names) {

        stop("No attribute matched by 'vertex_txt_color_variable'", call. = FALSE)

      }

    }

    if(!is.null(vertex_label_variable)) {

      if(!vertex_label_variable %in% attr_names) {

        stop("No attribute matched by 'vertex_label_variable'", call. = FALSE)

      }

    }

    stopifnot(is.logical(label_vertices))
    stopifnot(is.logical(label_edges))

    if(!inherits(cust_theme, 'theme')) {

      stop("'cust_theme' has to be a ggplot theme object/function.",
           call. = FALSE)

    }

    if(!is.function(vertex_labeller)) {

      stop("'vertex_txt_labeller' has to be a function.",
           call. = FALSE)

    }

    stopifnot(is.numeric(edge_txt_signif))

    edge_txt_signif <- as.integer(edge_txt_signif[1])

    ## base plot --------

    y <- NULL
    xend <- NULL
    yend <- NULL
    weight <- NULL

    if(is.null(plot_subtitle)) {

      net_dims <- dimensions(x)

      plot_subtitle <- paste0('vertices: n = ', net_dims['vertices'],
                              ', edges: n = ', net_dims['edges'])

    }

    if(!is.null(seed)) set.seed(seed)

    graph_plot <- ggplot(x,
                         aes(x = x,
                             y = y,
                             xend = xend,
                             yend = yend)) +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle)

    ## edges -------

    if(weighting_order == 0) {

      graph_plot <- graph_plot +
        geom_edges(alpha = edge_alpha,
                   linewidth = edge_width,
                   color = edge_color)

    } else {

      if(weighting_order != 1) {

        edge_title <- paste0('similarity <sup>', weighting_order, '</sup>')

      } else {

        edge_title <- 'similarity'

      }

      graph_plot <- graph_plot +
        geom_edges(aes(alpha = weight^weighting_order,
                       linewidth = weight^weighting_order)) +
        labs(alpha = edge_title,
             linewidth = edge_title) +
        theme(legend.title = element_markdown())

    }

    ## vertices --------

    if(!is.null(vertex_shape_variable) &
       !is.null(vertex_color_variable) &
       !is.null(vertex_fill_variable)) {

      graph_plot <- graph_plot +
        geom_nodes(aes(shape = .data[[vertex_shape_variable]],
                       color = .data[[vertex_color_variable]],
                       fill = .data[[vertex_fill_variable]]),
                   alpha = vertex_alpha,
                   size = vertex_size)

    } else if(is.null(vertex_shape_variable) &
              !is.null(vertex_color_variable) &
              !is.null(vertex_fill_variable)) {

      graph_plot <- graph_plot +
        geom_nodes(aes(color = .data[[vertex_color_variable]],
                       fill = .data[[vertex_fill_variable]]),
                   shape = 21,
                   alpha = vertex_alpha,
                   size = vertex_size)

    } else if(is.null(vertex_shape_variable) &
              is.null(vertex_color_variable) &
              !is.null(vertex_fill_variable)) {

      graph_plot <- graph_plot +
        geom_nodes(aes(fill = .data[[vertex_fill_variable]]),
                   color = vertex_color,
                   shape = 21,
                   alpha = vertex_alpha,
                   size = vertex_size)

    } else if(!is.null(vertex_shape_variable) &
              is.null(vertex_color_variable) &
              !is.null(vertex_fill_variable)) {

      graph_plot <- graph_plot +
        geom_nodes(aes(fill = .data[[vertex_fill_variable]],
                       shape = .data[[vertex_shape_variable]]),
                   color = vertex_color,
                   alpha = vertex_alpha,
                   size = vertex_size)

    } else if(!is.null(vertex_shape_variable) &
              !is.null(vertex_color_variable) &
              is.null(vertex_fill_variable)) {

      graph_plot <- graph_plot +
        geom_nodes(aes(color = .data[[vertex_color_variable]],
                       shape = .data[[vertex_shape_variable]]),
                   fill = vertex_fill,
                   alpha = vertex_alpha,
                   size = vertex_size)

    } else if(!is.null(vertex_shape_variable) &
              is.null(vertex_color_variable) &
              is.null(vertex_fill_variable)){

      graph_plot <- graph_plot +
        geom_nodes(aes(shape = .data[[vertex_shape_variable]]),
                   color = vertex_color,
                   fill = vertex_fill,
                   alpha = vertex_alpha,
                   size = vertex_size)

    } else if(is.null(vertex_shape_variable) &
              !is.null(vertex_color_variable) &
              is.null(vertex_fill_variable)) {

      graph_plot <- graph_plot +
        geom_nodes(aes(color = .data[[vertex_color_variable]]),
                   shape = 21,
                   fill = vertex_fill,
                   alpha = vertex_alpha,
                   size = vertex_size)

    } else {

      graph_plot <- graph_plot +
        geom_nodes(color = vertex_color,
                   shape = 21,
                   fill = vertex_fill,
                   alpha = vertex_alpha,
                   size = vertex_size)

    }

    ## edge labels ---------

    if(label_edges) {

      graph_plot <- graph_plot +
        geom_edgelabel_repel(aes(label = signif(weight, edge_txt_signif)),
                             size = edge_txt_size,
                             fontface = edge_txt_face,
                             color = edge_txt_color,
                             show.legend = FALSE)

    }

    ## node labels --------

    if(label_vertices) {

      if(!is.null(vertex_txt_color_variable)) {

        graph_plot <- graph_plot +
          geom_nodetext_repel(aes(label = vertex_labeller(.data[[vertex_label_variable]]),
                                  color = .data[[vertex_txt_color_variable]]),
                              size = vertex_txt_size,
                              fontface = vertex_txt_face, ...)

      } else {

        graph_plot <- graph_plot +
          geom_nodetext_repel(aes(label = vertex_labeller(.data[[vertex_label_variable]])),
                              color = vertex_txt_color,
                              size = vertex_txt_size,
                              fontface = vertex_txt_face, ...)

      }

    }

    ## output -------

    graph_plot

  }

# END -------
