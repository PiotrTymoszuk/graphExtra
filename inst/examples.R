# Example graph analysis using graphExtra

# tools --------

  library(tidyverse)
  library(igraph)
  library(graphExtra)

# analysis data: numeric variables of MASS's cars93 data set --------

  ## numeric data used for network construction

  numeric_cars <-
    MASS::Cars93[, c("Make",
                     "Min.Price", "Max.Price",
                     "MPG.city", "MPG.highway",
                     "Cylinders", "EngineSize", "Horsepower",
                     "Rev.per.mile", "Fuel.tank.capacity",
                     "Passengers",
                     "Length", "Wheelbase",
                     "Width", "Turn.circle",
                     "Rear.seat.room", "Luggage.room",
                     "Weight")]

  numeric_cars <- numeric_cars %>%
    mutate(Cylinders = as.numeric(Cylinders)) %>%
    column_to_rownames('Make')

  ## cars' attributes

  car_attributes <-
    MASS::Cars93[, c("Make", "Manufacturer", "Type",
                     "AirBags", "DriveTrain", "Origin")]

# Graph objects weighted by Kendall's Tau and inverse Manhattan distances --------

  ## graph object from a TauB similarity matrix

  tau_mtx <- numeric_cars %>%
    t %>%
    cor(method = 'kendall')

  as_iGraph(tau_mtx,
            input_type = 'similarity')

  ## graph object from a data frame, weighted by Kendall's TauB

  as_iGraph(numeric_cars,
            feature_type = 'rows',
            cutoff = 0.95,
            fun = cor,
            method = 'kendall',
            weighted = TRUE)

  ## graph object from a distance object of Manhattan distances:
  ## min/max normalization of the distances, selection of
  ## the strongest associations
  ## The object will be used in later analyses

  dist_mtx <- numeric_cars %>%
    dist(method = 'manhattan')

  norm_function <- function(x) {

    1 - (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  }

  car_network <-
    as_iGraph(dist_mtx,
              fun = norm_function,
              cutoff = 0.9,
              diag = FALSE)

# Pruning of low-degree vertices --------

  degree(car_network)

  car_network_pruned <- prune_degree(car_network, cutoff = 1)

  list(car_network, car_network_pruned) %>%
    map(degree) %>%
    map(sort)

# Getting and setting graph attributes -------

  get_vertex_attributes(car_network_pruned)

  car_network_pruned <-
    set_vertex_attributes(car_network_pruned,
                          lexicon = car_attributes)

  get_vertex_attributes(car_network_pruned)

# Communities --------

  ## definition of communities by Leyden method

  car_communities_leyden <-
    cluster_leiden(car_network_pruned,
                   objective_function = 'modularity')

  assignment(car_communities_leyden)

  ## re-coding of the communities

  car_communities_leyden <-
    comm_recode(car_communities_leyden,
                new_names = c('#1' = '1',
                              '#2' = '2',
                              '#3' = '3',
                              '#4' = '4',
                              '#5' = '6',
                              '#6' = '5'))

  assignment(car_communities_leyden)

  sizes(car_communities_leyden)

  ## lumping small communities with less than 10 cars together

  car_communities_leyden <- car_communities_leyden %>%
    comm_lump(cutoff = 7, other_name = 'other')

  assignment(car_communities_leyden)

  sizes(car_communities_leyden)

# Assignment of the cars to communities -------

  ## edge betweenness community search for the unpruned network
  ## collapsing the non-common communities
  ## re-naming of the communities

  car_communities_btw <-
    cluster_edge_betweenness(car_network) %>%
    comm_lump(cutoff = 5) %>%
    comm_recode(c('#1' = '1',
                  '#2' = '2',
                  '#3' = '3',
                  '#4' = '4',
                  '#5' = '7'))

  ## assignment of the community info

  car_network <- car_network %>%
    add_communities(car_communities_btw)

  ## appending with other attributes

  car_network <- car_network %>%
    set_vertex_attributes(car_attributes)

  get_vertex_attributes(car_network)

# Pruning by attribute --------

  ## removal of of nodes in the 'other' community

  car_network <- car_network %>%
    prune_vertices(community_id == 'other')

  ## selecting large and small class cars

  car_network %>%
    select_vertices(Type %in% c('Large', 'Van')) %>%
    plot(label_vertices = TRUE)

  car_network %>%
    select_vertices(Type %in% c('Small', 'Compact')) %>%
    plot(label_vertices = TRUE)

# Node importance summary, top most important nodes --------

  ## labels for the hub cars and ignored for the rest

  car_stats <- car_network %>%
    summary %>%
    mutate(top_car = ifelse(betweenness > 200,
                            name, NA)) %>%
    select(-index)

  car_network <- car_network %>%
    set_vertex_attributes(car_stats)

  get_vertex_attributes(car_network)

# visualizations -------

  ## customization via ggplot2 scales

  ## community plot

  plot(car_network,
       vertex_fill_variable = 'community_id',
       vertex_shape_variable = 'Type',
       vertex_label_variable = 'top_car',
       vertex_txt_color_variable = 'community_id',
       weighting_order = 3,
       label_edges = FALSE,
       label_vertices = TRUE,
       seed = 12345,
       plot_title = 'Cars93 network, communities',
       box.padding = 0.5,
       force = 2) +
    scale_linewidth(range = c(0.2, 1)) +
    scale_alpha_continuous(range = c(0.2, 0.5))

  ## betweenness plot

  plot(car_network,
       vertex_fill_variable = 'betweenness',
       vertex_shape_variable = 'Type',
       vertex_label_variable = 'top_car',
       vertex_txt_color_variable = 'betweenness',
       weighting_order = 3,
       label_edges = FALSE,
       label_vertices = TRUE,
       seed = 12345,
       plot_title = 'Cars93 network, node betweenness',
       box.padding = 0.5,
       force = 2) +
    scale_linewidth(range = c(0.2, 1)) +
    scale_alpha_continuous(range = c(0.2, 0.5)) +
    scale_fill_gradient2(low = 'steelblue',
                         mid = 'black',
                         high = 'firebrick',
                         midpoint = 200,
                         limits = c(0, 400),
                         oob = scales::squish) +
    scale_color_gradient2(low = 'steelblue',
                          mid = 'black',
                          high = 'firebrick',
                          midpoint = 200,
                          limits = c(0, 400),
                          oob = scales::squish)

# END ------
