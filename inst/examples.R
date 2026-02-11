# Example graph analysis using graphExtra

# tools --------

  library(tidyverse)

  library(igraph)
  library(graphExtra)

  library(patchwork) ## for multi-plot panels

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
    MASS::Cars93[, c("Make",  "Type")] %>%
    mutate(Type = fct_relevel(Type,
                              "Small", "Compact", "Midsize",
                              "Large", "Van", "Sporty"))

# Graph objects weighted by Kendall's Tau and inverse Manhattan distances --------

  ## graph object from a TauB similarity matrix with NA values

  tau_mtx <- numeric_cars %>%
    t %>%
    cor(method = 'kendall')

  ## not run: gives an error

  #tau_mtx %>%
   # as_iGraph(input_type = 'similarity',
    #          na_action = "ignore")

  tau_mtx %>%
    as_iGraph(input_type = 'similarity',
              na_action = "pad",
              na_pad_value = 0) %>%
    dimensions

  tau_mtx %>%
    as_iGraph(input_type = 'similarity',
              na_action = "remove") %>%
    dimensions

  ## graph object from a data frame, weighted by Kendall's TauB

  as_iGraph(numeric_cars,
            feature_type = 'rows',
            cutoff = 0.95,
            fun = cor,
            method = 'kendall',
            weighted = TRUE,
            na_action = "pad",
            na_pad_value = 0) %>%
    dimensions

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

# Pruning of isolated vertices --------

  ## there are vertices with no neighbors

  car_degrees <- degree(car_network)

  car_degrees[car_degrees == 0]

  car_network <- prune_degree(car_network, cutoff = 0)

# Getting and setting graph attributes -------

  get_vertex_attributes(car_network)

  car_network <-
    set_vertex_attributes(car_network, car_attributes)

  get_vertex_attributes(car_network)

# Communities --------

  ## definition of communities by Leiden method

  set.seed(5467)

  car_communities <-
    cluster_leiden(car_network,
                   objective_function = 'modularity',
                   resolution = 0.5,
                   n_iterations = 100)

  ## re-coding names of the communities

  car_communities <-
    comm_recode(car_communities,
                new_names = c('#1' = '1',
                              '#2' = '2',
                              '#3' = '3',
                              '#4' = '4'))

  assignment(car_communities)

  sizes(car_communities)

  ## lumping small communities with less than 7 cars together

  car_communities <- car_communities %>%
    comm_lump(cutoff = 7, other_name = 'other')

  assignment(car_communities)

  sizes(car_communities)

# Assignment of the cars to communities -------

  ## assignment of the community info

  car_network <- car_network %>%
    add_communities(car_communities)

  get_vertex_attributes(car_network)

# Pruning ans selecting by attribute --------

  ## removal of of nodes in the 'other' community

  car_network <- car_network %>%
    prune_vertices(community_id == 'other')

  ## selecting large and small class cars

  car_network %>%
    select_vertices(Type %in% c('Large', 'Van')) %>%
    get_vertex_attributes

  car_network %>%
    select_vertices(Type %in% c('Small', 'Compact')) %>%
    get_vertex_attributes

# Node importance summary, top most important nodes --------

  ## vertex importance statistics
  ## labels for the cars with the highest betweenness and ignored for the rest
  ## the name labels will be shown in plots

  car_stats <- car_network %>%
    summary %>%
    mutate(top_car = ifelse(betweenness > 200,
                            name, NA)) %>%
    select(-index)

  car_network <- car_network %>%
    set_vertex_attributes(car_stats)

  get_vertex_attributes(car_network)

# Community subgraphs ------

  ## local vertex importance statistics for communities and car types

  car_network %>%
    split_vertices(community_id) %>%
    map(summary) %>%
    map(filter, betweenness > 0) %>%
    map(slice_max, betweenness, n = 10)

  car_network %>%
    split_vertices(Type) %>%
    map(summary) %>%
    map(filter, betweenness > 0) %>%
    map(slice_max, betweenness, n = 10)

# visualizations -------

  ## some plot globals

  linewidth_range <- c(0.2, 1)
  alpha_range <- c(0.2, 0.5)

  type_shapes <- c(15:19, 9)

  community_colors <- c("aquamarine4", "orangered3", "steelblue")

  ## community plot

  car_network_plots <- list()

  car_network_plots$community_id <-
    plot(car_network,
         layout = layout.fruchterman.reingold,
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
    scale_color_manual(values = community_colors) +
    scale_fill_manual(values = community_colors)

  ## point color codes for betweennes

  car_network_plots$betweennes <-
    plot(car_network,
       layout = layout.fruchterman.reingold,
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

  ## common styling

  car_network_plots <- car_network_plots %>%
    map(~.x +
        scale_linewidth(range = linewidth_range) +
        scale_alpha_continuous(range = alpha_range) +
        scale_shape_manual(values = type_shapes))


  car_network_plots$community_id +
    car_network_plots$betweennes

# distribution of car types in the communities ---------

  community_types <- car_network %>%
    get_vertex_attributes %>%
    group_by(community_id) %>%
    count(Type) %>%
    mutate(n_total = sum(n),
           percent = n/n_total) %>%
    ungroup

  community_type_plot <- community_types %>%
    ggplot(aes(x = percent,
               y = reorder(paste(community_id, n_total, sep = "\nn = "),
                           -as.integer(community_id)),
               fill = Type)) +
    geom_bar(stat = "identity",
             position = position_stack(),
             color = "black") +
    scale_fill_brewer(palette = "Set2") +
    theme_classic() +
    labs(title = "Car types in the network communities",
         x = "% of community",
         y = "community, Leiden")

# Neighborhood ---------

  audi90_neighbors <- neighbor_graph(car_network, name = 'Audi 90')

  audi90_neighbors %>% V

  neighbor_attr(car_network, name = 'Audi 90')

  audi90_neighbor_plot <- car_network %>%
    neighbor_graph(name = 'Audi 90') %>%
    plot(label_vertices = TRUE) +
    labs(title = "Audi 90 and its neighbors")

# END ------
