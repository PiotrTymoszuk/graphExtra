# graphExtra

Manipulation and plotting accessories for `igraph` objects

## Installation

You may easily fetch the package via `devtools`: 

```r

devtools::install_github('PiotrTymoszuk/graphExtra')

```

## Terms of use

The package is available under a [GPL-3 license](https://github.com/PiotrTymoszuk/grphExtra/blob/main/LICENSE).

## Contact

The package maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Acknowledgements

Many thanks to authors, maintainers and contributors of the 
[tidyverse evironment](https://www.tidyverse.org/), and packages 
[igraph](https://igraph.org/), 
[ggnetwork](https://briatte.github.io/ggnetwork/), 
and [ggtext](https://wilkelab.org/ggtext/). 

## Basic functions

The package offers few functional tools that may help to integrate great tools of `igraph` in tidyverse-styled analysis pipelines: 

* `as_iGraph()`: generates `igraph` objects from a wide range of similarity and distance matrices as well as numeric matrices and data frames with similarity or correlation metrics defined by the user
* `prune_degree()`, `select_vertices()`, and `prune_vertices()` allow for selection or removal of graph vertices based on a degree cutoff or logical expressions
* `split_vertices()` splits a graph into sub-graphs by levels of one or more vertex attributes; this may be especially interesting for detailed analyses of communities
* `get_vertex_attributes()` and `set_vertex_attributes()` let the user fetch and set multiple vertex attributes with a data frame or a tibble
* `add_community()` merges the community information obtained with one of the clustering tools offered by `igraph` package with vertex attributes
* `summary()` computes a bunch of common vertex importance statistics such as degree, betweenness, and hub score
* `plot()` overwrites the respective default method to generate `ggplot`-compatible graphics for `igraph` objects
* `neighbor_graph()` and `neighbor_attr()` extract the neighborhood of a vertex of interest and the neighborhood attributes, respectively

## Basic usage: similarity network of cars

### Input data

In the following example, we will investigate a similarity network of cars in 
the `Cars93` data set provided with the `MASS` package. 
The input data for the similarity graph are numeric features such as number of cylinders, car dimensions, 
or price. 
The car's make and classification (small, compact, midsize, large, van, sporty) will serve as vertex attributes. 

```r

  ## required packages

  library(tidyverse)

  library(igraph)
  library(graphExtra)

  library(patchwork) ## for multi-plot panels

```

```r

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

```

### Construction of similarity network

The network is constructed with function `as_iGraph()` which takes a range of different objects 
and converts them to non-directional `igraph` graphs. 
The possible inputs are data frames, data matrices, correlation matrices, or distances. 
Please note that any `NA` values in the similarity matrix make computation impossible. 
The user can choose to fill them with a numeric value or rows/columns with `NA` values. 

In the code below, the TauB correlation matrix of cars has `NAs`. 
They are differently handled by `as_iGraph()`, which results in differences in network's dimensions: 

```r
 tau_mtx <- numeric_cars %>%
    t %>%
    cor(method = 'kendall')

```

```r

## ignoring: throws an error

>   tau_mtx %>%
+     as_iGraph(input_type = 'similarity',
+               na_action = "ignore")
Error: There are NA values in the similarity matrix.

```

```r

## filling with 0

> tau_mtx %>%
+     as_iGraph(input_type = 'similarity',
+               na_action = "pad",
+               na_pad_value = 0) %>%
+     dimensions
vertices    edges 
      93     3321 
Warning message:
There are 1914 NA values in the similarity matrix. They are padded with 0 

```

```r

## row/column-wise removal

> tau_mtx %>%
+     as_iGraph(input_type = 'similarity',
+               na_action = "remove") %>%
+     dimensions
vertices    edges 
      82     3321 
Warning message:
There are 1914 NA values in the similarity matrix. They amount to 11 incomplete rows, which will be removed.

```

For further analyses, we will construct a weighted network with a similarity measure 
derived from Manhattan distances between the cars. 
We choose a cutoff 0.9 of the similarity statistics, which means that the network's 
edges will be defined by pairwise associations between cars with similarity of 0.9 
and larger: 

```r

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

```

### Removal of isolated vertices, getting and setting vertex attributes

In our car's network there are two cars (Geo Metro and Volkswagen Eurovan) 
without neighbors, i.e. with no similarity of 0.9 or larger with any other cars. 
If desired, such isolated vertices can be easily removed with function `prune_degree()`, 
which removes all vertices with degree/neighbor number lower than a cutoff value: 

```r

  ## there are vertices with no neighbors

  car_degrees <- degree(car_network)

  car_network <- prune_degree(car_network, cutoff = 0)

```

```r

> car_degrees[car_degrees == 0]
         Geo Metro Volkswagen Eurovan 
                 0                  0 

```
By default, there are two vertex attributes in the graph: the vertex' index and 
name. 
The user can set additional attributes from a data frame with `set_vertex_attributes()` 
function. 
The attributes are matched with the vertices by the first column of the 
attribute data frame, which is expected to contain the vertex' names.
To obtain vertex attributes, please call `get_vertex_attributes()`: 

```r

## default vertex attributes

>   get_vertex_attributes(car_network)

# A tibble: 91 × 2
   index name            
   <int> <chr>           
 1     1 Acura Integra   
 2     2 Acura Legend    
 3     3 Audi 90         
 4     4 Audi 100        
 5     5 BMW 535i        
 6     6 Buick Century   
 7     7 Buick LeSabre   
 8     8 Buick Roadmaster
 9     9 Buick Riviera   
10    10 Cadillac DeVille
# ℹ 81 more rows
# ℹ Use `print(n = ...)` to see more rows

```

```r

## setting additional vertex attributes: 
## car type

  car_network <-
    set_vertex_attributes(car_network, car_attributes)
    
```

```r 

## type was set as the new attribute

> get_vertex_attributes(car_network)

# A tibble: 91 × 3
   index name             Type   
   <int> <chr>            <fct>  
 1     1 Acura Integra    Small  
 2     2 Acura Legend     Midsize
 3     3 Audi 90          Compact
 4     4 Audi 100         Midsize
 5     5 BMW 535i         Midsize
 6     6 Buick Century    Midsize
 7     7 Buick LeSabre    Large  
 8     8 Buick Roadmaster Large  
 9     9 Buick Riviera    Midsize
10    10 Cadillac DeVille Large  
# ℹ 81 more rows
# ℹ Use `print(n = ...)` to see more rows

```

### Community detection, manipulation, and additng community information to the graph

The `igraph` package offers a broad palette of tools for community detection, 
including the popular [Leiden algorithm](https://en.wikipedia.org/wiki/Leiden_algorithm) 
implemented by `cluster_leiden()` function. 

```r

  ## definition of communities by Leiden method

  set.seed(5467)

  car_communities <-
    cluster_leiden(car_network,
                   objective_function = 'modularity',
                   resolution = 0.5,
                   n_iterations = 100)

```

Our `graphExtra` package provides tools for re-naming of communities (`comm_recode()`), 
community assignment and size computation (`assignment()` and `sizes()`), 
merging small communities together (`comm_lump()`): 

```r

  ## re-coding names of the communities

  car_communities <-
    comm_recode(car_communities,
                new_names = c('#1' = '1',
                              '#2' = '2',
                              '#3' = '3',
                              '#4' = '4'))
                              
  ## lumping small communities with less than 7 cars together

  car_communities <- car_communities %>%
    comm_lump(cutoff = 7, other_name = 'other')
    
```
```r

> assignment(car_communities)

# A tibble: 91 × 3
   index name             community_id
   <int> <chr>            <fct>       
 1     1 Acura Integra    #1          
 2     2 Acura Legend     #2          
 3     3 Audi 90          #2          
 4     4 Audi 100         #2          
 5     5 BMW 535i         #2          
 6     6 Buick Century    #1          
 7     7 Buick LeSabre    #2          
 8     8 Buick Roadmaster other       
 9     9 Buick Riviera    #2          
10    10 Cadillac DeVille #2          
# ℹ 81 more rows
# ℹ Use `print(n = ...)` to see more rows

```

```r

>   sizes(car_communities)

Community sizes
 1  2  3  4 
44 36  8  3 

```

An easy way to add the community assignment to the network is to call `add_communities()`, 
which sets the community label as the vertex attribute `community_id`: 

```r

  car_network <- car_network %>%
    add_communities(car_communities)
    
```

```r

>   get_vertex_attributes(car_network)

# A tibble: 91 × 4
   index name             Type    community_id
   <int> <chr>            <fct>   <fct>       
 1     1 Acura Integra    Small   #1          
 2     2 Acura Legend     Midsize #2          
 3     3 Audi 90          Compact #2          
 4     4 Audi 100         Midsize #2          
 5     5 BMW 535i         Midsize #2          
 6     6 Buick Century    Midsize #1          
 7     7 Buick LeSabre    Large   #2          
 8     8 Buick Roadmaster Large   other       
 9     9 Buick Riviera    Midsize #2          
10    10 Cadillac DeVille Large   #2          
# ℹ 81 more rows
# ℹ Use `print(n = ...)` to see more rows


```

### Transformation of the network: pruning, selecting, and splitting vertices by attributes. Vertex's neighbors

Vertices can be removed or selected by their attributes or any other logical expressions 
with functions `prune_vertices()` and `select_vertices()`: 

```r

  ## removal of of nodes in the 'other' community

  car_network <- car_network %>%
    prune_vertices(community_id == 'other')
    
```

```r

  ## vertex attributes for car types
  
>   car_network %>%
+     select_vertices(Type %in% c('Large', 'Van')) %>%
+     get_vertex_attributes

# A tibble: 16 × 4
   index name                    Type  community_id
   <int> <chr>                   <fct> <fct>       
 1     1 Buick LeSabre           Large #2          
 2     2 Cadillac DeVille        Large #2          
 3     3 Chevrolet Lumina_APV    Van   #2          
 4     4 Chevrolet Astro         Van   #2          
 5     5 Chrylser Concorde       Large #2          
 6     6 Chrysler Imperial       Large #2          
 7     7 Dodge Caravan           Van   #2          
 8     8 Eagle Vision            Large #2          
 9     9 Ford Aerostar           Van   #2          
10    10 Lincoln Town_Car        Large #2          
11    11 Mazda MPV               Van   #2          
12    12 Nissan Quest            Van   #2          
13    13 Oldsmobile Silhouette   Van   #2          
14    14 Oldsmobile Eighty-Eight Large #2          
15    15 Pontiac Bonneville      Large #2          
16    16 Toyota Previa           Van   #2     

>   car_network %>%
+     select_vertices(Type %in% c('Small', 'Compact')) %>%
+     get_vertex_attributes

# A tibble: 36 × 4
   index name               Type    community_id
   <int> <chr>              <fct>   <fct>       
 1     1 Acura Integra      Small   #1          
 2     2 Audi 90            Compact #2          
 3     3 Chevrolet Cavalier Compact #1          
 4     4 Chevrolet Corsica  Compact #1          
 5     5 Chrysler LeBaron   Compact #1          
 6     6 Dodge Colt         Small   #4          
 7     7 Dodge Shadow       Small   #1          
 8     8 Dodge Spirit       Compact #1          
 9     9 Eagle Summit       Small   #1          
10    10 Ford Festiva       Small   #4          
# ℹ 26 more rows
# ℹ Use `print(n = ...)` to see more rows

```
Function `split_vertices()` generates a list of sub-graphs defined by levels 
of a vertex attribute. 
Here, we create a list of sub-plots for each of the communities: 

```r

  > car_network %>%
+     split_vertices(community_id)

$`#1`
IGRAPH 957a0bc UNW- 44 378 -- 
+ attr: name (v/c), Type (v/x), community_id (v/x), degree (v/n), betweenness (v/n), hub_score (v/n), transitivity
| (v/n), top_car (v/c), weight (e/n)
+ edges from 957a0bc (vertex names):
 [1] Acura Integra--Chevrolet Corsica  Acura Integra--Dodge Shadow       Acura Integra--Ford Tempo        
 [4] Acura Integra--Honda Prelude      Acura Integra--Hyundai Elantra    Acura Integra--Pontiac Sunbird   
 [7] Acura Integra--Saab 900           Buick Century--Chevrolet Corsica  Buick Century--Chevrolet Lumina  
[10] Buick Century--Dodge Shadow       Buick Century--Dodge Spirit       Buick Century--Dodge Dynasty     
[13] Buick Century--Ford Tempo         Buick Century--Ford Mustang       Buick Century--Ford Probe        
[16] Buick Century--Honda Prelude      Buick Century--Honda Accord       Buick Century--Hyundai Elantra   
[19] Buick Century--Hyundai Sonata     Buick Century--Mazda 626          Buick Century--Mercedes-Benz 190E
+ ... omitted several edges

$`#2`
IGRAPH 957a1a0 UNW- 36 178 -- 
+ attr: name (v/c), Type (v/x), community_id (v/x), degree (v/n), betweenness (v/n), hub_score (v/n), transitivity
| (v/n), top_car (v/c), weight (e/n)
+ edges from 957a1a0 (vertex names):
 [1] Acura Legend --Audi 90             Acura Legend --Audi 100            Acura Legend --BMW 535i           
 [4] Acura Legend --Chrylser Concorde   Acura Legend --Eagle Vision        Acura Legend --Lexus ES300        
 [7] Acura Legend --Lexus SC300         Acura Legend --Mazda MPV           Acura Legend --Mercedes-Benz 300E 
[10] Acura Legend --Mitsubishi Diamante Audi 90      --Audi 100            Audi 90      --Lexus ES300        
[13] Audi 90      --Lexus SC300         Audi 90      --Mazda MPV           Audi 90      --Mercedes-Benz 300E 
[16] Audi 90      --Mitsubishi Diamante Audi 90      --Nissan Maxima       Audi 100     --BMW 535i           
[19] Audi 100     --Lexus ES300         Audi 100     --Lexus SC300         BMW 535i     --Lexus ES300        
+ ... omitted several edges

$`#4`
IGRAPH 957a267 UNW- 8 11 -- 
+ attr: name (v/c), Type (v/x), community_id (v/x), degree (v/n), betweenness (v/n), hub_score (v/n), transitivity
| (v/n), top_car (v/c), weight (e/n)
+ edges from 957a267 (vertex names):
 [1] Dodge Colt    --Geo Storm      Dodge Colt    --Pontiac LeMans Dodge Colt    --Subaru Loyale  Dodge Colt    --Suzuki Swift  
 [5] Dodge Colt    --Toyota Tercel  Ford Festiva  --Subaru Justy   Ford Festiva  --Suzuki Swift   Geo Storm     --Pontiac LeMans
 [9] Geo Storm     --Subaru Loyale  Pontiac LeMans--Subaru Loyale  Suzuki Swift  --Toyota Tercel 

```

With functions `neighbor_graph()` and `neighbor_attr()`, we can extract 
a the network of all neighbors of a given vertex and investigate the neighbors' 
attributes. 
Below, we were interested in neighbors of Audi 90: 

```r

 audi90_neighbors <- neighbor_graph(car_network, name = 'Audi 90')

 ## names of the neighbors
 
 >  audi90_neighbors %>% V
 
+ 12/12 vertices, named, from cf07e58:
 [1] Acura Legend        Audi 90             Audi 100            Lexus ES300         Lexus SC300         Mazda MPV          
 [7] Mercedes-Benz 300E  Mitsubishi Diamante Nissan Maxima       Subaru Legacy       Toyota Camry        Volvo 850   
 
 
 ## attributes of the neighbors including similarities (`weight`) 
 ## with Audi 90
 
 >   neighbor_attr(car_network, name = 'Audi 90')
 
# A tibble: 12 × 5
   index name                Type    community_id weight
   <int> <chr>               <fct>   <fct>         <dbl>
 1     2 Acura Legend        Midsize #2            0.939
 2     3 Audi 90             Compact #2           NA    
 3     4 Audi 100            Midsize #2            0.936
 4    45 Lexus ES300         Midsize #2            0.958
 5    46 Lexus SC300         Midsize #2            0.909
 6    52 Mazda MPV           Van     #2            0.904
 7    55 Mercedes-Benz 300E  Midsize #2            0.934
 8    59 Mitsubishi Diamante Midsize #2            0.906
 9    63 Nissan Maxima       Midsize #2            0.912
10    78 Subaru Legacy       Compact #1            0.921
11    82 Toyota Camry        Midsize #1            0.905
12    88 Volvo 850           Midsize #1            0.964

```

### Vertex importance statistics

Method `summary()` called for a graph/network object allows us to compute basic vertex 
importance statistics: 

* __degree__: number of the vertex' neighbors

* __betweenness__: numbers of the shortest paths between pairs of vertices which pass via the vertex of interest

* __hub score__: eigenvector of the similarity matrix

* __transitivity__: density of the neighborhood of the vertex

Below, we are computing these statistics for our car similarity network and setting them 
as vertex attributes. 
We also define a new attribute `top_car`, which stores names of cars with the largest 
betweenness - these names will be later displayed in plots.

```r

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

```

```r

>   get_vertex_attributes(car_network)

# A tibble: 88 × 9
   index name             Type    community_id degree betweenness hub_score transitivity top_car
   <int> <chr>            <fct>   <fct>         <dbl>       <dbl>     <dbl>        <dbl> <chr>  
 1     1 Acura Integra    Small   #1                7           0  0.160           0.810 NA     
 2     2 Acura Legend     Midsize #2               11         103  0.0474          0.527 NA     
 3     3 Audi 90          Compact #2               11         263  0.140           0.473 Audi 90
 4     4 Audi 100         Midsize #2                7          43  0.0627          0.619 NA     
 5     5 BMW 535i         Midsize #2                6           0  0.00996         0.667 NA     
 6     6 Buick Century    Midsize #1               24          16  0.940           0.641 NA     
 7     7 Buick LeSabre    Large   #2               14           7  0.0139          0.670 NA     
 8     8 Buick Riviera    Midsize #2               15           0  0.0168          0.714 NA     
 9     9 Cadillac DeVille Large   #2                9           0  0.00568         0.917 NA     
10    10 Cadillac Seville Midsize #2                5           5  0.000695        0.5   NA     
# ℹ 78 more rows
# ℹ Use `print(n = ...)` to see more rows

```

### Graph visualization

Our `graphExtra` package overwrites the `plot()` method for `igraph` graphs/networks, 
to generate plots in `ggplot` format, which can be easily customized by the user. 
Under the hood, `plot()` makes use of tools of [`ggnetwork` package](https://briatte.github.io/ggnetwork/). 
This means, that it is always possible to add new node and edge symbols or labels to an 
existing graph. 

Below, we generate two plots of the car network with Fruchterman-Reingold algorithm. 
In the first plot, the vertex symbol color codes for the community assignment; in the second one, 
the vertex color codes for the betweenness statistic - aiming at identification of "hub cars". 
The vertex symbol shape codes for car type. 
By setting `weighting_order = 3`, we guarantee that the line width and alpha for the edges is proportional 
to the third power of similarity statistics. 
Consequently `weighting_order = 0` specifies that all edges in the plot have the same width independently 
of the similarity statistic. 
The plots are styled with color, fill, shape, alpha and line width scales provided by the `ggplot` tool set: 

```r

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

```

```r 

  car_network_plots$community_id +
    car_network_plots$betweennes

```

As we can infer from the plots, the communities are expected to differ in car sizes, 
with community #1 enriched with small and compact cars, community #2 enriched with midsize, large and van vehicles, 
and community #2 consisting almost exclusively of small automobiles. 
With few lines of code, we can extract, quantify, and plot percentages of car types 
in the communities: 

```r

  community_types <- car_network %>%
    get_vertex_attributes %>%
    group_by(community_id) %>%
    count(Type) %>%
    mutate(n_total = sum(n),
           percent = n/n_total) %>%
    ungroup
    
> community_types

# A tibble: 11 × 5
   community_id Type        n n_total percent
   <fct>        <fct>   <int>   <int>   <dbl>
 1 #1           Small      13      44  0.295 
 2 #1           Compact    15      44  0.341 
 3 #1           Midsize     7      44  0.159 
 4 #1           Sporty      9      44  0.205 
 5 #2           Compact     1      36  0.0278
 6 #2           Midsize    15      36  0.417 
 7 #2           Large       8      36  0.222 
 8 #2           Van         8      36  0.222 
 9 #2           Sporty      4      36  0.111 
10 #4           Small       7       8  0.875 
11 #4           Sporty      1       8  0.125 
    

```

```r

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

```

