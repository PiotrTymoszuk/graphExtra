# graphExtra

Manipulation and plotting accessories for `igraph` objects

## Basic usage

The package offers few functional tools that may help to integrate great tools of `igraph` in tidyverse-styled analysis pipelines: 

* `as_iGraph()`: generates `igraph` objects from a wide range of similarity and distance matrices as well as numeric matrices and data frames with similarity or correlation metrics defined by the user
* `prune_degree()`, `select_vertices()`, and `prune_vertices()` allow for selection or removal of graph vertices based on a degree cutoff or logical expressions
* `get_vertex_attributes()` and `set_vertex_attributes()` let the user fetch and set multiple vertex attributes with a data frame or a tibble
* `add_community()` merges the community information obtained with one of the clustering tools offered by `igraph` package with vertex attributes
* `summary()` computes a bunch of common vertex importance statistics such as degree, betweenness, and hub score
* `plot()` overwrites the respective default method to generate `ggplot`-compatible graphics for `igraph` objects

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

Many thanks to authors, maintainers and contributors of the [tidyverse evironment](https://www.tidyverse.org/), and packages [igraph](https://igraph.org/) and [ggnetwork](https://briatte.github.io/ggnetwork/).
