#' ----
# Packages used in Muylaert et al. sarbecovirus hosts distribution
# Make sure you are in distribution_models project
#' ----

# package list
pkg_list_cran <- c("devtools", 
                   "Rcpp",
                   "here",
                   "tidyverse",
                   "xlsx", 
                   "vroom",
                   "data.table",
                   "janitor",
                   "stringi",
                   "reshape2",
                   "DataExplorer",
                   "skimr",
                   "XML",
                   "spocc",
                   "taxize",
                   "CoordinateCleaner",
                   "rredlist",
                   "rnaturalearth",
                   "rnaturalearthdata",
                   "spData",
                   "sf",
                   "raster",
                   "terra",
                   "ncdf4",
                   "spatialEco",
                   "ecospat",
                   "dismo",
                   "tmap",
                   "ggmap",
                   "ggspatial",
                   "ggsn",
                   "ggbump",
                   "gghighlight",
                   "ggraph",
                   "ggridges",
                   "igraph",
                   "maps",
                   "mapdata",
                   "legendMap",
                   "rasterVis",
                   "leaflet",
                   "leafem",
                   "leaflet.opacity",
                   "htmlwidgets",
                   "htmltools",
                   "lattice",
                   "ggpubr",
                   "corrplot",
                   "classInt",
                   "graphlayouts",
                   "oaqc",
                   "bipartite",
                   "RColorBrewer",
                   "randomcoloR",
                   "viridis",
                   "wesanderson",
                   "hrbrthemes",
                   "rstatix",
                   "MuMIn")

# require else install all packages
lapply(X = pkg_list_cran, 
       FUN = function(x) if(!require(x, character.only = TRUE)) install.packages(x, dep = TRUE, quiet = TRUE))

# packages from github
if(!require(scico)) devtools::install_github("thomasp85/scico")
if(!require(platexpress)) devtools::install_github("raim/platexpress")
if(!require(Manu)) devtools::install_github("G-Thomson/Manu")

# end ---------------------------------------------------------------------