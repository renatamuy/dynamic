#' ----
#' Figure Bivariate Cloropleth map (Figure 3) - code by Colin Carlson, Renata Muylaert, Mauricio Vancine
#' ----

# packages
library(here)
library(tidyverse)
library(classInt)
library(raster)
library(rnaturalearth)
library(dismo)
library(XML)
library(maps)
library(ggspatial)

## functions
# colmat
colmat <- function(nquantiles = 10,
                   upperleft = rgb(0, 150, 235, maxColorValue = 255), 
                   upperright = rgb(130, 0, 80, maxColorValue = 255),
                   bottomleft = "grey",
                   bottomright = rgb(255, 230, 15, maxColorValue = 255), 
                   xlab = "x label", 
                   ylab = "y label"){
  
  my.data <- seq(0, 1, .01)
  my.class <- classIntervals(my.data, n = nquantiles, style = "quantile")
  my.pal.1 <- findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  
  for(i in 1:101){
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102-i, ] <- findColours(my.class, my.col)
    }
  
  plot(c(1, 1), pch = 19, col = my.pal.1, cex = 0.5, xlim = c(0, 1), ylim = c(0, 1), frame.plot = F, xlab = xlab, ylab = ylab, cex.lab = 1.3)
  
  for(i in 1:101){
    col.temp <- col.matrix[i-1, ]
    points(my.data, rep((i-1)/100, 101), pch = 15, col = col.temp, cex = 1)
    }
  
  seqs <- seq(0, 100, (100/nquantiles))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  
  }

# bivariate map
bivariate.map <- function(rasterx, rastery, colormatrix = col.matrix, nquantiles = 10){
  quanmean <- getValues(rasterx)
  temp <- data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  brks <- with(temp, quantile(unique(temp), na.rm = TRUE, probs = c(seq(0, 1, 1/nquantiles))))
  r1 <- within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks), include.lowest = TRUE))
  quantr <- data.frame(r1[, 2]) 
  quanvar <- getValues(rastery)
  temp <- data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  brks <- with(temp, quantile(unique(temp), na.rm = TRUE, probs = c(seq(0, 1, 1/nquantiles))))
  r2 <- within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks), include.lowest = TRUE))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  
  for(i in 1:length(col.matrix2)){
    ifelse(is.na(col.matrix2[i]), col.matrix2[i] <- 1, col.matrix2[i] <- which(col.matrix2[i] == cn)[1])
    }
  cols <- numeric(length(quantr[, 1]))
  
  for(i in 1:length(quantr[, 1])){
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
    }
  r <- rasterx
  r[1:length(r)] <- cols
  return(r)
  }

# -------------------------------------------------------------------------

## hotspot
# directory
setwd(here())
#setwd("/home/mude/data/onedrive/manuscritos/03_preparacao/renata/02_dados")
setwd("hotspots")

# import data
hosts <- raster("richnessm.tif")
hostspct <- hosts/ max(na.omit(values(hosts)))

## sampbias
# directory
setwd(here())
#setwd("/home/mude/data/onedrive/manuscritos/03_preparacao/renata/02_dados")
setwd("sampbias_025dd")
biasr <- raster("rcar_sarbecovirus_hosts.tif") 
crs(biasr) <- "+proj=longlat +datum=WGS84 +no_defs"
biaspct <- biasr/ max(na.omit(values(biasr)))

# resample
hostsr <- resample(hosts, biasr)
hostspct <- hostsr/ max(na.omit(values(hostsr)))

## bivariate map
# col matrix
col.matrix <- colmat(nquantiles = 10, upperleft = "cyan", upperright = "purple", 
                     bottomleft = "beige", bottomright = "brown1", 
                     xlab = "Hosts number per maximum number of hosts", 
                     ylab = "Sampling rate per maximum sampling rate" )
col.matrix

warning("SLOW")
bivmap <- bivariate.map(hostspct, biaspct, colormatrix = col.matrix, nquantiles = 10)
crs(bivmap) <- crs(hosts)
plot(bivmap, frame.plot = F, axes = F, box = F, add = F, legend = F, col = as.vector(col.matrix))
map(interior = T, add = T)

# -------------------------------------------------------------------------

# data
countries110 <- sf::st_as_sf(countries110)

# ggplot2
bivmap_data <- raster::rasterToPoints(bivmap) %>% 
  tibble::as_tibble()

map_q <- ggplot() +
  geom_raster(data = bivmap_data, aes(x = x, y = y, fill = richnessm)) +
  geom_sf(data = countries110, fill = NA) +
  scale_fill_gradientn(colours = col.matrix, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.5) +
  coord_sf(expand = FALSE, xlim = extent(bivmap)[1:2], ylim = extent(bivmap)[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", pad_x = unit(.1, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.1, "cm"), pad_y = unit(.7, "cm"),
                         style = north_arrow_fancy_orienteering)

# export
ggsave(filename = "Fig02.png", wi = 25, he = 20, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------