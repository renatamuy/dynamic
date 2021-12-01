# Contents:
# Exploring sampbias outcome
# Figure with geom_hex (possibly a panel in Figure 2)
# Figure S9

options(digits = 3, scipen = 999)

source("00_packages.R")
source("01_settings.R")
require(raster)

# Open richness map

setwd(aa)
all_aa <- stack(list.files(pattern = ".tif$"))

setwd(binrasterdir)

all <- stack(list.files(pattern = ".tif$"))

# over accessible area

allm <- mask(all, all_aa)

richness <- sum(allm, na.rm = TRUE) 

# Open bias raster
setwd(projdir)

setwd("sampbias_025dd/")

rbias <- raster("rcar_sarbecovirus_hosts.tif")

crs(rbias) <- CRS(" + init = EPSG:4326")

# Correlation

richness
rbias

rbiasr <- resample(rbias, richness )

plot(rbiasr)

summary(values(rbiasr))

hist(values(richness))

# Plot
rbiasrm <- crop( rbiasr, rbias)
richnessm <- crop( richness, rbiasrm)
plot(rbiasrm, colNA = "blue")
plot(richnessm, colNA = "blue")
richnessmm <-richnessm

richnessmm <- mask(richnessm, rbiasrm)

#plot(richnessmm, colNA = "blue")

# Arrange panel with other panels.. 
rbiasrp <- data.frame(rasterToPoints(rbiasrm) )
richnessp <-data.frame(rasterToPoints(richnessmm))

big <- merge(rbiasrp, richnessp, by = c("x","y"))
head(big)

#Zissou1 ,Moonrise3,#Rushmore1,#Darjeeling1

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

palgrey <- RColorBrewer::brewer.pal(9, "Greys")

figsamp <- ggplot(data = big, 
                  aes(y = layer, x = rcar_sarbecovirus_hosts )) +  
  #geom_point() + 
  #geom_bin2d(bins = 40) + 
  geom_hex(bins = 14) + 
  labs(x = "Sampling rate",y = "Sarbecovirus host species") + 
  scale_fill_gradientn(trans = "log10", colours = palgrey) + 
  #scale_fill_viridis(trans = "log10", option = "cividis") + 
  theme_bw()

figsamp

# Filter by richness > 0
figsampf <- big %>% 
  filter(layer > 0) %>% 
  ggplot(aes(y = layer, 
             x = rcar_sarbecovirus_hosts )) +  
  #geom_point() + 
  geom_hex(bins = 12) + 
  labs(x = "Sampling rate",y = "Sarbecovirus host species") + 
  scale_fill_gradientn(trans = "log10", colours = palgrey) + 
  #scale_fill_viridis(trans = "log10", option = "cividis") + 
  ggtitle( "C." ) + 
  theme_bw() + 
  theme(legend.title = element_blank(), 
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))

figsampf

check <- big %>% 
  filter(layer > 0)

# Discuss 
summary(big$rcar_sarbecovirus_hosts)
summary(check$rcar_sarbecovirus_hosts)

skim(big)
big %>%
  mutate(samprate = ifelse(rcar_sarbecovirus_hosts
                           <= 0.013, "low", "high"), 
         rcar_sarbecovirus_hosts = factor(samprate)) %>%
  janitor::tabyl(samprate)


setwd(projdir)
setwd("hotspots")

figs <- paste0("Fig_sampling_rich", version_suffix, ".jpg")
#ggsave(filename = figs, 
#  gridExtra::grid.arrange(figsamp, nrow = 1))

figsa <- paste0("Fig_sampling_rich_above_zero_F_", version_suffix, ".jpg")
ggsave(filename = figsa , width = 30, height = 26, units = "cm",
       gridExtra::grid.arrange(figsampf, nrow = 1))

# Find pixels with maximum values and countries
map = spData::world %>%
  dplyr::filter(!continent %in% c("South America", "North America", "Antarctica", "Oceania"))

# Russia only will not do; Fiji also crosses the antimeridean...
rossiya <- subset(map, iso_a2 %in% c("RU", "FJ"))
pacified_rossiya <- st_shift_longitude(rossiya)
rest_of_world <- subset(map, !iso_a2 %in% c("RU", "FJ", "FR", "TF")) # removing weird territories
fr <- subset( map, name_long %in% c("France"))
fre <- st_cast(fr,"POLYGON")
france <- fre[2:3,] #REMOVING FRENCH GUIANA......................
map2 <- rbind(pacified_rossiya,
              rest_of_world, france) 

worldmap <- st_collection_extract(map2, "POLYGON")

maxratecountry <- raster::extract( rbiasrm, worldmap, fun = max, na.rm = TRUE, df = TRUE, weights = FALSE, sp = TRUE) 
meanratecountry <- raster::extract( rbiasrm, worldmap, fun = mean, na.rm = TRUE, df = TRUE, weights = FALSE, sp = TRUE) 

require(sp)
require(sf)

maptheme <- theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.title = element_text(size = 10), 
        strip.background = element_blank(),
        strip.text = element_text(size = 16))

palr <- wesanderson::wes_palette("Zissou1", 152, type = "continuous")

head(maxratecountry)
mappolrates <- ggplot() + 
  geom_sf(data = st_as_sf(maxratecountry), 
          aes(fill = rcar_sarbecovirus_hosts ), col = "black", size = 0.3, alpha = 0.94) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160)) + 
  scale_fill_gradientn(colours = pal, 
                       breaks = c(0, 0.01,0.1, 0.2, 0.3, 0.4)) + 
  #scale_fill_viridis_c(option = "inferno", na.value = "mintcream", alpha = .8, breaks = c(5,10,15)) + #trans = "sqrt"
  ggtitle( "E. Maximum sampling rate per nation" ) + 
  maptheme + theme(legend.title = element_blank(), 
                   plot.title = element_text(size = 14, face = "italic"))

mappolrates

# zonal richness

names(richnessmm) <- "sarbeco"

maxpercountry <- raster::extract( richnessmm, worldmap, fun = max, 
                                  na.rm = TRUE, df = TRUE, weights = FALSE, sp = TRUE) 

maxpercountryoriginal <- raster::extract( richnessm, worldmap, 
                                          fun = max, na.rm = TRUE, df = TRUE, weights = FALSE, sp = TRUE) 


head(maxpercountry)
maxpercountry@data %>% arrange(desc(sarbeco)) %>% select(admin, sarbeco)
maxpercountryoriginal@data %>% arrange(desc(layer)) %>% select(admin, layer)
# Different worldmap


maxpercountryoriginaljustp <- raster::extract( richnessm, justp, 
                                               fun = max, na.rm = TRUE, df = TRUE, weights = FALSE, sp = TRUE) 

maxpercountryoriginaljustp@data %>% arrange(desc(layer)) %>% select(name_long, layer)

plot(maxpercountryoriginaljustp)
plot(maxpercountry)
# join with object 

jointable1avg <- left_join(maxpercountry@data, meanratecountry@data)

jointable1avg
table1avgarranged <- jointable1avg %>% arrange(desc(sarbeco))

# max 
jointable1 <- left_join(maxpercountry@data, maxratecountry@data)

jointable1$sarbeco

table1

# Add SD
sdcounties <- raster::extract( rbiasrm, worldmap, fun = sd, na.rm = TRUE, df = TRUE, weights = FALSE, sp = TRUE) 
names(sdcounties)[11] <- "SD_effort"
# Add Median
mediancountries <- raster::extract( rbiasrm, worldmap, fun = median, na.rm = TRUE, df = TRUE, weights = FALSE, sp = TRUE) 
names(mediancountries)[11] <- "median_effort"

jointable1a <- left_join(sdcounties@data, mediancountries@data)
jointable1b <- left_join(table1avgarranged, jointable1a)

head(jointable1b)
# Check columns

# Final table 1
table1 <- jointable1b %>% arrange(desc(sarbeco))

options(digits = 3)
table1clean <- table1 %>% select(country = name_long, 
                                 continent,
                                 subregion, 
                                 pop, area_km2,
                                 hosts = sarbeco,
                                 mean_sampling = rcar_sarbecovirus_hosts, 
                                 median_sampling = median_effort,
                                 SD_sampling = SD_effort )


head(table1clean)

# Exporting Table 1
write.xlsx2(table1clean, file = "Table_1_spData_world_original.xlsx", row.names = FALSE)

require(ggrepel)

ploti <- ggplot(table1clean, aes(mean_sampling, hosts, label = country)) + 
  geom_point(color = "black", alpha = 0) + 
  geom_label_repel() + # geom_label() + 
  labs(title = "", x = "") + theme_bw()

ploti

table1clean %>% ggplot(aes(mean_sampling, hosts, label = continent)) + 
  geom_point(alpha = 0.6) + geom_label_repel(max.overlaps = 20) + # geom_label() + 
  labs(title = "", x = "Mean sampling") + theme_bw()


maxsamplingpercdf <- data.frame(maxratecountry) %>%
  arrange(rcar_sarbecovirus_hosts) %>%
  select(sovereignt, rcar_sarbecovirus_hosts)
# Mean
mappolratesavg <- ggplot() + 
  geom_sf(data = st_as_sf(meanratecountry), 
          aes(fill = rcar_sarbecovirus_hosts ), col = "black", size = 0.3, alpha = 0.94) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160)) + 
  scale_fill_gradientn(colours = pal, 
                       breaks = c(0, 0.01,0.1, 0.2, 0.3, 0.4)) + 
  ggtitle( "D. Average sampling rate per nation" ) + 
  maptheme + theme(legend.title = element_blank(),
                   plot.title = element_text(size = 14, face = "italic"))


mappolratesavg

figratemaps <- paste0("Figure_sampling_rate_maps", version_suffix, ".jpg")
ggsave(filename = figratemaps,
       width = 20, height = 30, units = "cm",
       gridExtra::grid.arrange(mappolrates,mappolratesavg, nrow = 2))

# Better composition (not sure)
figratemapscompo <- paste0("Fig_sampling_rate_maps_compo_", version_suffix, ".jpg")
ggsave(filename = figratemapscompo,
       gridExtra::grid.arrange(arrangeGrob(mappolratesavg, mappolrates, nrow = 2),
                               figsampf, ncol = 2),
       width = 25, height = 20, units = "cm")


# ZONAL BIVARIATE MAPS THAT will not be in the manuscript!
# BIVARIATE CLOROPLETH MAP with tmap

library(tmap)
library(sf)
library(stars)
library(spData)
library(classInt)
library(lattice)
library(grid)
library(pals)

legend_creator = function(col.regions, xlab, ylab, nbins){
  bilegend = levelplot(matrix(1:(nbins * nbins), nrow = nbins),
                       axes = FALSE, col.regions = col.regions,
                       xlab = xlab, ylab = ylab,
                       cuts = 8, colorkey = FALSE, scales = list(draw = 0))
  bilegend
}
add_new_var = function(x, var1, var2, nbins, style = "quantile"){
  class1 = suppressWarnings(findCols(classIntervals(c(x[[var1]]), 
                                                    n = nbins, 
                                                    style = style)))
  
  class2 = suppressWarnings(findCols(classIntervals(c(x[[var2]]), 
                                                    n = nbins, 
                                                    style = style)))
  
  x$new_class = class1 + nbins * (class2 - 1)
  return(x)
}



library(rnaturalearth)
library(WDI)
library(tigris)
head(sdcounties)
testando <- geo_join(sdcounties, mediancountries, "iso_a2", "iso_a2")
bigjoin <- geo_join(maxpercountry, testando, "iso_a2", "iso_a2")
africa<- st_as_sf(bigjoin)

bilegend = legend_creator(stevens.pinkblue(n = 9), 
                          xlab = "sarbeco", 
                          ylab = "median_effort", 
                          nbins = 3)

africa = add_new_var(africa, 
                     var1 = "sarbeco", 
                     var2 = "median_effort", 
                     nbins = 3)

bimap1 = tm_shape(africa) + 
  tm_polygons("new_class", style = "cat", palette = stevens.pinkblue(n = 9)) + 
  tm_layout(legend.show = FALSE) + 
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) + 
  tm_compass(type = "rose", position = c("left", "top"), show.labels = 1, size = 3) + 
  tm_text("name_long")

bimap1
cloro <- paste0("Fig_bivariate_3_", version_suffix, ".tif")

tiff(filename = cloro, width = 40, height = 24, res = 600, units = "cm")

grid.newpage()
print(bimap1)
vp = viewport(x = 0.20, y = 0.20, width = 0.4, height = 0.25)
pushViewport(vp)
vp1 = viewport(x = 1.99, y = 1.3, width = 0.8, height = 0.8)
pushViewport(vp1)
print(bilegend, newpage = FALSE)#position = c(0,0.8,4.75,.75)

dev.off()

# No labels

bimap2 = tm_shape(africa) + 
  tm_polygons("new_class", style = "cat", palette = stevens.pinkblue(n = 9)) + 
  tm_layout(legend.show = FALSE) + 
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) + 
  tm_compass(type = "rose", position = c("left", "top"), show.labels = 1, size = 3)

bimap2
cloro <- paste0("Fig_bivariate_tmap_nolab_", version_suffix, ".tif")

tiff(filename = cloro, width = 40, height = 24, res = 600, units = "cm")

grid.newpage()
print(bimap2)
vp = viewport(x = 0.20, y = 0.20, width = 0.4, height = 0.25)
pushViewport(vp)
vp1 = viewport(x = 1.99, y = 1.3, width = 0.8, height = 0.8)
pushViewport(vp1)
print(bilegend, newpage = FALSE)#position = c(0,0.8,4.75,.75)

dev.off()

# BIVARIATE CLOROPLETH MAP with ggplot
library(biscale)
require(legendMap)

head(bigjoin)
data <- bi_class(st_as_sf(bigjoin), x = sarbeco, y = median_effort, style = "quantile", dim = 3)
data
map <- ggplot() + 
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "gray60", size = 0.1, show.legend = FALSE) + 
  #geom_sf_text(data = data, aes(label = name_long, geometry = geometry) ) + 
  bi_scale_fill(pal = "DkBlue", dim = 3) + 
  labs(
    title = "",
    subtitle = "") + 
  xlab("") + ylab("") + 
  bi_theme() + scale_bar(lon = 130, lat = 20, 
                         distance_lon = 1000, distance_lat = 200, distance_legend = 500, 
                         dist_unit = "km", orientation = TRUE,
                         arrow_length = 500, arrow_distance = 500, arrow_north_size = 3)


map

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher number of hosts",
                    ylab = "Higher sampling proportion",
                    size = 9)

require(cowplot)
finalPlot <- ggdraw() + 
  draw_plot(map, 0, 0, 1, 1) + 
  draw_plot(legend, 0.6, .05, 0.2, 0.2)

clorogg <- paste0("Fig_bivariate_ggplot_NOlab_", version_suffix, ".tif")

#tiff(filename = clorogg, width = 40, height = 24, res = 300, units = "cm")
clorogg <- paste0("Fig_bivariate_ggplot_NOlab_noaxes_", version_suffix, ".png")
png(filename = clorogg, width = 30, height = 24, res = 600, units = "cm")
finalPlot

dev.off()

################ Richness map with standart range color
################ Figure with countries

richnessc <- mask(richness, data)

ridf <- data.frame(rasterToPoints(richnessc))

require(wesanderson)

pal <- wes_palette("Zissou1", 100, type = "continuous")

maptheme <- theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.title = element_text(size = 10), 
        strip.background = element_blank(),
        strip.text = element_text(size = 16))

col.range = c(0,12)

maprichnesscountries <- ggplot() + 
  geom_tile(data = ridf , aes(x = x, y = y, fill = layer) ) + 
  #scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  scale_fill_gradientn(breaks = c(0, 5, 10), 
                       limits = col.range,
                       values = scales::rescale(c(min(ridf$layer),
                                                  median(ridf$layer), 
                                                  max(ridf$layer))), 
                       colours = wes_palette("Zissou1", 100, type = "continuous")) + 
  geom_sf(data = data, fill = NA, col = "grey40", size = 0.3, alpha = 1) + 
  coord_sf(crs = st_crs(crs(data)), ylim = c(-35, 80), xlim = c(-20, 160)) + 
  ggtitle( "Bat species hosts of sarbecovirus" ) + 
  geom_sf_text(data = data, aes(label = name_long, geometry = geometry) ) + 
  xlab("") + ylab("") + 
  scale_bar(lon = 130, lat = 20, 
            distance_lon = 1000, distance_lat = 200, distance_legend = 500, 
            dist_unit = "km", orientation = TRUE,
            arrow_length = 500, arrow_distance = 500, arrow_north_size = 3) + 
  maptheme + 
  theme(legend.title = element_blank(), legend.position = "right",
        plot.title = element_text(size = 14, face = "italic"))

rtoprint <- paste0("Print_richness", ".tif")
tiff(filename = rtoprint, width = 30, height = 24, res = 600, units = "cm")
maprichnesscountries
dev.off()

rtoprint <- paste0("Print_richness", ".png")
png(filename = rtoprint, width = 30, height = 24, res = 600, units = "cm")
maprichnesscountries
dev.off()

############ Now effort sampling proportion
ls()
biasc <- mask(rbiasr, data)

bdf <- data.frame(rasterToPoints(biasc))

require(wesanderson)

pal <- wes_palette("Zissou1", 100, type = "continuous")

col.range = c(0,0.5))
head(bdf)
mapsamplingcountries <- ggplot() + 
  geom_tile(data = bdf , aes(x = x, y = y, fill = rcar_sarbecovirus_hosts) ) + 
  #scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  #scale_fill_gradientn(breaks = c(0,0.1, 0.2, 0.3, 0.4), 
  #     limits = col.range,
  #       colours = wes_palette("Zissou1", 100, type = "continuous")) + 
  scale_fill_viridis() + 
  geom_sf(data = data, fill = NA, col = "grey40", size = 0.3, alpha = 1) + 
  coord_sf(crs = st_crs(crs(data)), ylim = c(-35, 80), xlim = c(-20, 160)) + 
  ggtitle( "Estimated sampling rate" ) + 
  geom_sf_text(data = data, aes(label = name_long, geometry = geometry) ) + 
  xlab("") + ylab("") + 
  scale_bar(lon = 130, lat = 20, 
            distance_lon = 1000, distance_lat = 200, distance_legend = 500, 
            dist_unit = "km", orientation = TRUE,
            arrow_length = 500, arrow_distance = 500, arrow_north_size = 3) + 
  maptheme + 
  theme(legend.title = element_blank(), legend.position = "right",
        plot.title = element_text(size = 14, face = "italic"))

stoprint <- paste0("Print_sampling", ".tif")

tiff(filename = stoprint, width = 30, height = 24, res = 600, units = "cm")
mapsamplingcountries
dev.off()

# Now log10
# #The highest undersampling is shown if you log10 (it will be the lowest negative value, like -12)
bdflog10 <- data.frame(rasterToPoints(log10(biasc)))

summary(bdflog10)

require(wesanderson)

# This won"t be used in the paper, but helps me to understand the bias
#Moonrise3,#Rushmore1,#Darjeeling1

col.range = c(-13.26,0.31))
head(bdf)
mapsamplingcountrieslog10 <- ggplot() + 
  geom_tile(data = bdflog10 , aes(x = x, y = y, fill = layer) ) + 
  #scale_fill_gradientn( colours = wes_palette("Rushmore1", 100, type = "continuous")) + 
  scale_fill_viridis() + 
  geom_sf(data = data, fill = NA, col = "grey40", size = 0.3, alpha = 1) + 
  coord_sf(crs = st_crs(crs(data)), ylim = c(-35, 80), xlim = c(-20, 160)) + 
  ggtitle( "Estimated sampling rate [log10]" ) + 
  geom_sf_text(data = data, aes(label = name_long, geometry = geometry) ) + 
  xlab("") + ylab("") + 
  scale_bar(lon = 130, lat = 20, 
            distance_lon = 1000, distance_lat = 200, distance_legend = 500, 
            dist_unit = "km", orientation = TRUE,
            arrow_length = 500, arrow_distance = 500, arrow_north_size = 3) + 
  maptheme + 
  theme(legend.title = element_blank(), legend.position = "right",
        plot.title = element_text(size = 14, face = "italic"))

mapsamplingcountrieslog10

stoprint <- paste0("Print_sampling_log10", ".tif")

tiff(filename = stoprint, width = 30, height = 24, res = 600, units = "cm")
mapsamplingcountrieslog10
dev.off()


# end ---------------------------------------------------------------------
