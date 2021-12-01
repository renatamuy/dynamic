#' ----
#' Contents
#' Figure: Difference in richness hotspots
#' Figure: Inset in Asia
#' Figure: Inset in Europe
#' Figure: Richness ~ Effort
#' Figure ggridges: Shifts for temperature and hotspots
#' Centroids of richness and temperature shift on their locations in the present and future
#' S9 Figure
#' ----

# prepare r ---------------------------------------------------------------

# directory
setwd(here())

# options
options(digits = 3, scipen = 999)

# source
source("00_packages.R")
source("01_settings.R")

# richness present --------------------------------------------------------

# import species raster
setwd(aa)

all_aa <- raster::stack(dir(pattern = ".tif$"))
all_aa

# import species raster binaries
setwd(binrasterdir)

all <- raster::stack(dir(pattern = ".tif$"))
all

# over accessible area
allm <- raster::mask(all, all_aa)
allm

# richness
richness <- sum(allm, na.rm = TRUE) 
richness

# richness future ---------------------------------------------------------

# directory
setwd(enmresultsdir)
setwd("Projection")

# Implement for all combinations of scenarios, GCMs and periods later
setwd("CanESM5_ssp585_2081-2100_27kms/Ensemble/W_MEAN/MAX_TSS/")

fut <- raster::stack(dir(pattern = ".tif$"))
futm <- raster::mask(fut, all_aa)
richnessf <- sum(futm, na.rm = TRUE) 


# map shift  --------------------------------------------------------------
# shift
rdif <- richnessf - richness
worldmap <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
rdifm <- mask(x = rdif, mask = worldmap)
rdif_df <- data.frame(rasterToPoints(rdifm))
rdif_df$layer <- as.integer(rdif_df$layer)
rpct <- richnessf/richness

plot(rpct) 

# centroids
cdif <- colMeans(xyFromCell(rdifm, which(rdif[] == max(values(rdif)))))
closs <- colMeans(xyFromCell(rdifm, which(rdif[] == min(values(rdif)))))

# Centroid future
cfut <- colMeans(xyFromCell(richnessf, which(richnessf[] == max(values(richnessf)))))

# Centroid present
cfpres <- colMeans(xyFromCell(futm, which(richness[] == max(values(richness)))))
data.frame(t(cfpres))
pfpres <- st_point(t(cfpres))
maptheme <- theme_classic() + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 14), 
        legend.title = element_text(size = 10), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 16))

# scico_palette_show(palettes = c("broc", "cork", "vik", 
#                                 "lisbon", "tofino", "berlin", 
#                                 "batlow", "roma"))

# Difference map, Hotspot movement Figure
pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")
mypali <- viridisLite::viridis(5)

# White palette for Zero
paldifr <- c("#001260" , "#013074" , "#034F88" , 
             "#D6E5EB", "white", "#ECE5D4" , "#A88030", "#8E5905")

paldif <- scico(14, palette = "vik")

mapdifa <- ggplot() + 
  ggtitle( "Shift" ) + #C. Difference between future and present
  geom_tile(data = rdif_df, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "gray60", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(16, 27), xlim = c(95, 106)) + 
  scale_fill_gradientn(colours = paldif, breaks = c(-8, -5, -2, 0 , 2, 5)) + 
  annotate(geom = "text", x = 97.1, y = 19.5, label = "Present hotspot", 
           fontface = "italic", color = "grey22", size = 6) + 
  annotate(geom = "text", x = 100.8, y = 20.8, label = "Future hotspot", 
           fontface = "italic", color = "grey22", size = 6) + 
  annotate(geom = "text", x = 97.1, y = 20.4, label = ".", 
           fontface = "bold", color = "grey22", size = 20) + 
  annotate(geom = "text", x = 100.8, y = 20.7, label = ".", 
           fontface = "bold", color = "grey22", size = 20) + 
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.1, "cm"), pad_y = unit(.7, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(x = "Longitude", y = "Latitude", fill = "Host number \nshift in 2100", title = "") + 
  theme_bw(base_size = 15) + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 16) )
mapdifa

# export
setwd(projdir)
setwd("hotspots")
figcentroids <- paste0("Figure_4_inset_", version_suffix, ".png")
ggsave(filename = figcentroids, plot = mapdifa,
       width = 25, height = 22, units = "cm", dpi = 600)


# map present --------------------------------------------------------

# richness
richnessm <- mask(x = richness, mask = worldmap)

# map 
rdf <- data.frame(rasterToPoints(richnessm))
rdf$layer <- as.integer(rdf$layer)
rdf

col.range = c(0, 12)

maprdf <- ggplot() + 
  ggtitle( "A. Current hotspots" ) + 
  geom_tile(data = rdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-15, 50), xlim = c(70, 136)) + #poly #"mintcream"
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  labs(x = "Longitude", y = "Latitude", fill = "Potential \richness") + 
  theme_bw(base_size = 15) + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 15)  ) 
maprdf

# map present zoom ----
zpresent <- ggplot() + 
  ggtitle( "Present" ) + #C. Difference between future and present
  geom_tile(data = rdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(16, 27), xlim = c(95, 106)) + 
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  #annotate(geom = "text", x = 97.1, y = 20.4, label = "Present hotspot", 
  #  fontface = "italic", color = "grey22", size = 6) + 
  #annotate(geom = "text", x = 100.8, y = 20.7, label = "Future hotspot", 
  #  fontface = "italic", color = "grey22", size = 6) + 
  #annotate(geom = "text", x = 97.1, y = 20.4, label = ".", 
  #  fontface = "bold", color = "grey22", size = 20) + 
  #annotate(geom = "text", x = 100.8, y = 20.7, label = ".", 
  #  fontface = "bold", color = "grey22", size = 20) + 
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.1, "cm"), pad_y = unit(.7, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(x = "Longitude", y = "Latitude", fill = "Potential \nrichness") + 
  theme_bw(base_size = 15) + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 16) )
zpresent


# plot future map ---------------------------------------------------------

# richness
richnessfm <- mask(x = richnessf, mask = worldmap)

# map
rfdf <-data.frame(rasterToPoints(richnessfm))
rfdf$layer <- as.integer(rfdf$layer)
rfdf

mapfuture <- ggplot() + 
  ggtitle( "B. Future hotspots" ) + 
  geom_tile(data = rfdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-15, 50), xlim = c(70, 136)) + #poly #"mintcream"
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  labs(x = "Longitude", y = "Latitude", fill = "Potential \nrichness") + 
  theme_bw(base_size = 15) + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 15) ) 
mapfuture

# zoom in future ----
zfuture <- ggplot() + 
  ggtitle( "Future" ) + #C. Difference between future and present
  geom_tile(data = rfdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(16, 27), xlim = c(95, 106)) + 
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  #annotate(geom = "text", x = 97.1, y = 20.4, label = "Present hotspot", 
  #  fontface = "italic", color = "grey22", size = 6) + 
  #annotate(geom = "text", x = 100.8, y = 20.7, label = "Future hotspot", 
  #  fontface = "italic", color = "grey22", size = 6) + 
  #annotate(geom = "text", x = 97.1, y = 20.4, label = ".", 
  #  fontface = "bold", color = "grey22", size = 20) + 
  #annotate(geom = "text", x = 100.8, y = 20.7, label = ".", 
  #  fontface = "bold", color = "grey22", size = 20) + 
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.1, "cm"), pad_y = unit(.7, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(x = "Longitude", y = "Latitude", fill = "Potential \nrichness") + 
  theme_bw(base_size = 15) + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 16) )
zfuture

# export ------------------------------------------------------------------

# Slow to run!
setwd(here())
setwd("hotspots")

figcomplarge <- paste0("Figure_4_world_", version_suffix, ".png")
gridExtra::grid.arrange(maprdf, mapfuture, mapdifa, ncol = 3)
ggsave(filename = figcomplarge, width = 35, height = 29, units = "cm", dpi = 600, 
       gridExtra::grid.arrange(maprdf, mapfuture, ncol = 2))

figcompz <- paste0("Figure_4_zoom_", version_suffix, ".png")
ggsave(filename = figcompz, width = 38, height = 29, units = "cm", dpi = 600, 
       gridExtra::grid.arrange(zpresent, zfuture, mapdifa, ncol = 3))

figcomp2 <- paste0("Fig_4_zoom_PRESENT_FUTURE_", version_suffix, ".png")
ggsave(filename = figcomp2, width = 34, height = 28, units = "cm", dpi = 600, 
       gridExtra::grid.arrange(zpresent, zfuture, ncol = 2))


# Exporting Masked rasters of hotsposts
# writeRaster(richnessm, filename = "richnessm.tif", format = "GTiff", 
#             overwrite = TRUE, progress = "text", options=c("COMPRESS=NONE", "TFW=YES"))

# writeRaster(richnessfm, filename = "richnessfm.tif", format = "GTiff", 
#             overwrite = TRUE, progress = "text", options=c("COMPRESS=NONE", "TFW=YES"))


# europe frame ------------------------------------------------------------

# map
mapeurope <- ggplot() + 
  ggtitle( "A" ) + 
  geom_tile(data = rdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(34, 72), xlim = c(-20, 50)) + #poly #"mintcream"
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  labs(fill = "Potential \nrichness", x = "Longitude", y = "Latitude") + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "italic")) + 
  theme_bw()
mapeurope

mapfuteurope <- ggplot() + 
  ggtitle( "B" ) + 
  geom_tile(data = rfdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(34, 72), xlim = c(-20, 50)) + #poly #"mintcream"
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  labs(fill = "Potential \nrichness", x = "Longitude", y = "Latitude") + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "italic")) + 
  theme_bw()
mapfuteurope

# SEAsia frame present
mapseasia <- ggplot() + 
  ggtitle( "C" ) + 
  geom_tile(data = rdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), c(-10, 50), xlim = c(70, 126)) + 
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  labs(fill = "Potential \nrichness", x = "Longitude", y = "Latitude") + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "italic")) + 
  theme_bw()
mapseasia

# SEAsia frame future
mapfutseasia <- ggplot() + 
  ggtitle( "D" ) + 
  geom_tile(data = rfdf, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), c(-10, 50), xlim = c(70, 126)) + 
  scale_fill_gradientn(colours = pal, breaks = c(0, 5, 10), limits = col.range) + 
  labs(fill = "Potential \nrichness", x = "Longitude", y = "Latitude") + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        legend.title = element_blank(), 
        plot.title = element_text(size = 14, face = "italic")) + 
  theme_bw()
mapfutseasia


# Exporting zooms with same scale for legend
figcomposquare <- paste0("Fig_4_europe_SEasia_", version_suffix, ".png")
ggsave(filename = figcomposquare, width = 36, height = 36, units = "cm", dpi = 600, 
       gridExtra::grid.arrange(mapseasia, mapfutseasia, mapeurope, mapfuteurope, ncol = 2, nrow = 2))


# world difs stats and world map ------------------------------------------

rdif_df$hosts <- as.factor(rdif_df$layer)
(18 + 166 + 463 + 1006 + 1552 + 1551 + 1983 + 5443) / sum(table(rdif_df$hosts) )
#2.33X times species loss where changes happened
(18 + 166 + 463 + 1006 + 1552 + 1551 + 1983 + 5443) / (4605 + 484 + 134 + 11) 
(4605 + 484 + 134 + 11) /(18 + 166 + 463 + 1006 + 1552 + 1551 + 1983 + 5443) 
# 3.5 % species loss

# World figure

#palcate <- c("black", "navy", scico(12, palette = "vik"))
# Aux plot to manually fix color scale
# plot(1:20, col = paldifr, pch = 19, cex = 9)

mapdifw <- ggplot() + 
  ggtitle( "" ) + #B. Difference between future and present
  geom_tile(data = rdif_df, aes(x = x, y = y, fill = layer), alpha = 0.8) + 
  geom_sf(data = st_as_sf(worldmap), fill = NA, col = "grey40", size = 0.4) + 
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160)) + 
  scale_fill_gradientn(colours = paldif, breaks = c(-8, -5, -2, 0 , 2, 5)) + #
  geom_rect(data = data.frame(), 
            aes(xmin = 95, xmax = 106, ymin = 16, ymax = 27), 
            colour = "black", fill = NA) + 
  labs(fill = "Host number \n shift", x = "Longitude", y = "Latitude") + 
  theme_bw(base_size = 16) + 
  theme(plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 16) , legend.position = "none")
mapdifw

figworldbrown <- paste0("Fig_world_spshift_BROWN_", version_suffix, ".png")
ggsave(filename = figworldbrown, plot = mapdifw,
       width = 30, height = 22, units = "cm", dpi = 600)

gridExtra::grid.arrange(mapdifw, mapdifa, ncol = 2)
figshift <- paste0("Fig_world_inset_shift_poster_BROWN_", version_suffix, ".png")
ggsave(filename = figshift, width = 42, height = 24, units = "cm", dpi = 600, 
       gridExtra::grid.arrange(mapdifw, mapdifa, ncol = 2))


# are hotspots getting hotter ---------------------------------------------

# directory
setwd(projdir)
setwd("rasters_temp_forest")

# 
t <- raster("bio_1.tif")
tfut <- raster("bio_1_2100_SSP585.tif")
tfut <- resample(tfut, rdifm )
tdif <- tfut - t

# Hotspots getting hotter
cfpres
cfut

# Hotspot shift
cellFromXY(t, cfpres)
t[401429]

cellFromXY(t, cfut)
t[400004]

# Forest and hotspots
forest <- raster("CMIP6_Land_Use_Harmonization_primf_2015.tif")
forestfut <- raster("CMIP6_Land_Use_Harmonization_primf_SSP5_85_2100.tif")

# Myanmar
cellFromXY(forest, cfpres)
forest[401429]

cellFromXY(forestfut, cfut)
# TYPE 20.7, 100.8 IN GOOGLE MAPS
forestfut[400004]

# Are the hotspots getting hotter?
# Yes

zr <- data.frame(rasterToPoints(richnessm))
zt <- data.frame(rasterToPoints(t))

zz <- left_join(zr, zt )
zz$time <- "present"

plot(richnessf)
max(rdifm)

zfr <- data.frame(rasterToPoints(richnessf))
zft <- data.frame(rasterToPoints(tfut))

zzf <- left_join(zfr, zft )
zzf$time <- "future"

head(zz)
colnames(zzf) <- c( "x" , "y" , "layer", "bio_1", "time")
zzz <- rbind(zz, zzf)

# mean + /- SD 
p <- ggplot(zzz, aes(x = layer, y = bio_1, fill = factor(layer) )) + 
  geom_boxplot() # + scale_color_grey() 
p

p + stat_summary(fun.data = mean_sdl, mult = 1, 
                 geom = "pointrange") + coord_flip() + 
  theme_classic()

# distribution
zzz %>% 
  filter(layer > 0) %>% 
  mutate(time = str_to_title(time)) %>% 
  ggplot(aes(x = bio_1, y = factor(layer), fill = time)) + 
  geom_density_ridges(aes(color = time, fill = time), alpha = .7) + 
  labs(x = "Average temperature", y = "Species number", color = "Time", fill = "Time") + 
  theme_bw(base_size = 15)

# end ---------------------------------------------------------------------