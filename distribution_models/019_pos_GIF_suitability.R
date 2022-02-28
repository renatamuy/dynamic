##############################################################################################################
# Richness maps and Suitability maps 
# Warning: slow!
##############################################################################################################

options(digits = 3, scipen = 999)

source("00_packages.R")
source("01_settings.R")

##############################################################################################################

maptheme <- theme_classic() + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 14), 
        legend.title = element_text(size = 10), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 16))

poly <- rnaturalearth::ne_coastline() 

##############################################################################################################
# Just some continents, and exploding FRENCH guyana france
############### other way

map <- spData::world %>%
  dplyr::filter(!continent %in% c("South America", "North America", "Antarctica"))

require(sf)
rossiya <- subset(map, iso_a2 %in% c("RU", "FJ"))
pacified_rossiya <- st_shift_longitude(rossiya)
rest_of_world <- subset(map, !iso_a2 %in% c("RU", "FJ", "FR", "TF")) # removing weird territories
fr <- subset(map, name_long %in% c("France"))
fre <- st_cast(fr, "POLYGON")
france <- fre[2:3, ] #REMOVING FRENCH GUIANA

map2 <- rbind(pacified_rossiya, 
              rest_of_world, france) 

exp <- spatialEco::explode(map2)
justp <- st_collection_extract(map2, "POLYGON")

##############################################################################################################
# Exporting maps per species 
##############################################################################################################

# IUCN not-intersected data 

setwd(aa)
all_aa <- stack(list.files(pattern = ".tif$"))

setwd(binrasterdir)
setwd("../")

allsuit <- stack(list.files(pattern = ".tif"))

# over

allsuitm <- mask(allsuit, all_aa)

bigo <- as.data.frame(allsuitm, xy = TRUE) %>%
  reshape2::melt(id.vars = c("x", "y"))

# All in one -------------------------------------------------------------------------------------------------

bigo$labi <- sub("_", " " , str_to_sentence(bigo$variable)) 

mypal <- viridisLite::turbo(200)

bigo1 <- bigo %>% filter(value > 0)

bigo1$genus <- gsub(' .*$', '', bigo1$labi)

bigfig <- ggplot() + 
  geom_sf(data = st_as_sf(justp), fill = "white", col = "grey40", size = 0.3, alpha = 1) + 
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 150))+
  ggtitle("Habitat suitability for sarbecovirus hosts") + 
  geom_tile(data = bigo1 , aes(x = x, y = y, fill = value)) +
  facet_wrap(~ labi, nrow = 4)+
  scale_fill_viridis_c(option = "turbo", na.value = "white") + 
  maptheme + theme(legend.position = "bottom", plot.background = element_rect(fill = NA, color = NA), 
                   legend.title = element_blank(), plot.title = element_text(size = 16, face = "bold"), 
                   strip.text = element_text(size = 12, face = "italic")) +
  ylab("Latitude") + xlab("Longitude")

bigfig

# Exporting figure ###########################################################################################
namefigall <- paste0(version_suffix, "_suit_not_intersected.jpg")
setwd(here())
setwd("dynamic_maps/")
jpeg(filename = namefigall, res = 600, width = 44, height = 32, units = "cm")
print(bigfig)
dev.off()

# Animation --------------------------------------------------------------------------------------------------

setwd(here())
setwd('dynamic_maps')
require(gganimate)

start <- print(Sys.time())

anim <- ggplot() + geom_sf(data = st_as_sf(justp), fill = "white", col = "grey40", size = 0.3, alpha = 1) + 
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 150))+
  geom_tile(data = bigo1 , aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = "turbo", na.value = "white") + 
  theme_bw(base_size = 18) + 
  theme(legend.text = element_text(size=11), legend.position = "bottom", plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 18, face = "italic")) + #legend.text = element_text(size=14), legend.title = element_text(size=14)
  annotation_scale(location = "bl", pad_x = unit(.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.15, "cm"), pad_y = unit(.7, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill = "Habitat \nsuitability", title = 'Species: {closest_state}', x ='Longitude', y='Latitude' ) +
  transition_states(labi,
                    state_length = 2,
                    transition_length = 1)

animate(anim,
        fps = 10,
        nframes = 400,
        end_pause = 20,
        rewind = FALSE,
        width = 1000,
        height = 900)

end <- print(Sys.time())
time_to_render_animation <- end-start
time_to_render_animation

#gifski::save_gif("gifski_suit_not_intersected.gif",  
#                 animate(anim,
#                         fps = 10,
#                         nframes = 400,
#                         end_pause = 20,
#                         rewind = FALSE,
#                         width = 1000,
#                         height = 900)
#                 , 
#                width = 1000,
#                 height = 900, 
#                 delay=2, 
#                 loop=TRUE)

# individual maps --------------------------------------------------------------------------------------------
bats <- unique(bigo1$labi)

setwd(here())

dir.create('_gif_frames_ni') # ni = not intersected

for(b in bats ){
  setwd(here())
  setwd('_gif_frames_ni')
  
  print(b)
  temp <- bigo1[bigo1$labi == b,]
  
  spfig <- ggplot() + 
    geom_sf(data = st_as_sf(justp), fill = "grey88", col = "grey40", size = 0.3, alpha = 1) + 
    coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 150))+
    ggtitle(paste0(b)) + 
    geom_tile(data = temp , aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "turbo", na.value = "white") + 
    theme_bw() +
    theme(legend.position = "bottom",  plot.background = element_rect(fill = NA, color = NA),
          legend.title = element_text("Habitat suitability"), plot.title = element_text(size = 16, face = "italic") ) +
    ylab("Latitude") + xlab("Longitude") +
    annotation_scale(location = "bl", pad_x = unit(.1, "cm")) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(.1, "cm"), pad_y = unit(.7, "cm"),
                           style = north_arrow_fancy_orienteering)+
    labs(fill = "Habitat \nsuitability")
  
  nametemp <- paste0(b, "_ni.jpg")
  jpeg(filename = nametemp, res = 400, width = 34, height = 22, units = "cm")
  print(spfig)
  dev.off()
}

#-------------------------------------------------------------------------------------------------------------

# IUCN intersected data ######################################################################################
setwd(here())
setwd(aai)

all_aai <- stack(list.files(pattern = ".tif$"))

setwd(binrasterdiri)

setwd("../")

allsuiti <- stack(list.files(pattern = ".tif"))

allsuitmi <- mask(allsuiti, all_aai) #intersected and over accessible area

bigoi <- as.data.frame(allsuitmi, xy = TRUE) %>%
  reshape2::melt(id.vars = c("x", "y"))

# All in one - IUCN intersected data
require(stringr)

bigoi$labi <- sub("_", " " , str_to_sentence(bigoi$variable)) 

mypali <- viridisLite::turbo(200)

bigo1i <- bigoi %>% filter(value > 0)

bigfigi <- ggplot() + geom_sf(data = st_as_sf(justp), fill = "white", col = "grey40", size = 0.3, alpha = 1) + 
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 150))+
  ggtitle("Habitat suitability for sarbecovirus hosts - IUCN intersected data") + 
  geom_tile(data = bigo1i , aes(x = x, y = y, fill = value)) +
  facet_wrap(~ labi, nrow = 4)+
  scale_fill_viridis_c(option = "turbo", na.value = "white") +
  maptheme + theme(legend.position = "bottom", plot.background = element_rect(fill = NA, color = NA), 
                   legend.title = element_blank(), plot.title = element_text(size = 16, face = "bold"), 
                   strip.text = element_text(size = 12, face = "italic")) +
  ylab("Latitude") + xlab("Longitude")

bigfigi

namefigalli <- paste0(versioni, "_suit_iucn_intersected.jpg")
setwd(here())
setwd("dynamic_maps/")
jpeg(filename = namefigalli, res = 600, width = 44, height = 32, units = "cm")
print(bigfigi)
dev.off()

# Animation - IUCN intersected data --------------------------------------------------------------------------
setwd(here())
setwd('dynamic_maps')

start <- print(Sys.time())

animi <- ggplot() + geom_sf(data = st_as_sf(justp), fill = "white", col = "grey40", size = 0.3, alpha = 1) + 
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 150))+
  geom_tile(data = bigo1i, aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = "turbo", na.value = "white") + 
  theme_bw(base_size = 18) + 
  theme(legend.text = element_text(size=11), legend.position = "bottom", plot.background = element_rect(fill = NA, color = NA), 
        plot.title = element_text(size = 18, face = "italic")) + # legend.title = element_text(size=14)
  annotation_scale(location = "bl", pad_x = unit(.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.15, "cm"), pad_y = unit(.7, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill = "Habitat \nsuitability", title = 'Species: {closest_state}', x ='Longitude', y='Latitude' ) +
  transition_states(labi,
                    state_length = 2,
                    transition_length = 1)

animate(animi,
        fps = 10,
        nframes = 400,
        end_pause = 20,
        rewind = FALSE,
        width = 1000,
        height = 900)

end <- print(Sys.time())
time_to_render_animation <- end-start
time_to_render_animation

#########################################################################################