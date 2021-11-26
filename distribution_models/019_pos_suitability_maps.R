##############################################################################################################
# Richness maps and Suitability maps 
# Warning: slow!
##############################################################################################################

options(digits = 3, scipen = 999)

source('00_packages.R')

source('01_settings.R')

##############################################################################################################

maptheme <- theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, size=14),
        legend.title = element_text(size=10), 
        strip.background = element_blank(),
        strip.text = element_text(size=16))

poly <- rnaturalearth::ne_coastline() 

##############################################################################################################
# Just some continents, and exploding FRENCH guyana france
############### other way

map = spData::world %>%
  dplyr::filter(!continent %in% c("South America", "North America", "Antarctica"))

# Russia only will not do; Fiji also crosses the antimeridean...
require(sf)
rossiya <- subset(map,  iso_a2 %in% c("RU", "FJ"))
pacified_rossiya <- st_shift_longitude(rossiya)
rest_of_world <- subset(map, !iso_a2 %in% c("RU", "FJ", 'FR', 'TF')) # removing weird territories
#plot(rest_of_world)
#plot(subset( map, name_long %in% c('France')))
fr <- subset( map, name_long %in% c('France'))
fre <- st_cast(fr,"POLYGON")
france <- fre[2:3,] #REMOVING FRENCH GUIANA......................

map2 <- rbind(pacified_rossiya,
              rest_of_world, france) 

exp <- spatialEco::explode(map2)
justp <- st_collection_extract(map2, "POLYGON")

##############################################################################################################
# Exporting maps per species 
##############################################################################################################
setwd(aa)
all_aa <- stack(list.files(pattern='.tif$'))

setwd(binrasterdir)
setwd('../')

allsuit <- stack(list.files(pattern= '.tif'))

# over

allsuitm <- mask(allsuit, all_aa)

bigo <- as.data.frame(allsuitm, xy = TRUE) %>%
  reshape2::melt(id.vars = c('x','y'))

# All in one

bigo$labi <- sub('_',' ' , str_to_sentence( bigo$variable    ))  

mypal <- viridisLite::turbo(200)

bigo1 <- bigo %>% filter(value > 0 )

bigfig <-   ggplot() +  geom_sf(data=st_as_sf(justp), fill='white', col="grey40", size=0.3, alpha=1) + #poly #"mintcream"
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 150))+
  ggtitle( 'Habitat suitability for sarbecovirus hosts' )  + 
  geom_tile(data=bigo1 , aes(x=x, y=y, fill=value) ) +
  facet_wrap(~ labi, nrow= 4)+
  scale_fill_viridis_c(option="turbo", na.value= "white") + 
  maptheme + theme(legend.position="bottom", plot.background = element_rect(fill=NA, color=NA), 
                    legend.title=element_blank(), plot.title=element_text(size=16, face = "bold"),
                    strip.text = element_text(size=12, face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

bigfig

# Exporting figure ###########################################################################################
namefigall <- paste0(version_suffix, "_suit.jpg")
setwd(projdir)
setwd("dynamic_maps/")
jpeg(filename = namefigall, res=600, width = 44, height = 32, units = "cm" )
print(bigfig)
dev.off()

# IUCN intersected data

setwd(aai)

all_aai <- stack(list.files(pattern='.tif$'))

setwd(binrasterdiri)

setwd('../')

allsuiti <- stack(list.files(pattern= '.tif'))

allsuitmi <- mask(allsuiti, all_aai) #intersected and over accessible area

bigoi <- as.data.frame(allsuitmi, xy = TRUE) %>%
  reshape2::melt(id.vars = c('x','y'))

# All in one
require(stringr)

bigoi$labi <- sub('_',' ' , str_to_sentence( bigoi$variable    ))  

mypali <- viridisLite::turbo(200)

bigo1i <- bigoi %>% filter(value > 0 )

bigfigi <- ggplot() +  geom_sf(data=st_as_sf(justp), fill='white', col="grey40", size=0.3, alpha=1) + 
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 150))+
  ggtitle( 'Habitat suitability for sarbecovirus hosts - IUCN intersected data' )  + 
  geom_tile(data=bigo1i , aes(x=x, y=y, fill=value) ) +
  facet_wrap(~ labi, nrow=4)+
  scale_fill_viridis_c(option="turbo", na.value= "white") +
  maptheme +  theme(legend.position="bottom", plot.background = element_rect(fill=NA, color=NA), 
                    legend.title=element_blank(), plot.title=element_text(size=16, face = "bold"),
                    strip.text = element_text(size=12, face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

bigfigi

namefigalli <- paste0(versioni, "_suit_iucn_intersected.jpg")
setwd(projdir)
setwd("dynamic_maps/")
jpeg(filename = namefigalli, res=600, width = 44, height = 32, units = "cm" )
print(bigfigi)
dev.off()
#########################################################################################