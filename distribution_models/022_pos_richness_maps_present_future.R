##############################################################################################################
# Supplementary plots (SLOW)
##############################################################################################################

options(digits = 3, scipen = 999)

source('00_packages.R')

source('01_settings.R')

##############################################################################################################
#############################################################################################################

maptheme <- theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, size=14),
        legend.title = element_text(size=10), 
        strip.background = element_blank(),
        strip.text = element_text(size=16))

poly <- rnaturalearth::ne_coastline() # not used for zonal stats

# Open files from ENMs

setwd(aa)
all_aa <- stack(list.files(pattern='.tif$'))

setwd(binrasterdir)

all <- stack(list.files(pattern='.tif$'))

allm <- mask(all, all_aa)

richness <- sum(allm, na.rm=TRUE) 

setwd(projdir)
dir.create('Richness_maps')
setwd('Richness_maps')

values(richness)[values(richness) < 0] = 0

# With NAs
richness2 <- richness 

table(values(richness))
table(values(richness2))
summary(values(richness))
summary(na.omit(values(richness2)))

values(richness2)[values(richness2) == 0] = NA

# Compare with richness maps intersected
##############################################################################################################
# Cutoff (exploring) - Hotspots of sarbecovirus diversity (present) ------------------------------------------

hist(richness2)
cutoff <- 10
r3 <- richness2
values(r3)[values(r3) < cutoff ] = NA  

mypalhot <- viridisLite::turbo(10)

worldmap <- ne_countries(scale = 'medium', returnclass = 'sf')
asia <- worldmap[worldmap$continent == 'Asia',]
ggplot() + geom_sf(data = asia, fill=NA, col="grey40") + theme_bw()
asiac <- st_crop(worldmap, xmin = -20, xmax = 60, ymin = 60, ymax = 130)
asiaf <-fortify(asia, region="id")
h3 <-data.frame(rasterToPoints(r3))

hotspots <- ggplot() +
  geom_sf(data=worldmap, fill=NA, col="grey40", size=0.3, alpha=0.8) +
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-20, 60), xlim = c(60, 130))+
  geom_tile(data=h3 , aes(x=x, y=y, fill=factor(h3[,3]) ) ) + 
  scale_fill_manual(values = rev(mypalhot) )  +
  #geom_sf(data=st_as_sf(asiaf), fill=NA, col="grey40", size=0.3, alpha=0.8) + 
  ggtitle( 'Hotspots of sarbecovirus hosts' )  + 
  #geom_text(data = worldmap, aes(X, Y, label = ID), size = 5)+
  maptheme + 
  theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic")) #+
#scalebar(data = worldmap, dist = 500, dist_unit = "km",   transform = T, model = "WGS84",       border.size = 0.4, st.size = 2) 

hotspots

spol <-sf:::as_Spatial(worldmap$geom)

# Zonal statistics

testando <- raster::extract( r3, worldmap,  fun=sum, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 
head(testando)
##############################################################################################################
test1 <- st_as_sf(testando)
test1
ggplot() +
  geom_sf(data=test1, aes(fill=layer), col="black", size=0.3, alpha=0.8)  +
  scale_fill_viridis_c( na.value= "mintcream", alpha = .4)+  #trans = "sqrt"
  ggtitle( 'Hotspots of sarbecovirus hosts' )  + 
  maptheme + 
  theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic"))

# All species zonal per country
maxpercountry <- raster::extract( richness2, worldmap,  fun=max, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 

# Setting inf to zero
which(maxpercountry$layer == '-Inf')

vv <- max(na.omit(values(richness2))) +1

mypalhotcountry <- viridisLite::turbo( vv )

ggplot() +
  geom_sf(data=st_as_sf(maxpercountry), aes(fill=factor(layer) ), col="black", size=0.3, alpha=0.8)  +
  scale_fill_manual(values = mypalhotcountry )+
  #scale_fill_viridis_c( na.value= "mintcream", alpha = .4)+  #trans = "sqrt"
  ggtitle( 'Hotspots of sarbecovirus hosts' )  + 
  maptheme + 
  theme(legend.title=element_blank(), plot.title=element_text(size=18, face = "italic"))

##############################################################################################################
# Just some continents, and exploding FRENCH guyana france
############### other way

map = spData::world %>%
  dplyr::filter(!continent %in% c("South America", "North America", "Antarctica", "Oceania"))

# Russia only will not do; Fiji also crosses the antimeridean...
rossiya <- subset(map,  iso_a2 %in% c("RU", "FJ"))
pacified_rossiya <- st_shift_longitude(rossiya)
rest_of_world <- subset(map, !iso_a2 %in% c("RU", "FJ", 'FR', 'TF')) # removing weird territories
#plot(rest_of_world)
#plot(subset( map, name_long %in% c('France')))
fr <- subset( map, name_long %in% c('France'))
fre <- st_cast(fr,"POLYGON")
france <- fre[2:3,] #REMOVING FRENCH GUIANA......................

#spatialEco::explode(fr)
map2 <- rbind(pacified_rossiya,
              rest_of_world, france) 

exp <- spatialEco::explode(map2)
checking <- st_polygon(map2)
justp <- st_collection_extract(map2, "POLYGON") # used for zonal stats

##############################################################################################################
# Zonal ######################################################################################################

maxpercountry <- raster::extract( richness2, justp,  fun=max, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 
# Removing inf values

work <- maxpercountry$layer
work[which(work == '-Inf')] <- 0
maxpercountry$sarbeco <- work

##############################################################################################################
# Maximum richness values table

those <- data.frame(table(maxpercountry$name_long, maxpercountry$sarbeco) )

those_tidy <- those %>% as_tibble() %>% 
  filter(Freq > 0) %>% 
  arrange(desc(Var2))

setwd(projdir)

#dir.create('hotspots')
setwd('hotspots')

#write.xlsx(those_tidy, paste0(version_suffix,'max_richness_per_country_AA.xlsx'))

##############################################################################################################
###############################################################################################################
# Continuous cividis
checkc <- st_as_sf(maxpercountry)
class(checkc)
skim(maxpercountry)
require(wesanderson)
pald <-wesanderson::wes_palette("Zissou1", 100, type = "continuous")

mappol <- ggplot() +
  geom_sf(data=st_as_sf(maxpercountry), aes(fill=sarbeco ), col="black", size=0.3, alpha=0.94)  +
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 160))+
  scale_fill_gradientn(colours = pald, breaks=c(5, 10, 15)) + 
  #scale_fill_viridis_c(option="inferno", na.value= "mintcream", alpha = .8, breaks=c(5,10,15))+  #trans = "sqrt"
  ggtitle( 'A. Sarbecovirus hosts per nation' )  + 
  maptheme + theme(legend.title=element_blank(), plot.title=element_text(size=14, face = "italic"))

mappol

# Richness in continuous space--------------------------------------------------------------------------------
##############################################################################################################
rr <-data.frame(rasterToPoints(richness2))

rr$layer <- as.integer(rr$layer)
summary(rr$layer)
pal <- wes_palette("Zissou1", 100, type = "continuous")

mappx <- ggplot() + 
  geom_sf(data=st_as_sf(maxpercountry), fill='white', col="grey40", size=0.3, alpha=1) + #poly #"mintcream"
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-35, 80), xlim = c(-20, 136))+
  ggtitle( 'B. Sarbecovirus hosts' )  + #
  geom_tile(data=rr , aes(x=x, y=y, fill=layer) ) + 
  scale_fill_gradientn(colours = pal, breaks=c(5, 10, 15)) + 
  #scale_fill_viridis_c(option="turbo", na.value= "white", breaks=c(5, 10, 15)) +
  maptheme +  theme(plot.background = element_rect(fill=NA, color=NA),
                    legend.title=element_blank(), plot.title=element_text(size=14, face = "italic")) +
  ylab('Latitude') + xlab('Longitude')
#+ ggspatial::annotation_scale(location="bl", 
 #                                                                   pad_x = unit(1, "cm"), text_cex = 0.5, height = unit(0.2, "cm"))
#ggspatial::annotation_north_arrow(location="bl", height = unit(.75, "cm"),
 #                                 style = ggspatial::north_arrow_orienteering(text_size = 8),
  #                                width = unit(.5, "cm"), pad_y = unit(0.4, "cm")) +

mappx

# Zoom Asia hosts (main one)
mappxasia <- ggplot() +
  geom_sf(data=st_as_sf(maxpercountry), fill='white', col="grey40", size=0.3, alpha=0.9) + #poly #"mintcream"
  coord_sf(crs = st_crs(crs(justp)), ylim = c(-20, 60), xlim = c(70, 127))+
  ggtitle( 'C. Sarbecovirus hosts in Southeast Asia' )  +
  geom_tile(data=rr , aes(x=x, y=y, fill=layer) ) +
  scale_fill_gradientn(colours = pal, breaks=c(5, 10, 15)) +
  maptheme +  theme(plot.background = element_rect(fill=NA, color=NA), legend.title=element_blank(), 
                    plot.title=element_text(size=14, face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

mappxasia

# Exporting fig ----------------------------------------------------------------------------------------------

#fig1n <- paste0('Figure 1_grob_abc_', version_suffix, '.jpg')
#ggsave(filename = fig1n, gridExtra::grid.arrange(arrangeGrob(mappol, mappx, nrow=2),mappxasia, ncol = 2),
#       width = 25, height=20, units='cm')

#ggsave(filename = fig1n, gridExtra::grid.arrange(mappx,mappol, nrow = 2))
# Exporting raster with zeroes 
#writeRaster(richness, filename= 'richness.tif', format= 'GTiff', overwrite=TRUE)

# 2 small one big:
# gridExtra::grid.arrange(arrangeGrob(mappolratesavg, mappolrates, nrow=2),
#figsampf, ncol = 2)

library(grid)
library(gridExtra)
grid.newpage()
mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
print(mappxasia, vp = mainmap)
insetmap <- viewport(width = 0.30, height = 0.3, x = 0.32, y = 0.8)
print(mappx, vp = insetmap) 

#  CLIMATE CHANGE FIGS----------------------------------------------------------------------------------------
setwd(enmresultsdir)
setwd('Projection')
list.files()

recebe <- data.frame()

for(f in list.files() ) {
  print(f)
  
  # Suitability in future
  #setwd(paste0(f,'/Ensemble/W_MEAN/'))
  #suit <- stack(list.files(pattern='.tif'))
  #print(list.files())
  
  # Binary in future
  setwd(enmresultsdir)
  setwd('Projection')
  setwd(paste0(f,'/Ensemble/W_MEAN/MAX_TSS/'))
  pres <- stack(list.files(pattern='.tif'))
  presm <- mask(pres, all_aa)
  
  ##################################################################
  # Exporting spatially restricted files in the future in a folder
  dir.create('range')
  setwd('range')
  filenamefutrange <- 'range.RData'
  save(presm, file = filenamefutrange)
  #######################################################

  richness <- sum(presm, na.rm=TRUE) 

  richness2 <- richness 
  values(richness2)[values(richness2) == 0] = NA
  maxpercountry <- raster::extract( richness2, worldmap,  fun=max, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 
  
  temp <-data.frame(rasterToPoints(richness2))
  temp$facet <- sub(other_suffix, '', f  )
  
  temp$period <- str_extract(f, pattern = periods_fut)
  
  temp$scenario <- str_extract(f, pattern = pathways)
  
  # Important manual setting here if you opt for more GCMs, see: noquote(unique(paste0(gcms, sep='|')) )
  
  temp$gcm <- str_extract(f, pattern = 'BCC-CSM2-MR|CanESM5' )
  
  recebe <- rbind(recebe, temp)
}


recebe %>% group_by(period) %>% skim()

paste0( 'Total number of species to be plotted: ', length(names(presm) )) 

# Slow plots  ################################################################################################
##############################################################################################################

map_future <- ggplot() + 
  geom_sf(data=worldmap, fill="white", col="grey40", size=0.3, alpha=0.8) +
  #coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-38, 60), xlim = c(-10, 130))+ # with aussie
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160))+
  ggtitle( 'Future hotspots of sarbecovirus hosts' )  + 
  geom_tile(data=recebe, aes(x=x, y=y, fill=layer) ) + 
  facet_grid(gcm+scenario~period) + #(period+scenario~gcm)
  scale_fill_gradientn(colours = pal, breaks=c(5, 10, 15) ) +
  maptheme +
  theme(plot.background = element_rect(fill=NA, color=NA), 
        legend.title=element_blank(), 
         plot.title=element_text(size=13, face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

map_future

# R1 map future -----------------------------------------------------------------

recebeb <- recebe %>% filter(!period %in% c("2041-2060", "2061-2080"))
unique(recebeb$period)

figs14 <- ggplot() +
    geom_sf(data=worldmap, fill="white", col="grey40", size=0.3, alpha=0.8) +
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160))+
  ggtitle( 'Future hotspots of sarbecovirus hosts' )  + 
  geom_tile(data=recebeb, aes(x=x, y=y, fill=layer) ) + 
  facet_grid(gcm+scenario~period) +
  scale_fill_gradientn(colours = pal, breaks=c(5, 10, 15) ) +
  maptheme +
  labs(fill='Habitat \n suitability')+
  theme(legend.text = element_text(size=13), 
        legend.position = "bottom", 
        plot.background = element_rect(fill=NA, color=NA), 
            plot.title=element_text(size=13, face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

# Figure S14  --------------------------------------------------------------------
setwd(here())
setwd('hotspots')
figR1_scenarios <- paste0('Figure_S14', version_suffix, '.jpg')
ggsave(filename = figR1_scenarios,figs14, width = 18, height=25, units='cm')

############# Exploring
require(dplyr)

inspecting <- aggregate(layer ~  gcm+scenario+period, data=c, max)

#write.xlsx2(table_avrich, file='max_richness_future.xlsx')

inspecting %>%
  pivot_wider(names_from = "scenario", 
              values_from = "layer")

q <- recebe %>% filter(scenario == 'ssp245', period== '2081-2100') 

table(q$layer) # 1 value

inspectingmed <- aggregate(layer ~ gcm+scenario+period, data=recebe, median)

inspectingmed %>%
  pivot_wider(names_from = "scenario", 
              values_from = "layer")

#end------------------------------------------------------------------------------