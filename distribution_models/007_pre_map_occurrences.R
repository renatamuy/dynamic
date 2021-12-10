##############################################################################################################
# Map with all species
# R 4.1.0 Camp Pontanezen 
# ############################################################################################################

R.version
source('00_packages.R')

setwd('./dynamic_master/')
# large data
df <- read.table('CC_master_bat_data_2021_09_D16.txt', header = TRUE)
# master species control
spl <- read.delim('master.txt')

bat <- data.table(df, longitude = df$long, latitude = df$lat)

head(bat)

coordinates(bat) <- ~ long + lat

projection(bat) <- "+init=epsg:4326"

batsf <- sf::st_as_sf(bat, coords = c("longitude", "latitude"),  crs = "+proj=longlat +ellps=WGS84")

# Making world map and checking coordinates there
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # rnaturalearth (version 0.1.0)

# st_centroid is giving error to simplify for labels, so we have to use another code for thos
#Exporting world shp for ENV
#st_write(world, dsn = "world_rnaturalworld.shp", layer = "world_rnaturalworld.shp", driver = "ESRI Shapefile")

# Generate palette for bat spcies
colourCount = length(unique(batsf$sp))
palette <- distinctColorPalette(colourCount)

#gghighlight(continent == 'Asia') + geom_sf_label(aes(label = name))+

wmap <- ggplot(data = world) +
  geom_sf() +
  geom_point(data= batsf, aes(x = longitude, y = latitude, color = splab), alpha = 1)+
  theme(legend.position="bottom") +
  geom_sf_label(aes(label = name),  alpha=0.5)+
  annotate(geom = "text", x = -90, y = 26, label = "Study region", 
           fontface = "italic", color = "grey22", size = 6) +
  scale_fill_manual(values =palette, name= "Sarbecovirus hosts") +
  coord_sf(xlim = c(-30, 170), ylim = c(-40, 75), expand = FALSE) +
  annotation_scale(location = 'bl', width_hint = 0.1) +
  annotation_north_arrow(location = 'bl', which_north = 'true', pad_x = unit(0.75, 'in'), pad_y = unit(0.5, 'in'), style = north_arrow_fancy_orienteering) + 
  xlab('Long') + ylab('Lat') + ggtitle("") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', size = 0.5), 
        panel.background = element_rect(fill =  'aliceblue'),
        legend.text  = element_text(face = "italic"), legend.position = 'bottom')
wmap

setwd('../')
setwd('dynamic_maps/')
png(filename=paste0('map_points', '.png'), width = 49, height = 30, units = 'cm', res=600 )
wmap
dev.off()


# Whole world
area <- ggplot2::map_data("world", zoom=5) 

batsf$splab <- sub('_', ' ', stringr::str_to_sentence(batsf$sp))

colourCount = length(unique(batsf$splab))
palette <- randomcoloR::distinctColorPalette(colourCount)

g <- ggplot() + theme(panel.grid.major = element_line(color = gray(.5),
                                                        linetype = 'dashed', size = 0.5),
                        panel.background = element_rect(fill =  'aliceblue'),
                        legend.text  = element_text(face = "italic"), legend.position = 'bottom')+
  geom_polygon(data = area,
                             aes(x=long, y = lat, group = group),
                             fill = "grey77", color = "lightgrey", size=0.38) + #Note que voce pode mudar as cores do fundo e da borda
  coord_fixed(1.1) + #Use isto para o mapa ficar proporcional
    geom_point(data = batsf[batsf$splab %in% spl$Master_sp_list,], aes(x = longitude, y = latitude, fill= splab), 
             shape = 21, 
             size = 3, #Tamanho dos pontos
             alpha = 0.6) + #Transparencia: quanto mais proximo de 1, menos transparente
  ggtitle('Bats') + #De nome ao plot, caso seja necessario
  labs(x="Longitude", y = "Latitude") + #De nome aos eixos
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90)) +
  xlab('Long') + ylab('Lat') + ggtitle("") + 
  scale_fill_manual(values =palette, name= "Sarbecovirus hosts") 

g 

filesuf <- 'master_bat_data_2021_07_D26'
png(filename=paste0('worldmap_lbottom_', filesuf, '.png'), width = 49, height = 30, units = 'cm', res=600 )
g
dev.off()
####################################################################################
myd <- batsf[batsf$splab %in% spl$Master_sp_list,]
data.frame(table(myd$splab))

