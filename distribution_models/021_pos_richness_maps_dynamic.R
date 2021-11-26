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

# Open files from ENMs
setwd(binrasterdir)

all <- stack(list.files(pattern='.tif'))

richness <- sum(all, na.rm=TRUE) 

plot(richness)


setwd(projdir)
#dir.create('Richness_maps')
setwd('Richness_maps')

values(richness)[values(richness) < 0] = 0

# With NAs
richness2 <- richness 

table(values(richness))
table(values(richness2))
summary(values(richness))
summary(na.omit(values(richness2)))

values(richness2)[values(richness2) == 0] = NA

levelplot(richness2, cuts=6, col.regions= wes_palette(9, name='FantasticFox1', type='continuous') )

##############################################################################################################
# Cutoff of more than 10 sp
# Hotspots of sarbecovirus diversity (present)
##############################################################################################################

r3 <- richness2

# Select cutoff value. If you can modell all species, go for 10
values(r3)[values(r3) < 6 ] = NA  

mypalhot <- viridisLite::turbo(8)

worldmap <- ne_countries(scale = 'medium', returnclass = 'sf')
asia <- worldmap[worldmap$continent == 'Asia',]
ggplot() + geom_sf(data = asia, fill=NA, col="grey40") + theme_bw()

asiac <- st_crop(worldmap, xmin = -20, xmax = 60, ymin = 60, ymax = 130)

asiaf <-fortify(asia, region="id")

head(worldmap)

h3 <-data.frame(rasterToPoints(r3))

hotspots <- ggplot() +
  geom_sf(data=worldmap, fill=NA, col="grey40", size=0.3, alpha=0.8) +
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-20, 60), xlim = c(60, 130))+
  geom_tile(data=h3 , aes(x=x, y=y, fill=factor(h3[,3]) ) ) + 
  scale_fill_manual(values = mypalhot )  +
  #geom_sf(data=st_as_sf(asiaf), fill=NA, col="grey40", size=0.3, alpha=0.8) + 
  ggtitle( 'Hotspots of sarbecovirus hosts' )  + 
  #geom_text(data = worldmap, aes(X, Y, label = ID), size = 5)+
  maptheme + 
  theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic")) #+
#scalebar(data = worldmap, dist = 500, dist_unit = "km",
   #        transform = T, model = "WGS84",
    #       border.size = 0.4, st.size = 2) 

hotspots
crs(r3)

class(worldmap)
spol <-sf:::as_Spatial(worldmap$geom)

##############################################################################################################
# Zonal statistics

testando <- raster::extract( r3, worldmap,  fun=sum, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 

testando <- raster::extract( r3, worldmap,  fun=max, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 

head(testando)
##############################################################################################################
# Check
test1 <- st_as_sf(testando)

st_crs(crs(test1)) 

test1
ggplot() +
  geom_sf(data=test1, aes(fill=layer), col="black", size=0.3, alpha=0.8)  +
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-30, 80), xlim = c(-20, 181))+
  scale_fill_viridis_c( na.value= "mintcream", alpha = .4)+  #trans = "sqrt"
  ggtitle( 'Hotspots of sarbecovirus hosts' )  + 
  maptheme + 
  theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic"))
  
  #scale_fill_manual(values = rev(mypalhot) ) 

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
  dplyr::filter(!continent %in% c("South America", "North America", "Antarctica"))

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
justp <- st_collection_extract(map2, "POLYGON")

##############################################################################################################
# Zonal ######################################################################################################

maxpercountry <- raster::extract( richness2, justp,  fun=max, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 
# Removing inf values

work <- maxpercountry$layer
work[which(work == '-Inf')] <- 0
maxpercountry$sarbeco <- work

##############################################################################################################
# Maximum table
those <- data.frame(table(maxpercountry$name_long, maxpercountry$sarbeco) )

those_tidy <- those %>% as_tibble() %>% 
  filter(Freq > 0) %>% 
  arrange(desc(Var2))

setwd(projdir)
#dir.create('hotspots')
setwd('hotspots')

write.xlsx(those_tidy, paste0(version_suffix,'max_richness_per_country_40ocmin.xlsx' ))


##############################################################################################################
# Discrete
head(maxpercountry$sarbeco)
table(maxpercountry$continent)

ggplot() +
  geom_sf(data=st_as_sf(maxpercountry), aes(fill=factor(sarbeco) ), col="black", size=0.3, alpha=0.8)  +
  scale_fill_manual(values = mypalhotcountry , na.value= "mintcream")+
  ggtitle( 'Hotspots of sarbecovirus hosts' )  + 
  maptheme + 
  theme(legend.title=element_blank(), plot.title=element_text(size=18, face = "italic")) 

###############################################################################################################
# Continuous cividis
checkc <- st_as_sf(maxpercountry)
class(checkc)
skim(maxpercountry)

ggplot() + geom_sf(data=checkc, aes(fill=sarbeco ), col="black", size=0.3, alpha=0.94)

pald <- wes_palette("Zissou1", 100, type = "continuous")
mappol <- ggplot() +
  geom_sf(data=st_as_sf(maxpercountry), aes(fill=sarbeco ), col="black", size=0.3, alpha=0.94)  +
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160))+
  scale_fill_gradientn(colours = pal, breaks=c(5, 10, 15)) + 
  #scale_fill_viridis_c(option="inferno", na.value= "mintcream", alpha = .8, breaks=c(5,10,15))+  #trans = "sqrt"
  ggtitle( 'B. Hotspots of sarbecovirus hosts per nation' )  + 
  maptheme + theme(legend.title=element_blank(), plot.title=element_text(size=13, face = "italic"))

mappol
# Richness in continuous space
##############################################################################################################
rr <-data.frame(rasterToPoints(richness2))

#ylim = c(-20, 60), xlim = c(60, 130)

rr$layer <- as.integer(rr$layer)
summary(rr$layer)
pal <- wes_palette("Zissou1", 100, type = "continuous")


mappx <- ggplot() + 
  geom_sf(data=st_as_sf(maxpercountry), fill='white', col="grey40", size=0.3, alpha=1) + #poly #"mintcream"
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160))+
  ggtitle( 'A. Hotspots of sarbecovirus hosts' )  + 
  geom_tile(data=rr , aes(x=x, y=y, fill=layer) ) + 
  scale_fill_gradientn(colours = pal, breaks=c(5, 10, 15)) + 
  #scale_fill_viridis_c(option="turbo", na.value= "white", breaks=c(5, 10, 15)) +
   maptheme +  theme(plot.background = element_rect(fill=NA, color=NA), legend.title=element_blank(), plot.title=element_text(size=13, face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

mappx
#geom_rect(aes(ymin=-90, ymax=90, xmin=-180, xmax=180), fill=NA, color="black", size=0.5) +

# Exporting fig

fig1n <- paste0('Figure 1_AB_', version_suffix, '.jpg')

ggsave(filename = fig1n, gridExtra::grid.arrange(mappx,mappol, nrow = 2))

# Exporting raster with zeroes 
#writeRaster(richness, filename= 'richness.tif', format= 'GTiff', overwrite=TRUE)

# Non restricted

setwd(nonrestrictedrasters)

allnr <- stack(list.files(pattern='.tif'))

richnessnr <- sum(allnr, na.rm=TRUE)

print(c('If you dont restrict them spatially, there would be many more species up to', max(values(richnessnr)) ))

plot(richnessnr)

#Exporting
#writeRaster(richness, filename= 'richnessnr.tif', format= 'GTiff', overwrite=TRUE)

##################################################### Suitability

setwd('..//')
getwd()

allsuit <- stack(list.files(pattern= '.tif'))


labs <- sub('_',' ' , str_to_sentence( names(allsuit)))  

labs1 <- vector()
for(i in labs) { 
  labs1[i] <- c(labs1, bquote(italic( labs) ) )
                
          }


plot(allsuit, main= labs, col= colorRampPalette(c("blue", "orange", "purple", "red"))(10))
plot(allsuit, main=labs, col= wes_palette(7, name='FantasticFox1', type='continuous'))



##############################################################################################################
# Exporting maps per species 
##############################################################################################################
setwd(binrasterdir)
setwd('../')

allsuit <- stack(list.files(pattern= '.tif'))
plot(allsuit)
names(allsuit)

bigo <- as.data.frame(allsuit, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

# All in one

bigo$labi <- sub('_',' ' , str_to_sentence( bigo$variable    ))  

mypal <- viridisLite::turbo(200)

bigo1 <- bigo %>% filter(value > 0 )

table(is.na(bigo1$value))
bigo1$value
  
bigfig <-   ggplot() +  geom_sf(data=st_as_sf(maxpercountry), fill='white', col="grey40", size=0.3, alpha=1) + #poly #"mintcream"
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160))+
  ggtitle( 'Habitat suitability for sarbecovirus hosts' )  + 
  geom_tile(data=bigo1 , aes(x=x, y=y, fill=value) ) +
  facet_wrap(~ labi, ncol=3)+
  scale_fill_viridis_c(option="turbo", na.value= "white") + #breaks=c(2, 6, 10, 12)
  maptheme +  theme(plot.background = element_rect(fill=NA, color=NA), legend.title=element_blank(), plot.title=element_text(size=13, face = "bold"),
                    strip.text = element_text(face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

namefigall <- paste0(version, "_allinone_wrapv.tif")

setwd("D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_maps//")
tiff(filename = namefigall, res=600, width = 30, height = 44, units = "cm" )
print(bigfig)
dev.off()

# geom_sf(data=st_as_sf(maxpercountry), fill='white', col="grey40", size=0.3, alpha=1) + 
#geom_sf(data=st_as_sf(poly), fill=NA, col="grey40", size=0.2, alpha=0.8) 


# IUCN intersected
versioni

setwd(binrasterdiri)
setwd('../')

allsuiti <- stack(list.files(pattern= '.tif'))

bigoi <- as.data.frame(allsuiti, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

# All in one

bigoi$labi <- sub('_',' ' , str_to_sentence( bigoi$variable    ))  

mypali <- viridisLite::turbo(200)

bigo1i <- bigoi %>% filter(value > 0 )

table(is.na(bigo1i$value))
bigo1$value

bigfigi <-   ggplot() +  geom_sf(data=st_as_sf(maxpercountry), fill='white', col="grey40", size=0.3, alpha=1) + #poly #"mintcream"
  coord_sf(crs = st_crs(crs(worldmap)), ylim = c(-35, 80), xlim = c(-20, 160))+
  ggtitle( 'Habitat suitability for sarbecovirus hosts - IUCN intersected data' )  + 
  geom_tile(data=bigo1i , aes(x=x, y=y, fill=value) ) +
  facet_wrap(~ labi, ncol=3)+
  scale_fill_viridis_c(option="turbo", na.value= "white") + #breaks=c(2, 6, 10, 12)
  maptheme +  theme(plot.background = element_rect(fill=NA, color=NA), legend.title=element_blank(), plot.title=element_text(size=13, face = "bold"),
                    strip.text = element_text(face = "italic")) +
  ylab('Latitude') + xlab('Longitude')

namefigalli <- paste0(versioni, "_allinone_wrap_I.tif")

setwd("D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_maps//")
tiff(filename = namefigalli, res=600, width = 30, height = 44, units = "cm" )
print(bigfigi)
dev.off()

#
#
#

##########################################################################################
namefigall1 <- paste0(version, "_allinone_histsuitnolog.tif")
tiff(filename = namefigall1, res=600, width = 27, height = 22, units = "cm" )
ggplot(bigo1) +
  geom_histogram(aes(value)) +  
  xlab('Habitat suitability') + 
  facet_wrap(~labi) +theme_bw()+theme(strip.text = element_text(face = "italic")) 
dev.off()

##########################################################################################
summary(bigo)

summary(bigo1)

setwd(nonrestrictedrasters)
setwd('../')

allsuitnr <- stack(list.files(pattern= '.tif'))

# Setting values to na
allsuit_na <- allsuit
values(allsuit_na)[values(allsuit_na) == 0] = NA

for( i in names(allsuit)) {
  #for(j in names(allsuitnr)) {

dr <-data.frame(rasterToPoints(allsuit[[i]]))

labi <- sub('_',' ' , str_to_sentence( colnames(dr[3])    ))  
mypal <- viridisLite::turbo(200)

#mapsuit = ggplot() + 
 # geom_raster(data=dr , aes(x=x, y=y, fill=dr[,3]) ) + 
 # geom_sf(data=st_as_sf(poly), fill=NA, col="grey40", size=0.2, alpha=0.8) + 
 # ggtitle( labi )  +  
  #maptheme + 
  #scale_fill_gradientn(colors=mypal, na.value = "white") +
  #geom_rect(aes(ymin=-90, ymax=90, xmin=-180, xmax=180), fill=NA, color="black", size=0.5) +
  #theme(plot.background = element_rect(fill=NA, color="black")) +
 # theme(,legend.title=element_blank(), plot.title=element_text(size=16, face = "italic")) 

print(i)
# Export 
#setwd("D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_maps//")
#tiff(filename = paste0('Map_',i,"_present_MSDM_turbo.tif"), res=600, width = 30, height = 22, units = "cm" )
#print(mapsuit)
#dev.off()

# With NA

drna <-data.frame(rasterToPoints(allsuit_na[[i]]))
mapsuitna = ggplot() + 
  geom_raster(data=drna , aes(x=x, y=y, fill=drna[,3]) ) + 
  geom_sf(data=st_as_sf(poly), fill=NA, col="grey40", size=0.2, alpha=0.8) + 
  ggtitle( labi )  +  
  maptheme + 
  scale_fill_gradientn(colors=mypal, na.value = "white") +
  theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic")) 

# Exportwith NA
setwd(projdir)
setwd("dynamic_maps")

fn <- paste0('Map_',i,version_suffix, "_present_MSDM_turbo_NA.tif")
tiff(filename = fn, res=600, width = 30, height = 22, units = "cm" )
print(mapsuitna)
dev.off()

# Non restricted

#drnr <-data.frame(rasterToPoints(allsuitnr[[j]]))

#labinr <- sub('_',' ' , str_to_sentence( colnames(drnr[3])    ))  

#mapsuit = ggplot() + 
# geom_raster(data=drnr , aes(x=x, y=y, fill=drnr[,3]) ) + 
 # geom_sf(data=st_as_sf(poly), fill=NA, col="grey40", size=0.2, alpha=0.8) + 
  #ggtitle( labinr )  +  
  #maptheme + 
  #scale_fill_distiller(palette = "RdBu") +
  #scale_fill_gradientn(colors=mypal   , na.value = "white") +
  #theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic")) 

#print(j)
# Export 
#setwd("D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_maps//")
#tiff(filename = paste0('Map_',j,"_present_turbo_non_restricted.tif"), res=300, width = 30, height = 22, units = "cm" )
#print(mapsuit)
#dev.off()
  #}
}

############################################################################