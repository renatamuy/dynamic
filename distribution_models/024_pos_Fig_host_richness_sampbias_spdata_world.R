#' Contents:
#' Table 1
#' Supplemental figures
#' Exploring sampbias outcomes

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
setwd(here())

setwd("sampbias_025dd/")

rbias <- raster("rcar_sarbecovirus_hosts.tif")

crs(rbias) <- CRS("+init=EPSG:4326")

# Correlation

richness
rbias

rbiasr <- resample(rbias, richness )

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

# Just comparing
summary(big$rcar_sarbecovirus_hosts)
summary(check$rcar_sarbecovirus_hosts)

skim(big)
big %>%
  mutate(samprate = ifelse(rcar_sarbecovirus_hosts
                           <= 0.013, "low", "high"), 
         rcar_sarbecovirus_hosts = factor(samprate)) %>%
  janitor::tabyl(samprate)


setwd(here())
setwd("hotspots")

figs <- paste0("Fig_supplements_sampling_rich_", version_suffix, ".jpg")
#ggsave(filename = figs, 
#  gridExtra::grid.arrange(figsamp, nrow = 1))

figsa <- paste0("Fig_supplements_sampling_rich_", version_suffix, ".jpg")
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
# end ---------------------------------------------------------------------