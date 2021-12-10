# Table number of points per species

source('00_packages.R')

setwd('./dynamic_master')
m <- read.delim('master.txt')

mo <- read.csv('master_bat_data_2021_07_D26.csv')

mo$splab <- sub('_', ' ', stringr::str_to_sentence(mo$sp))

str(mo$splab)

str(m$Master_sp_list)

mot <- mo[mo$splab %in% m$Master_sp_list,]

data.frame(table(mot$splab))

##############################################################################################################
# Read rasters and extract info for points
# Raster prep disabled (in the end)
# EDA 1km 
##############################################################################################################
setwd('D:/OneDrive - Massey University/PD_2021/master_species_list/sarbecovirus_bat_hosts/sarbecobats/')

d <- mot

head(d)

d_env <- "D:/OneDrive - Massey University/_env/_env_1km/"

dir(d_env)
setwd(d_env)

s <- stack(dir(d_env))
plot(s)
names(s)

tags <- c('forest_loss_risk_continent','forest_loss_risk_global', 
'karst',
'Annual Mean Temperature',
'Annual Precipitation',
'Precipitation Seasonality (Coefficient of Variation)',
'Precipitation of Driest Quarter',
'Temperature Seasonality')

##############################################################################################################
ds <- d %>% 
  tidyr::drop_na(long, lat) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

dsx <- raster::extract( s, ds, method= 'simple')

dsxd <- data.frame(dsx)

dsxd$sp <- ds$sp

# Better species names
dsxd$sp <- sub('_', ' ',dsxd$sp )
dsxd$sp <- str_to_sentence(dsxd$sp)

##################################################################################################

colourCount = length(unique(dsxd$sp))
palette <- distinctColorPalette(colourCount)

hist(dsxd$karst_resample_1km_wgs84)
karsthist <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= karst_resample_1km_wgs84)) +
  geom_histogram( fill="purple", alpha=0.6, position = 'identity') +
  scale_y_log10()+
  #scale_fill_manual(values =palette, name= "Hosts") +
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Distance to karst (km)") + theme_bw()

karsthist

bio1hist <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= wc2.1_30s_bio_1)) +
  geom_histogram( fill="firebrick", alpha=0.8, position = 'identity') +
  scale_y_log10()+
  #scale_fill_manual(values =palette, name= "Hosts") +
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Annual Mean Temperature") + theme_bw()

bio1hist

bio4hist <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= wc2.1_30s_bio_4)) +
  geom_histogram( fill="firebrick3", alpha=0.8, position = 'identity') +
  scale_y_log10()+
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Temperature Seasonality (standard deviation * 100)") + theme_bw()

bio4hist

tags
bio12hist <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= wc2.1_30s_bio_12)) +
  geom_histogram( fill="darkcyan", alpha=0.6, position = 'identity') +
  scale_y_log10()+
  #scale_fill_manual(values =palette, name= "Hosts") +
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Annual Precipitation") + theme_bw()

bio12hist

bio15hist <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= wc2.1_30s_bio_15)) +
  geom_histogram( fill="navy", alpha=0.6, position = 'identity') +
  scale_y_log10()+
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Precipitation Seasonality") + theme_bw()

bio15hist

bio17hist <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= wc2.1_30s_bio_17)) +
  geom_histogram( fill="lightskyblue4", alpha=0.6, position = 'identity') +
  scale_y_log10()+
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Precipitation of Driest Quarter") + theme_bw()

bio17hist

head(dsxd)

landhist <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= Continent_transition_potential_1km)) +
  geom_histogram( fill="darkolivegreen4", alpha=0.6, position = 'identity') +
  scale_y_log10()+
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Forest loss risk (2030)") + theme_bw()

landhist

landhistg <- dsxd %>%
  drop_na() %>% 
  ggplot( aes(x= Global_transition_potential_1km)) +
  geom_histogram( fill="darkolivegreen4", alpha=0.6, position = 'identity') +
  scale_y_log10()+
  facet_wrap(~sp)+
  theme_ipsum() +
  labs(x="Global forest loss risk (2030)") + theme_bw()

landhistg

# Get pixel codes
# Export EDA results

setwd("D:/OneDrive - Massey University/PD_2021/master_species_list/sarbecovirus_bat_hosts/sarbecobats/")



multi.page <- ggarrange(karsthist,
landhist,
bio1hist,
bio4hist,
bio12hist,
bio15hist,
bio17hist,
landhistg,
landhist, nrow=1, ncol=1) # for one plot per page
multi.page[[1]] # for seeing the first plot
ggexport(multi.page, filename="EDA_extract_histograms_log10_2021_06_D24.pdf")
ggexport(multi.page, filename="EDA_extract_histograms_log10_2021_06_D24.png",
         width = 3000, height = 2000, res=300)


getwd()

data.frame(table(dsxd$ESA_land_cover_1km))

create_report(dsxd, output_file = 'EDA_extract_report.html',
              report_title = "Data Profiling Report")

# Getting transition potential and resampling it
# https://futureclimates.conservation.org/riskstreecoverloss.html
# The transition potential surface identifies the potential of each pixel to transition from its current state, 
# in this study from tree cover to non-tree cover, thus highlighting areas of highest risk of tree cover loss; the transition potential surface provides the basis for identifying those pixels with the highest likelihood to change, or potential to transition. We used the transition potential surfaces as the basis to generate 15-year projections of potential future tree cover loss for 2014-2029 under a BAU scenario based on the historical deforestation rate (Figure 1)
#trans <- raster('D://OneDrive - Massey University//_env//deforestation//Global_transition_potential.tif')
#transr <- resample(trans,s[[3]], method = 'bilinear')
#writeRaster(transr, 'Global_transition_potential_1km.tif', format= 'GTiff')
#transcont <- raster('D://OneDrive - Massey University//_env//deforestation//Continent_transition_potential.tif')
#transcontr <- resample(transcont, s[[3]], method = 'bilinear')
# exporting to env 1km
#writeRaster(transcontr, 'Continent_transition_potential_1km.tif', format= 'GTiff')

