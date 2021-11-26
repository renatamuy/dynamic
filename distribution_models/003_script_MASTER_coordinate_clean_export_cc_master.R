# Master master curation, cc, and outlier removal
# Leave thinning for ENMTML (RES)
# Outlier analysis
# Minimum number of occurrences
# Get data only from Asia, Africa and Europe
# Data curation, filtering

source('00_packages.R')

setwd('./dynamic_master')

m <- read.delim('master.txt')

mo <- read.csv('master_bat_data_2021_09_D16.csv')

unique(mo$splab)

str(mo$splab)

str(m$Master_sp_list)

mot <- mo[mo$splab %in% m$Master_sp_list,]

######################################################################################################################
#outliers_method
#The method used for outlier testing IS null. See details.
# Leave thinning for ENMTML (RES)
#outliers_mtp
#numeric. The multiplier for the interquartile range of the outlier test. If NULL outliers.td is used. Default = 5.
#####################################################################################################################

# flag data
flags_bias <- CoordinateCleaner::clean_coordinates(
  x = mot, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  outliers_mtp = NULL, 
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "seas", # in the sea
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon 
  )
)


flags_bias %>% summary

# exclude records flagged by any test

occ_data_taxa_date_bias <- mot %>% 
  dplyr::filter(flags_bias$.summary == TRUE)
occ_data_taxa_date_bias

# Since previous master data was already cleaned, this is just a 'make sure step'

nrow(flags_bias)  - nrow(occ_data_taxa_date_bias) 

# resume data
mot %>% dplyr::count(sp)
occ_data_taxa_date_bias %>% dplyr::count(sp) %>% arrange(desc(n))

# map
occ_data_taxa_date_bias_vector <- occ_data_taxa_date_bias %>% 
  tidyr::drop_na(long, lat) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)


tmap_options(check.and.fix = TRUE)

# spatial -----------------------------------------------------------------
# spatial occ ASIA AFRICA EUROPE .. this stopped working

occ_data_taxa_date_bias_vector <- occ_data_taxa_date_bias %>%
  dplyr::mutate(x = long, lon = long, y = lat, latitude = lat, lat=lat) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_taxa_date_bias_vector
head(occ_data_taxa_date_bias_vector)
occ_data_taxa_date_bias_vector$lat <- occ_data_taxa_date_bias_vector$y

# extent NOT WORKING after R 4.1.0 UPDATE
#li_sa <- rnaturalearth::ne_countries( scale = 110, continent = c("Africa", "Asia", "Europe"), returnclass = "sf")
#li_sa %>% tm_shape() + tm_polygons() + tm_graticules(lines = FALSE)

############### other way
map = spData::world %>%
  dplyr::filter(!continent %in% c("South America", "North America", "Antarctica", "Oceania"))

# Russia only will not do; Fiji also crosses the antimeridean...
rossiya <- subset(map,  iso_a2 %in% c("RU", "FJ"))
pacified_rossiya <- st_shift_longitude(rossiya)
rest_of_world <- subset(map, !iso_a2 %in% c("RU", "FJ", 'FR', 'TF')) # removing weird territories
plot(rest_of_world)
plot(subset( map, name_long %in% c('France')))
fr <- subset( map, name_long %in% c('France'))
fre <- st_cast(fr,"POLYGON")
france <- fre[2:3,] #REMOVING FRENCH GUIANA......................

#spatialEco::explode(fr)
map2 <- rbind(pacified_rossiya,
              rest_of_world, france) 


exp <- spatialEco::explode(map2)
checking <- st_polygon(map2)
justp <- st_collection_extract(map2, "POLYGON")

#spatialEco::explode(fr)
##############################################################################################################
# don't crop to limit
map2
# exporting
#st_write(map2, paste0(getwd(), "/", "working_extent_wgs84.shp"))
# exporting points to work on QGIS

datadate <- readline(prompt="Enter date for version: ")
prefixo <- "CC_master_bat_data_"

st_write(occ_data_taxa_date_bias_vector, paste0(getwd(),"/" ,prefixo, datadate, ".shp"))

prefixo <- "CC_master_bat_data_"

data.frame(table(occ_data_taxa_date_bias$splab)  ) %>% arrange(desc(Freq))

# Exporting table 

write.table(occ_data_taxa_date_bias, paste0(getwd(),"/" ,prefixo, datadate, ".txt"), row.names = FALSE, sep='\t')

colnames(occ_data_taxa_date_bias_vector)

colnames(occ_data_taxa_date_bias)
nrow(occ_data_taxa_date_bias)

#################ATTENTION####################################################################################
#
# Since the st_crop was not working AND the rnaturalearth shapefile is really bad and not accurate for 
# cropping by extention,
# I proceeded with visual inspection for all species in QGIS crossing info with IUCN polygons and Tigga
# After that, I can do any thinning and export the master data as TXT to model in ENMTML
##############################################################################################################
