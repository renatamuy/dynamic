###############################################################################################################
#  Script to generate interactive maps for each target species
# This script is preprocessing for SDM
# Generates interactive maps containing mined cleaned data and data intersecting with IUCN or bat handbook polygons
###############################################################################################################

source('00_packages.R')


iucn <- st_read('D:/OneDrive - Massey University/Verena/betacov_bats_mapping-master/MAMMALS_TERRESTRIAL_ONLY.shp')


setwd("D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_master//")
dynmaster <- "D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_master//"
# CREATING TARGETS


#file_all <- read.xlsx("D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_visual_inspection_after_cc//CC_master_bat_data_2021_07_D27_before_Tigga.xlsx",
#                      sheetIndex = 1)

file_all <- read.table('CC_master_bat_data_2021_09_D16.txt', header=TRUE)
file_all$x <- file_all$long
file_all$y <- file_all$lat
file_all$latitude <- file_all$lat

targets <-  sub('_', ' ', unique(file_all$sp))



firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

targets <- firstup(targets) 

# Match with targets

bi <- iucn[iucn$binomial %in% targets,]

unique(bi$binomial)

# Rhinolophus cornutus"  "Rhinolophus monoceros" manual
setdiff(targets, unique(bi$binomial)) # Cornutus (JAPAN, MANUAL) and monocerus

getwd()

# Directory creation
setwd('D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_master')

dir.create('IUCN_ranges_intersect_2021_09_D16')
setwd('./IUCN_ranges_intersect_2021_09_D16')

bi$binomial_l <- stringr::str_to_lower(bi$binomial)

t2 <- stringr::str_to_lower(targets)

d$spb <- sub('_', ' ', d$sp)

# Creating simple feature
pointsmain <- st_as_sf(x = d, 
                       coords = c("long", "lat"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Looping around, considering conditional species
# Warning: st_intersection gets rid of long lat columns in the point input...

aux <- data.frame()

for(year in unique(t2)) {  #bi$binomial_l
  try({
    
    print(year)
    
    if(year == 'rhinolophus cornutus' )
    {
      filename= paste0(year,'_cc_IUCN.html')
      
      p <- st_as_sf(x = d[d$spb== year,], 
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      os <- d[d$spb== year,]
      
      cornutus <- st_read('D://OneDrive - Massey University//PD_2021//IUCN_range//Rhinolophus cornutus//data_0.shp')
      
      popups <- paste("Base:", os$base, "<br>",
                      "Species:", os$splab, "<br>",
                      "Date:", os$year, "<br>")
      
      
      m <- leaflet(cornutus) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~os[,"long"], lat = ~os[,"lat"], weight=1.2,
                         color = "navy",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(m, file=filename)
      
      #Filter automatically here and export in loop, make table without outlier, maybe just thin in ENMTML
      #st_write(cornutus, "rhinolophus cornutus.shp") 
      
      # Intersect map 
      filename_i = paste0(year,'_cc_IUCN_inter.html')
      
      inter <- dplyr::select(as.data.frame(st_set_crs(st_intersection(p, cornutus), "EPSG:4326")), -geometry) %>%
        select("sp" ,  "date"   ,  "base"    , "key"   ,   "year"    , "splab"    ,   "x"    ,   "y" ,    "spb")  
      
      aux <- rbind(aux, inter) 
      
      minter <-  leaflet(cornutus) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~inter[,"x"], lat = ~inter[,"y"], weight=1.2, #lng = ~inter[,"x"], lat = ~inter[,"y"], 
                         color = "purple",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(minter, file=filename_i)
      
    } 
    if(year == 'rhinolophus monoceros' )    
    {
      filename= paste0(year,'_cc_IUCN.html')
      
      
      p <- st_as_sf(x = d[d$spb== year,], 
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      os <- d[d$spb== year,]
      
      popups <- paste("Base:", os$base, "<br>",
                      "Species:", os$splab, "<br>",
                      "Date:", os$year, "<br>")
      
      mono <- st_read('D://OneDrive - Massey University//PD_2021//IUCN_range//Rhinolophus formosae//data_0.shp')
      
      m <- leaflet(mono) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~os[,"long"], lat = ~os[,"lat"], weight=1.2,
                         color = "navy",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(m, file=filename)
      #st_write(mono, "rhinolophus monoceros.shp") 
      # Intersect map
      
      filename_i = paste0(year,'_cc_IUCN_inter.html')
      
      inter <- dplyr::select(as.data.frame(st_set_crs(st_intersection(p, mono), "EPSG:4326")), -geometry) %>%
        select("sp" ,  "date"   ,  "base"    , "key"   ,   "year"    , "splab"    ,   "x"    ,   "y" ,    "spb")  
      
      aux <- rbind(aux,inter) 
      
      minter <-  leaflet(mono) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~inter[,"x"], lat = ~inter[,"y"], weight=1.2,
                         color = "purple",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(minter, file=filename_i)
      
    }
    ################
    if(year == 'hipposideros pomona' )     # pomona complex gentilis
    {
      filename= paste0(year,'_cc_IUCN.html')
      
      #year = 'hipposideros pomona'
      
      p <- st_as_sf(x = d[d$spb== year,], 
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      os <- d[d$spb== year,]
      
      popups <- paste("Base:", os$base, "<br>",
                      "Species:", os$splab, "<br>",
                      "Date:", os$year, "<br>")
      
      gentilis <- iucn[iucn$binomial %in% 'Hipposideros gentilis',]
      
      m <- leaflet(gentilis) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~os[,"long"], lat = ~os[,"lat"], weight=1.2,
                         color = "navy",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(m, file=filename)
      
      # Intersect map (filter points within respective IUCN range shapefile)
      filename_i = paste0(year,'_cc_IUCN_inter.html')
      
      inter <- dplyr::select(as.data.frame(st_set_crs(st_intersection(p, gentilis), "EPSG:4326")), -geometry) %>%
        select("sp" ,  "date"   ,  "base"    , "key"   ,   "year"    , "splab"    ,   "x"    ,   "y" ,    "spb")  
      
      aux <- rbind(aux, inter) 
      
      minter <-  leaflet(gentilis) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~inter[,"x"], lat = ~inter[,"y"], weight=1.2,
                         color = "purple",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(minter, file=filename_i)
      ################
    }   else{
      
      df.year = bi[bi$binomial_l== year,]
      
      file.name = paste0(year,".shp")
      
      #st_write(df.year, file.name)
      
      #df.buffer <- st_buffer(df.year, dist= 3)
      
      p <- st_as_sf(x = d[d$spb== year,], 
                    coords = c("long", "lat"),
                    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      os <- d[d$spb== year,]
      
      filename= paste0(year,'_cc_IUCN.html')
      
      popups <- paste("Base:", os$base, "<br>",
                      "Species:", os$splab, "<br>",
                      "Date:", os$year, "<br>")
      
      m <- leaflet(df.year) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~os[,"long"], lat = ~os[,"lat"], weight=1.2,
                         color = "navy",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(m, file=filename)
      
      print('saved html simple')
      
      # Intersect map
      filename_i = paste0(year,'_cc_IUCN_inter.html')
      # Sf objects are weird, so get back to df
      
      inter <- dplyr::select(as.data.frame(st_set_crs(st_intersection(p, df.year), "EPSG:4326")), -geometry)%>%
        select("sp" ,  "date"   ,  "base"    , "key"   ,   "year"    , "splab"    ,   "x"    ,   "y" ,    "spb")  
      
      aux <- rbind(aux, inter) 
      
      minter <-  leaflet(df.year) %>% addTiles() %>%
        addPolygons(color = "gray", fillColor='gold',weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)    %>%
        addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
        addCircleMarkers(lng = ~inter[,"x"], lat = ~inter[,"y"], weight=1.2,
                         color = "purple",
                         group = "Occurrences" , popup = popups) %>%  
        addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                           options = layersControlOptions(collapsed=FALSE) ) 
      
      saveWidget(minter, file=filename_i)
      
    }
  })
}

# Cleaning the looks of aux (IUCN cropped occurrences)

nrow(unique(aux))

aux_u<- unique(aux)
oi <- data.frame(table(aux_u$splab))

colnames(aux_u) <- c("sp"  ,  "date" , "base",  "key" ,  "year" , "splab", "long"  ,   "lat"  ,   "spb" )

# Exporting aux_u so you can work on script 04_script_MASTER_outlier and thinning
setwd('../')
getwd()


setwd(dynmaster)
toexp <- aux_u %>%  relocate(lat, .after= long)

# Standard cols for modelling
#"sp"	"date"	"base"	"key"	"year"	"splab"	"long"	"lat"	"spb"

write.xlsx(toexp, 'CC_IUCN_intersect_master_bat_data_2021_09_D16.xlsx')

# As text file

write.table(toexp, 'CC_IUCN_intersect_master_bat_data_2021_09_D16.txt', row.names=FALSE)

iu <- data.frame(table(toexp$sp)) %>%  arrange()

allcc <- data.frame(table(file_all$sp)) %>%  arrange()

merge( allcc, iu, by='Var1', all.y=TRUE)


# Conferring

dinter <- read.xlsx('CC_IUCN_intersect_master_bat_data_2021_09_D16.xlsx', sheetIndex = 1)


# Confirming that all species had IUCN polygon.. tricky for that one from India! CONDITIONAL!!!!
#setdiff( unique(bi$binomial_l), sub('.shp', '', list.files(pattern = '.shp'))) 
# Data intersected with IUCN or bat book ranges 
###############################################################################################################
##############################################################################################################
# Manual for the little japanese bat and for R monocerus (no range)
# Manual for the little formosan bat r monocerus
##############################################################################################################
#setwd("D://OneDrive - Massey University//PD_2021//master_species_list//sarbecovirus_bat_hosts//sarbecobats//")
#cornutus <- st_read('D://OneDrive - Massey University//PD_2021//IUCN_range//Rhinolophus cornutus//data_0.shp')
#mono <- st_read('D://OneDrive - Massey University//PD_2021//IUCN_range//Rhinolophus formosae//data_0.shp')
#############################################################################################################
