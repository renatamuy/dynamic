################################################################################################################
# Removing points from the ocean and thinning intersecting with IUCN
################################################################################################################
# Data outputs filtered for 27 km, 20 km, 5 km, 1 km
################################################################################################################

source('00_packages.R')

setwd('D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_master//')

load('workspace_2021_07_D27.RData') # Workspace with cleaned data (prior Tigga and prior IUCN intersect)

# SHAPE exported manually as xlsx in QGIS.. BEFORE TIGGA..
# remove latitude repeated column next time

d <- read.xlsx("D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_visual_inspection_after_cc//CC_master_bat_data_2021_07_D27_before_Tigga.xlsx",
              sheetIndex = 1)

head(d)
dd <- d %>%  relocate(lat, .after= long)

# Default = "quantile", Default = 5

thin_1km <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = dd, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.008333 )

# 1km: Removed 656 records

thin_5km <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = dd, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.041666)

# 5km: Removed 573 records

thin_20km <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = dd, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.1666 )

# 27km

thin_27km <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = dd, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.25 )

length(table(thin_20km$sp))
length(unique(d$sp))

# 20km: Removed 488 records

####################################
# 0.25 dd ~27 km
# 0.1666 dd  ~ 10 min ~18.5km
# 0.041666 dd  ~ 2.5 min ~ 5km
# 0.008333 dd  ~ 30s~      1km
#
######################################

# thinning after IUCN intersection from di
# Data with intersection with IUCN polygons generated in script IUCN loop
di <- read.xlsx('D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_master//CC_master_bat_data_2021_07_D27_after_Tigga.xlsx', sheetIndex = 1)


thin_1kmi <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = di, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.008333)

head(thin_1kmi)
unique(thin_1kmi$splab)

# 1kmi: Removed  528 records

thin_5kmi <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = di, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.041666)

# 5km: Removed 439 records

thin_20kmi <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = di, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.1666)

# 20km: Removed 391 records


# 0..25 dd or 27km 

thin_27kmi <- CoordinateCleaner::cc_outl(
  min_occs = 20,
  x = di, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  thinning = TRUE,
  thinning_res = 0.25)

#Removed 343 records.

# ready_to_rock
dir.create('ready_to_rock_27km')

setwd('ready_to_rock_27km')

# Dstaa not thinned (so ENTMTML can thin) cleaned and within IUCN limits
write.table(di, 'to_thin.txt', row.names = FALSE, sep='\t')

# Data cleaned and thinned (not necessarily intersecting with IUCN or handbook range )
write.table(thin_1km, 'thin_1km.txt', row.names = FALSE, sep='\t')
write.table(thin_5km, 'thin_5km.txt', row.names = FALSE, sep='\t')
write.table(thin_20km, 'thin_20km.txt', row.names = FALSE, sep='\t')
write.table(thin_27km, 'thin_27km.txt', row.names = FALSE, sep='\t')

# Data cleaned and thinned intersecting with IUCN or handbook range 
write.table(thin_1kmi, 'thin_1kmi.txt', row.names = FALSE, sep='\t')
write.table(thin_5kmi, 'thin_5kmi.txt', row.names = FALSE, sep='\t')
write.table(thin_20kmi, 'thin_20kmi.txt', row.names = FALSE, sep='\t')
write.table(thin_27kmi, 'thin_27kmi.txt', row.names = FALSE, sep='\t')

# Next steps: merge with darkcides source data with sp+lat+long info
table(thin_20kmi$base) # 820 records

#readme <- c('')
  
table(thin_20kmi$spb)
table(thin_27kmi$spb)
# Exporting
# Update with 27km
#write.xlsx(, 'points_sarbecobats_2021_06_D01.xlsx', sheetName = '1')
# ENMTML INPUT FILE
#write.table(, 'points_sarbecobats_2021_06_D01.txt', row.names = FALSE, sep='\t')

#### Check file
#check <- read.table('points_sarbecobats_2021_06_D01.txt',sep='\t' )

# I did not export them yet, because I want to talk to Tigga first, but won't change much!

# Column 1: Points with CC+inspection
cc_inspected <- data.frame(table(dd$splab))
# Column 2: Points thinned+outlier removal 20km
o20 <- data.frame(table(thin_20km$splab))
# Column 3: Points thinned+outlier removal 5km
o5 <- data.frame(table(thin_5km$splab))
# Column 3: Points thinned+outlier removal 1km
o1 <- data.frame(table(thin_1km$splab))
# Column 4: Points thinned+outlier removal 27km
o27 <- data.frame(table(thin_27km$splab))

# 20 and 5
o205 <- merge(o20,  o5, by=c('Var1'))

colnames(o205) <- c('Var1', 'Thinned 20 km', 'Thinned 5km')
# 205 and 1
o2051 <- merge(o205,  o1, by=c('Var1'))   

colnames(o2051) <- c("Var1"      ,    "Thinned 20 km" ,"Thinned 5km" ,  "Thinned 1 km" )

o2051_27 <- merge(o27, o2051,   by=c('Var1')) 

colnames(o2051_27) <- c("Var1"      , 'Thinned 27 km' ,   "Thinned 20 km" ,"Thinned 5km" ,  "Thinned 1 km")

table_data_filtering <- merge(cc_inspected, o2051_27, by=c('Var1'))

table_data_filtering

colnames(table_data_filtering) <- c("Species",    
                                    "Points cleaned and inspected",  
                                    "Thinned 27 km",
                                    "Thinned 20 km",
                                    "Thinned 5km"  ,
                                    "Thinned 1 km" )

write.xlsx(table_data_filtering, 'table_thinned.xlsx', row.names = FALSE)


###############################################################################################################
########## IUCN intersect tables###############################################################################

cc_inspectedi <- data.frame(table(di$splab))
# Column 2: Points thinned+outlier removal 20km
o20i <- data.frame(table(thin_20kmi$splab))
# Column 3: Points thinned+outlier removal 5km
o5i <- data.frame(table(thin_5kmi$splab))
# Column 3: Points thinned+outlier removal 1km
o1i <- data.frame(table(thin_1kmi$splab))
# Column 4: Points thinned+outlier removal 27km
o27i <- data.frame(table(thin_27kmi$splab))

#
o205i <- merge(o20i,  o5i, by=c('Var1'))
colnames(o205i) <- c('Var1', 'Thinned 20 km', 'Thinned 5km')
o2051i <- merge(o205i,  o1i, by=c('Var1'))   
colnames(o2051i) <- c("Var1"      ,    "Thinned 20 km" ,"Thinned 5km" ,  "Thinned 1 km" )

#
o2051i_27i <- merge(o27i, o2051i,   by=c('Var1')) 

colnames(o2051i_27i) <- c("Var1"      , 'Thinned 27 km' ,   "Thinned 20 km" ,"Thinned 5km" ,  "Thinned 1 km")

table_data_filteringi <- merge(cc_inspectedi, o2051i_27i, by=c('Var1'))

table_data_filteringi

colnames(table_data_filteringi) <- c("Species",    
                                    "Points cleaned and inspectedi",  
                                    "Thinned 27 kmi",
                                    "Thinned 20 kmi",
                                    "Thinned 5kmi"  ,
                                    "Thinned 1 kmi" )

write.xlsx(table_data_filteringi, 'table_IUCN_intersected_thinned.xlsx', row.names = FALSE)


##############################################################################################################
# Saving workspace
save.image('workspace_2021_09_D07.RData')

#############################################################################################################
# Exporting and pasting in supplemental material (Geographical filtering and bias reduction)
write.xlsx(table_data_filtering, 'table_data_filtering.xlsx', row.names = FALSE)
#Exporting and pasting in supplemental material actually used to model (INTERSECTED WITH iucn or handbook)
write.xlsx(table_data_filteringi, 'table_data_filtering_intersect.xlsx', row.names = FALSE)
#######################################################
# Near current criteria (1970-2021)

hist(dd$year)
sum(table(dd$year)) - nrow(dd)

dy <- data.frame(table(dd$year))
sum(dy[1:30, 'Freq'])/ nrow(dd) # 734
##########################################################################################################
# Maps ROCK to any final dataset


setwd('D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_master//')
dir.create('leaflets_thin')
setwd('leaflets_thin')

ls()
o <- thin_1kmi
sp <- unique(o$splab)

for(s in sp) {print(s)
  
  os <- o[o$splab == s,]
  
  filename= paste0(s,'.html')
  
  popups <- paste("Base:", os$base, "<br>",
                  "Species:", os$splab, "<br>",
                  "Date:", os$year, "<br>")
  
  
  m <- leaflet(os) %>% addTiles() %>%
    addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
    addCircleMarkers(lng = ~os[,"long"], lat = ~os[,"lat"], weight=1.2,
                     color = "green4",
                     group = "Occurrences" , popup = popups) %>%  
    addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                       options = layersControlOptions(collapsed=FALSE) ) 
  
  saveWidget(m, file=filename)
  
}

