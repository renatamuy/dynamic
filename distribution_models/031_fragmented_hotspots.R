#############################################################################################################
# Content
# Hotspots fragmentation in the future
# Think about connectivity
# Coordinates for the area with the highest concentration of hosts 
# xlim = c(95, 106), ylim = c(16, 27)

# packages----------------------------------------------------------------------------------------------------
require(raster)
require(landscapemetrics)
require(tidyverse)


# time--------------------------------------------------------------------------------------------------------
start <- print(Sys.time())

setwd(here())
setwd('hotspots')

# xmin,xmax,ymin,ymax

e <- as(extent(95, 106, 16, 27), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"

# Open richness map present

ri <- raster::raster('richnessm.tif')
r <- crop(ri, e)

table(values(ri))
table(values(r))

class_metrics_present <- calculate_lsm(r, 
                              level = c("class"), #patch- landscape other options
                              type = "aggregation metric") #get all the metrics for the given level

# Open richness map future
rif <- raster::raster('richness_2100_BCC_SSP585.tif')
rfut <- crop(rif, e)

plot(rif)
r[values(r) > 12] <- 12
class_metrics_fut <- calculate_lsm(rfut, 
                                       level = c("class"), #patch- landscape other options
                                       type = "aggregation metric") #get all the metrics for the given level

# Compare

table(values(rfut))
table(values(ri))

class_metrics_present %>% filter(metric == 'np' & class > 10) %>% 
  summarise(value) %>% sum()


class_metrics_fut %>% filter(metric == 'np' & class > 10) %>% 
  summarise(value) %>% sum()


# So, the number of patches with 10 or more species in the future increases (from 26 patches to 38)
# The division index for patches with high richness values increases in the future from 2.99 to 3.99

# Landscape division index (Aggregation metric) --------------------------------------------------------------
#directions: The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).

# 8 directions is default
#Larger values of division means more division, like when all patches of a certain class are single cells across the landscape

class_metrics_present %>% filter(metric == 'division' & class > 10) %>% 
  summarise(value) %>% sum()

class_metrics_fut %>% filter(metric == 'division' & class > 10) %>% 
  summarise(value) %>% sum()

class_metrics_present %>% filter(metric == 'division') %>% 
  summarise(value) %>% sum()

class_metrics_fut %>% filter(metric == 'division') %>% 
  summarise(value) %>% sum()



inset_division_present <- lsm_c_division(r, directions = 8) 
inset_division_present %>%  filter(class>10) %>%  summarise(value) %>% sum()

inset_division_fut <- lsm_c_division(rfut, directions = 8)
inset_division_fut %>%  filter(class>10) %>%  summarise(value) %>% sum()

plot(r)
plot(rfut)

end-start
#r[values(r) > 12] <- 12
# Maybe talk about connectivity? clusters?
# ------------------------------------------------------------------------------------------------------------