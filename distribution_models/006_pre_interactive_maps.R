##############################################################################################################
# Interactive leaflet maps
##############################################################################################################
ls()
gc() 
options(digits=7, scipen=999)

source('00_packages.R')

# Opening bat points
# automatize: merge, create report, pal, leaflet
# note location C or D #########################################################

setwd(here())
setwd('dynamic_master')

d <-  read.table("CC_master_bat_data_2021_09_D16.txt", header=TRUE)

data.frame(table(d$splab)) %>% arrange(Freq)


#Setting area of study
asia <- rnaturalearth::ne_countries(
  continent = c("Asia"),
  returnclass = "sf")
st_crs(asia) <- CRS("+init=EPSG:4326")
# Downloading and Exploding it
asia <- spatialEco::explode(rnaturalearth::ne_countries(
  continent = c("Asia","Africa", "Europe", "Oceania"),
  returnclass = "sf"
))

#st_write(asia, dsn = "aaeo.shp", layer = "aaeo.shp", driver = "ESRI Shapefile")
# Checking weird territories as French Guyana
grep("^[G]", asia$name, value=TRUE)
grep("^[S]", asia$name, value=TRUE)
check <- data.frame(st_coordinates(asia))
asia[which(check$X < -48),]
delete <- which(check$X < -48)
check[which(check$X < -48),]
asia <- asia[-delete,]
plot(asia)

# new subset
#asia <- asia %>% filter(!subregion %in% c("Eastern Africa", "Southern Africa"
#                                ,"Middle Africa", "Northern Europe")) %>%  
#                             filter( !name %in%  c("New Zealand"))
#setwd("C://Users//Renata//OneDrive - Massey University//_env/land_cover//")

########### Popups

o <- d
popups <- paste("Base:", o$base, "<br>",
                "Species:", o$splab, "<br>",
                "Date:", o$year, "<br>")

palsp <- colorFactor("viridis", o$splab)

table(is.na(o$splab))

########### Plot 

allm <-  leaflet(o) %>% addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
  addCircleMarkers(lng = ~o[,"long"], lat = ~o[,"lat"], weight=1.2,
                   color = ~palsp(splab), #"firebrick"
                   group = "Occurrences" , popup = popups) %>%  
     addLegend(pal = palsp, values = ~o$splab, opacity = 1) %>% 
     addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                                       options = layersControlOptions(collapsed=FALSE) ) 

library(htmlwidgets)

saveWidget(allm, file='all_sarbecovirus_hosts.html')

# For
#dir.create('./leaflets/')
setwd('leaflets')

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
                     color = "navy",
                     group = "Occurrences" , popup = popups) %>%  
      addLayersControl(  baseGroups = c("Open SM", "Esri WorldImagery"),
                       options = layersControlOptions(collapsed=FALSE) ) 
  
  saveWidget(m, file=filename)
  
    }


#overlayGroups =  as.character(unique(o$splab)),

pal <-  colorFactor(palette = c("blue", "red", "green"), 
              levels = c("Public", "Private", "For-Profit"))

m %>% 
  addCircleMarkers(color = ~pal(sector_label))

#
###########################################################
#save.image(file='leaflet_session.RData')
#load(".RData")

records <- data.frame(t(table(o2$sp_enm)))
records$Var1 <- NULL
records$Var2
nrow(records)
sum(records$Freq)
records <- records[rev(order(records$Freq)),]

# Correct typo

ggplot(data=records)+geom_bar()
head(o2)
go2 <- ggplot(data=o2) + 
  geom_bar(mapping = aes(x=forcats::fct_infreq(sp_enm), fill = base),
           position="dodge")+ 
  scale_fill_viridis_d()+
  theme(axis.text.y = element_text(angle=0,
                                   vjust=0.4,
                                   hjust = 0.5,
                                   family="Helvetica", face="italic", colour="black", size=rel(0.5)))+
  ylab("Number of unique locations")+xlab("Species")+
  coord_flip()+
  theme_bw()
ggsave("go2.png", go2, width = 12, height = 16)

# end---------------------------------------------------------------------------------------------------------