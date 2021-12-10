###############################################################
# Dynamic pipeline                                            #
# Getting additional bat locations                            #
# September 2021                                              #
# Renata Muylaert                                             #
# Occurrences already adding INAT quality grade level points  #
###############################################################

source('00_packages.R')

getwd()

#  add missing species here ##################################################################################

#d <- c( 'Rhinolophus blythi',
#        'Hipposideros galeritus',
#        'Hipposideros larvatus',
#        'Rhinolophus creaghi',
#       'Rhinolophus marshalli')

# 
d <- read.delim('D://OneDrive - Massey University//PD_2021//distribution_models//dynamic_master//master.txt')
d <- d$Master_sp_list

###---------------------------------------------------------------------------###
# Set region for your maps
area <- ggplot2::map_data("world", zoom=5) 
###---------------------------------------------------------------------------###

## import list of species
## bases
ba <- c("gbif",  "ecoengine", "vertnet",  "idigbio") 

banat <-c('inat')

## download data for the missing occurrences

dir.create("./missing_occurrences_2021")
setwd("./missing_occurrences_2021")

# for
aux <- data.frame()
auxn <- data.frame()
head(d)
sp <- d

getwd()

for(i in sp){
  
  print(paste0("Species --", i, "--"))
  re <- occ(query = i, from = ba, has_coords = T, limit = 10000)
  renat <- occ(query = i, from = banat, has_coords = T, limit = 10000)
  
  if(length(occ2df(re)) == 0){
    print(paste0("Sorry, no data for synonymies --", j, "--"))
    
  } else{
    da <- data.table(occ2df(re)[1], 
                     sp_enm = str_to_lower(sub(" ", "_", i)), 
                     occ2df(re)[-1])
    
    colnames(da) <- c("sp", "sp_enm", "long", "lat", "base", "date", "key") #"stateProvince", "basisofrecord","coordinateuncertaintyinmeters","coordinateprecision" )
    
    da.d <- distinct(da, long, lat, .keep_all = T)
    
    da.d$long <- as.numeric(da.d$long)
    da.d$lat <- as.numeric(da.d$lat)
    
    fwrite(da.d, paste0("occurrences_spocc_", str_to_lower(sub(" ", "_", i)), ".csv"))
    
    # map
    g <- ggplot() + geom_polygon(data = area,
                                 aes(x=long, y = lat, group = group),
                                 fill = "grey77", color = "lightgrey", size=0.04) + #Note que voce pode mudar as cores do fundo e da borda
      coord_fixed(1.1) + #Use isto para o mapa ficar proporcional
      geom_point(data = da.d, aes(x = long, y = lat, fill= base), 
                 shape = 21, 
                 size = 3, #Tamanho dos pontos
                 alpha = 0.6) + #Transparencia: quanto mais proximo de 1, menos transparente
      theme_bw() +
      ggtitle(i) + #De nome ao plot, caso seja necessario
      labs(x="Longitude", y = "Latitude") + #De nome aos eixos
      theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
            plot.title = element_text(size=20, hjust=0.5),
            axis.text.x = element_text(size = 10, angle=0, hjust=1),
            axis.text.y = element_text(size = 10, angle=0, vjust=1),
            axis.title.x = element_text(size = 12, angle=0),
            axis.title.y = element_text(size = 12, angle=90)) 
    #ggsn::scalebar(area, dist = 1000, location = "bottomright", transform = TRUE, #Adicione uma barra de escala
    #               dist_unit = "km", st.dist = 0.03, st.size = 2, model = 'WGS84') +
    #ggsn::north(area, scale = .1) #Adicione uma seta com o norte
    # Export map
    ggsave(paste0("occurrences_map_", str_to_lower(sub(" ", "_", i)), ".tiff"), dpi = 200)
    # All together
    aux <- rbind(aux,da.d )
    
    ############### INAT BIT ##################################
    ###########################################################
    ###########################################################
    da <- data.table(occ2df(renat)[1], 
                     sp_enm = str_to_lower(sub(" ", "_", i)), 
                     occ2df(renat)[-1])
    
    tdf <- renat$inat
    # indec is just necessary to make the columns names less annoying
    indec <-sub(" ", "_", i) 
    tdfd <- as.data.frame(tdf$data[indec] )
    # tdfd gives us quality grage, which is what we want to filter
    colnames(tdfd) <- sub(paste0(indec, '.'), '', colnames(tdfd))
    # getting only research quality observations
    tr <- tdfd %>% filter(quality_grade == 'research') 
    # look how much data is lost
    print(table(tr$quality_grade))
    print(i)
    
    notwanted <- setdiff( da$key, tr$id) # how many observations did not reach research grade quality
    
    da <- da[ !da$key %in% notwanted,]
    
    print('logical test must be TRUE')
    print(nrow(da) == nrow(tr))
    #tdfd[tdfd$id == '68999321',]$quality_grade
    
    colnames(da) <- c("sp", "sp_enm", "long", "lat", "base", "date", "key") #"stateProvince", "basisofrecord","coordinateuncertaintyinmeters","coordinateprecision" )
    
    da.d <- distinct(da, long, lat, .keep_all = T)
    
    da.d$long <- as.numeric(da.d$long)
    da.d$lat <- as.numeric(da.d$lat)
    
    fwrite(da.d, paste0("occurrences_inat_research_", str_to_lower(sub(" ", "_", i)), ".csv"))
    
    # map
    g <- ggplot() + geom_polygon(data = area,
                                 aes(x=long, y = lat, group = group),
                                 fill = "grey77", color = "lightgrey", size=0.04) + #Note que voce pode mudar as cores do fundo e da borda
      coord_fixed(1.1) + #Use isto para o mapa ficar proporcional
      geom_point(data = da.d, aes(x = long, y = lat, fill= base), 
                 shape = 21, 
                 size = 3, #Tamanho dos pontos
                 alpha = 0.6) + #Transparencia: quanto mais proximo de 1, menos transparente
      theme_bw() +
      ggtitle(i) + #De nome ao plot, caso seja necessario
      labs(x="Longitude", y = "Latitude") + #De nome aos eixos
      theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
            plot.title = element_text(size=20, hjust=0.5),
            axis.text.x = element_text(size = 10, angle=0, hjust=1),
            axis.text.y = element_text(size = 10, angle=0, vjust=1),
            axis.title.x = element_text(size = 12, angle=0),
            axis.title.y = element_text(size = 12, angle=90)) 
    # Export map
    ggsave(paste0("map_inat_research_", str_to_lower(sub(" ", "_", i)), ".tiff"), dpi = 200)
    # All together
    # Apparently R updates made impossible to rbind data.table in data.frame
    auxn <- rbind(auxn,data.frame(da.d) )
  }
}

###################################################
# Careful with weird data in Americas..
###################################################

# Deal with aux and auxn (inat)

colnames(aux)
colnames(auxn)

auxa <- rbind(aux, auxn)

# Remove weird data from Neotropics
drop <- which(auxa$long < -30) # Re-check all over() during accessible area selections and more filtering..

aux <- aux[-drop,]

records <- data.frame(t(table(aux$sp_enm)))
sum(records$Freq)
nrow(records)

records$Var1 <- NULL

records$Var2
nrow(records)
sum(records$Freq)

getwd()


write.table(file = "summary_missing_sarbecobats_2021.txt", records[rev(order(records$Freq)),])

sp_nu <- sub(" ", "_", sp)
# Check nas
setdiff(str_to_lower(sp_nu), unique(aux$sp_enm)) 

# data aready without duplicates

table(auxa$base)
fwrite(auxa, "occurrences_missing_sarbecobats_spocc_2021.csv")

str(aux)

getwd()


create_report(aux, output_file = "report_missing_sarbecobats_spocc_2021.html")

###############################################################################################################

# Cleaning already to merge in post filtering


###############################################################################################################

getwd()

list.files()

toclean <- read.csv('occurrences_missing_sarbecobats_spocc_2021.csv')

nrow(toclean)

table(toclean$sp_enm)

# year > 1970 and < or equal to 2020
require(tidyverse)

# For updates just change the year intervals


toclean$year <- stringr::str_sub(toclean$date, 1,4)

table(toclean$year)

toclean2 <- toclean %>% dplyr::filter(is.na(year) == FALSE,
                (year > 1970 & year <= 2021)) %>% 
  dplyr::arrange(year)


toclean2 <- toclean2 %>% dplyr::filter(is.na(long) == FALSE)
toclean2 <- toclean2 %>% dplyr::filter(is.na(lat) == FALSE)


head(toclean2)

nrow(toclean2)

table(toclean2$sp_enm) # 191

summary(toclean2$lat)


# flag data

flags_bias <- CoordinateCleaner::clean_coordinates(
  x = toclean2, 
  species = "sp",
  lon = "long", 
  lat = "lat",
  outliers_mtp = 5, 
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

# exclude records flagged by any test

occ_data_taxa_date_bias <- flags_bias %>% 
  dplyr::filter(flags_bias$.summary == TRUE)

nrow(flags_bias) -nrow(occ_data_taxa_date_bias)
table(occ_data_taxa_date_bias$sp_enm)

# CC stands for coordinatcleaner

fwrite(occ_data_taxa_date_bias, "occurrences_missing_sarbecobats_spocc_2021_cc.csv")

##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################

colourCount = length(unique(auxa$sp_enm))
palette <- randomcoloR::distinctColorPalette(colourCount)

g <- ggplot() + geom_polygon(data = area,
                             aes(x=long, y = lat, group = group),
                             fill = "grey77", color = "lightgrey", size=0.04) + #Note que voce pode mudar as cores do fundo e da borda
  coord_fixed(1.1) + #Use isto para o mapa ficar proporcional
  geom_point(data = occ_data_taxa_date_bias, aes(x = long, y = lat, fill= sp_enm), 
             shape = 21, 
             size = 3, #Tamanho dos pontos
             alpha = 0.6) + #Transparencia: quanto mais proximo de 1, menos transparente
  theme_bw() +
  ggtitle('Bats') + #De nome ao plot, caso seja necessario
  labs(x="Longitude", y = "Latitude") + #De nome aos eixos
  theme(text = element_text(size=14), #Ajuste os tamanhos das fontes 
        plot.title = element_text(size=20, hjust=0.5),
        axis.text.x = element_text(size = 10, angle=0, hjust=1),
        axis.text.y = element_text(size = 10, angle=0, vjust=1),
        axis.title.x = element_text(size = 12, angle=0),
        axis.title.y = element_text(size = 12, angle=90)) +
  xlab('Long') + ylab('Lat') + ggtitle("") + 
  scale_fill_manual(values =palette, name= "Hosts") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', size = 0.5), panel.background = element_rect(fill =  'aliceblue'))


#ggspatial::annotation_scale(location = 'bl', width_hint = 0.1) +
#ggspatial::annotation_north_arrow(location = 'bl', which_north = 'grid', pad_x = unit(0.75, 'in'),
#                                   pad_y = unit(0.5, 'in')) 


g

##############################################################################################################
# Export map
##############################################################################################################

ggsave(paste0("map_todos_plus_inat_research", str_to_lower(sub(" ", "_", 'all')), ".tiff"), dpi = 300)


