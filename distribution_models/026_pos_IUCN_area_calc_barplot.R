###############################################################################################################
#  Script to generate range maps areas  for each target species
# Generates data containing mined cleaned data and data intersecting with IUCN or bat handbook polygons
###############################################################################################################

# Figure S7

source('00_packages.R')

source('01_settings.R')

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

### Open files

setwd(dynmaster)

file_all <- read.table(master, header=TRUE)

setwd(projdir)
iucndir <- 'D:/OneDrive - Massey University/PD_2021/distribution_models/iucn_shapefile/'
iucn <- sf::st_read(paste0(iucndir,'/MAMMALS_TERRESTRIAL_ONLY.shp'))

file_all$x <- file_all$long
file_all$y <- file_all$lat

targets <-  sub('_', ' ', unique(file_all$sp))

targets <- firstup(targets) 
targets

# Tag modelled versus non-modelled species (lack of enough data)

setwd(binrasterdir)

o1 <-   sub('_', ' ', list.files()) 
o2  <-  sub('.tif', '', firstup(o1) ) 

t <- data.frame(binomial=targets, has_sdm= targets %in%  o2  )
t$has_sdm <- as.factor(t$has_sdm)

nrow(allcomp)
nrow(t)
t
# Slow

iucn$binomial %in% t$binomial

bi <- iucn[iucn$binomial %in% t$binomial,]

head(bi)

iucn_area <- bi %>% group_by(binomial) %>% summarise(sum = sum(SHAPE_Area))  %>% arrange(desc(sum))

iad <- as.data.frame(iucn_area)

iad$geometry <- NULL

iad

# Compare with accessible area size from occurrenses mathing Olson

cornutus <- sf::st_read(paste0(iucndir,'IUCN_range//Rhinolophus cornutus//data_0.shp'))

cornutus$SHAPE_Area <- as.vector(sf::st_area(cornutus)) /10^9 #(from sqm to sqkm)

# R blythi had no range in September 2021
#https://www.iucnredlist.org/search?query=Rhinolophus%20blythi&searchType=species

iucn[iucn$binomial %in% 'Rhinolophus lepidus',]

# Other species with ranges extra-IUCN shapefile

mono <- sf::st_read(paste0(iucndir,'IUCN_range//Rhinolophus formosae//data_0.shp'))

mono$SHAPE_Area <- as.vector(sf::st_area(mono)) /10^9

gentilis <- iucn[iucn$binomial %in% 'Hipposideros gentilis',]

ca <-  as.data.frame(cornutus %>% group_by(abbrev) %>% summarise(sum = sum(SHAPE_Area))  %>% arrange(desc(sum)) )

ma <- as.data.frame(mono %>% group_by(BINOMIAL) %>% summarise(sum = sum(SHAPE_Area))  %>% arrange(desc(sum)) )

ga <- as.data.frame(gentilis %>% group_by(binomial) %>% summarise(sum = sum(SHAPE_Area))  %>% arrange(desc(sum)) )

ca$geometry <- NULL
ma$geometry <- NULL
ga$geometry <- NULL
ca$binomial <- 'Rhinolophus cornutus'

colnames(ca) <- c('binomial', 'sum')
colnames(ma) <- c('binomial', 'sum')

ma[1, 'binomial'] <- 'Rhinolophus monoceros'
ca[1, 'binomial'] <- 'Rhinolophus cornutus'

ca[3] <- NULL

iadc <- rbind(iad, ca)
iadc <- rbind(iadc, ma)
iadc <- rbind(iadc, ga)

iadc <- iadc %>% arrange(desc(sum))

iadc

#######
# Calculate area from modelled species

setwd(aa)

all_aa <- stack(list.files(pattern='.tif$'))

# Calculating accessible areas total area

recebe <- data.frame()

message('SLOW')

for(i in names(all_aa)){
  print(i)
  
  ar <- area(all_aa[[i]], na.rm=TRUE) # ## applies a correction for latitude, to km2
  
  temp <- sum(values(ar), na.rm = TRUE) 
  
  recebe <- rbind(recebe, temp)
}

recebe$sp <- names(all_aa)
recebe$binomial<- sub('_', ' ', firstup(recebe$sp) )

recebe$sp <-NULL
colnames(recebe)<- c('accessible area sqkm', 'binomial')
recebe

allcomp <- merge(iadc, recebe, by=c('binomial'), all.x=TRUE)

message('Number of species: ', nrow(allcomp))

allcomp 

# Merging

allcompt <- merge(t, allcomp,  by=c('binomial'), all.x=TRUE)

allcompt %>%  arrange(has_sdm) 

# Reading Predicted ranges (binary MSDM files overlapped with accessible areas)
# Open files from ENMs
# Open accessible area M for near current

message('SLOW')

setwd(aa)

all_aa <- stack(list.files(pattern='.tif$'))

setwd(binrasterdir)

all <- stack(list.files(pattern='.tif$'))

# spatial restriction

allm <- mask(all, all_aa)

# removing zeroes

allm[values(allm)==0] <- NA

table(values(allm[[1]]))

recebem <- data.frame()

for(m in names(allm)){
  print(m)
  
  arm <- area(allm[[m]], na.rm=TRUE) # applies a correction for latitude, to km2
  
  tempm <- sum(values(arm ), na.rm = TRUE)
  
  recebem <- rbind(recebem, tempm)
}

recebem$sp <- names(allm)

recebem$binomial<- sub('_', ' ', firstup(recebem$sp) )

recebem$sp <-NULL

colnames(recebem) <- c('Modelled potential range', 'binomial')

recebem

allcompm <- merge(allcompt, recebem, by=c('binomial'), all.x=TRUE)

allcompm

fig <- paste0('Figure_range_current_',version_suffix, '.jpg')

allcompm %>%  
  drop_na("Modelled potential range") 

# Range figure for near current. I also should add the average estimated range as dashed lines

rmodel <- allcompm %>%  
  drop_na() %>% 
  ggplot(aes(x=na.omit(allcompm$`accessible area sqkm`),  y=`Modelled potential range`, size=sum*10^4, label=binomial )) + 
  geom_point(col="#56B4E9")+
  theme_bw()+
  xlab('Accessible area (sqkm)') +
  labs(size='IUCN polygon area (sqkm)')+
  ggtitle( 'B.' )+
  ylab('Estimated occupied area (sqkm)'  )+ geom_text(size=4, fontface = "italic") +
  theme(plot.title.position = "plot",axis.title=element_text(size=15),legend.position="bottom", legend.direction="horizontal")

rmodel

# Bar Figure
options(scipen = 999)

fr <- allcompm %>%   
  filter(! (allcompm$binomial=="Rhinolophus blythi") ) %>% 
  ggplot(aes(y=reorder(binomial,sum*10^4), x=sum*10^4, fill=has_sdm))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  xlab( 'IUCN polygon area (sqkm)') +
  ylab('Sarbecovirus host species') +
  geom_vline(aes(xintercept= mean(na.omit(sum*10^4))),
             color="black", linetype="dashed", size=1)+
  labs(fill='Modelled?')+
  ggtitle( 'A.' )+
  coord_flip()+
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=15),
        axis.text.x = element_text(face = "italic", size = 13.5, angle = 90, vjust = 0.5, hjust=1) )


fr

# Exporting figure 
setwd(projdir)

setwd('hotspots')

fig_range <- paste0('Fig_occupied_area_S7_', version_suffix, '.jpg')

rcompo <- gridExtra::grid.arrange(fr, rmodel, nrow = 2, ncol=1)

ggsave(filename = fig_range, width =28, height = 39, units = 'cm', dpi=600, rcompo)

#############################################################################################################
