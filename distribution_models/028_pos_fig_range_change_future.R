#-------------------------------------------------------------------------------------------------------------
# Potential occupied area calculation for present and future (SLOW)
# Figure Supplements - go straight to line 372 after loading packages and settings
# Potential ranges in the future
# Be awared of species names - manual code (L293)
#-------------------------------------------------------------------------------------------------------------

source('00_packages.R')

source('01_settings.R')

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

################################################################################
# Future ranges

start <- print(Sys.time())

setwd(enmresultsdir)
setwd('Projection')
recebe <- data.frame()

for(f in list.files() ) {
  print(f)


    setwd(aa)
  
  message('SLOW')
  
  all_aa <- stack(list.files(pattern='.tif$'))
  
  setwd(binrasterdir)
  
  # Binary in future
  setwd(enmresultsdir)
  setwd('Projection')
  setwd(paste0(f,'/Ensemble/W_MEAN/MAX_TSS/'))
  
  allfut <- stack(list.files(pattern='.tif$'))
  
  # spatial restriction
  
  presm <- mask(allfut, all_aa)
  
  # removing zeroes
  
  presm[values(presm)==0] <- NA
  
  
  recebem <- data.frame()
  
  # Species level
  for(m in names(presm)){
    print(m)
    
    arm <- area(presm[[m]], na.rm=TRUE) # ## applies a correction for latitude, to km2
    
    tempm <- sum(values(arm ), na.rm = TRUE)
    
    recebem <- rbind(recebem, tempm)
  }
  
  # Projection unit level
  recebem$sp <- names(presm)
  
  recebem$binomial<- sub('_', ' ', firstup(recebem$sp) )
  
  recebem$sp <-NULL
  
  colnames(recebem) <- c('Potential range', 'binomial')
  
  recebem$facet <- sub(other_suffix, '', f  )
  
  recebem$period <- str_extract(f, pattern = periods_fut)
  
  recebem$scenario <- str_extract(f, pattern = pathways)
  
  # Important manual setting here if you opt for more GCMs, see: noquote(unique(paste0(gcms, sep='|')) )
  
  recebem$gcm <- str_extract(f, pattern = 'BCC-CSM2-MR|CanESM5' )
  
  recebe <- rbind(recebe, recebem)
  
}

# Final dataset with future to be merged with present

futranges <- recebe 

futranges %>% group_by(binomial) %>% skim()

################################################################################
#### Present ranges restricted by accessible area and MSDM OBR a posteriori
# Reading Predicted ranges (binary MSDM files overlapped with accessible areas)
# Open files from ENMs
# Open accessible area M for near current #####################################

setwd(aa)

all_aa <- stack(list.files(pattern='.tif$'))

setwd(binrasterdir)

all <- stack(list.files(pattern='.tif$'))

# over to restrict them spatially

allm <- mask(all, all_aa)

# Removing zeroes

allm[values(allm)==0] <- NA

recebem <- data.frame()

for(m in names(allm)){
  print(m)
  
  arm <- area(allm[[m]], na.rm=TRUE) # ## applies a correction for latitude, to km2
  
  tempm <- sum(values(arm ), na.rm = TRUE)
  
  recebem <- rbind(recebem, tempm)
}

recebem$sp <- names(allm)

recebem$binomial<- sub('_', ' ', firstup(recebem$sp) )

recebem$sp <-NULL

colnames(recebem) <- c('Potential range', 'binomial')

presentranges <- recebem

presentranges$facet <- 'Present'
presentranges$period <- 'Present'
presentranges$scenario <- 'Present'
presentranges$gcm <- 'Present'
colnames(presentranges)
colnames(futranges)

allranges <- rbind(presentranges, futranges)

allranges %>% group_by(binomial) %>% skim()

allranges
#################################################################################################################
# Calculating accessible area area
#################################################################################################################
setwd(aa)

all_aa <- stack(list.files(pattern='.tif$'))

recebea <- data.frame()

for(i in names(all_aa)){
  print(i)
  
  ar <- area(all_aa[[i]], na.rm=TRUE) ### applies a correction for latitude, to km2
  
  temp <- sum(values(ar ), na.rm = TRUE)
  
  recebea <- rbind(recebea, temp)
}

recebea$sp <- names(all_aa)

recebea$binomial<- sub('_', ' ', firstup(recebea$sp) )

recebea$sp <-NULL

colnames(recebea)<- c('accessible area sqkm', 'binomial')

recebea

allrangesa <- left_join(allranges, recebea, by='binomial')

head(allrangesa)


###############################################################################################################
#### Present ranges restricted by accessible BUT NOT MSDM-RESTRICTED
#BUT NOT MSDM-RESTRICTED
#BUT NOT MSDM-RESTRICTED
#BUT NOT MSDM-RESTRICTED
###############################################################################################################

setwd(binrasterdirn) # no-MSDM files

alln <- stack(list.files(pattern='.tif$'))

# over to restrict them spatially

allmn <- mask(alln, all_aa)

# Removing zeroes

allmn[values(allmn)==0] <- NA

recebemn <- data.frame()

for(m in names(allmn)){
  print(m)
  
  armn <- area(allmn[[m]], na.rm=TRUE) ### applies a correction for latitude, to km2
  
  tempmn <- sum(values(armn ), na.rm = TRUE)
  
  recebemn <- rbind(recebemn, tempmn)
}

recebemn$sp <- names(allmn)

recebemn$binomial<- sub('_', ' ', firstup(recebemn$sp) )

recebemn$sp <-NULL

colnames(recebemn) <- c('Potential range', 'binomial')

presentrangesn <- recebemn

presentrangesn$facet <- 'Present no msdm'
presentrangesn$period <- 'Present no msdm'
presentrangesn$scenario <- 'Present no msdm'
presentrangesn$gcm <- 'Present no msdm'

hist(presentranges$`Potential range`- presentrangesn$`Potential range`)

# Check
colnames(presentrangesn)

allrangesan <- rbind(allranges, presentrangesn)

# Adding accessible area

allranges_table <- left_join(allrangesan, recebea, by='binomial')

end <- print(Sys.time())

print(end-start) # Time difference of 25.9 mins

##############################################################################################################
#
#                                          Figures and tables
#
##############################################################################################################

fig <- paste0('Figure_all_ranges_', version_suffix, '.jpg')

# Range figure for near current. I also should add the average estimated range as dashed lines

rmodel <- allranges_table %>%  
  drop_na() %>% 
  ggplot(aes(x=na.omit(`accessible area sqkm`),  y=`Potential range`, label=binomial, col=period )) + 
  facet_grid(.~scenario)+
  geom_point(col="#56B4E9")+
  stat_smooth()+
  theme_bw()+
  xlab('Accessible area (sqkm)') +
  ggtitle( '' )+
  ylab('Estimated occupied area (sqkm)'  ) + geom_text(size=4, fontface = "italic") +
  theme(plot.title.position = "plot",axis.title=element_text(size=15),legend.position="bottom", legend.direction="horizontal")

rmodel

# Save df so you do not need to run the process again

allrangesb <- allranges_table
allrangesb[allrangesb$scenario =='Present', 'scenario'] = NA
table(allrangesb$binomial)

# Range fig 5 ------------------------------------------------------------------------------------------------
colourCount = length(unique(allranges_table$binomial))
palette <- randomcoloR::distinctColorPalette(colourCount)
palette <- viridis::viridis(n = colourCount)

# With log in one panel---------------------------------------------------------------------------------------
allrangesb$`Potential range mi` <- allrangesb$`Potential range`/1000000

head(allrangesb)
unique(allrangesb$gcm)

###Families

require(tidyverse)
require(janitor)

rangefamily <- allrangesb %>%  mutate(family = fct_recode(binomial,
                                             'Hipposideridae' = "Aselliscus stoliczkanus" ,
                                             'Hipposideridae' = "Hipposideros armiger" ,
                                             'Hipposideridae' = "Hipposideros galeritus" ,
                                             'Hipposideridae' = "Hipposideros larvatus",
                                             'Hipposideridae' = "Hipposideros pomona"   ,
                                             'Hipposideridae' = "Hipposideros ruber"  ,
                                             'Rhinolophidae' =  "Rhinolophus affinis",   
                                             'Rhinolophidae' = "Rhinolophus blasii",    
                                             'Rhinolophidae' =  "Rhinolophus euryale"  ,  
                                             'Rhinolophidae' =  "Rhinolophus ferrumequinum" ,
                                             'Rhinolophidae' = "Rhinolophus hipposideros" ,
                                             'Rhinolophidae' = "Rhinolophus macrotis",
                                             'Rhinolophidae' ="Rhinolophus malayanus" ,
                                             'Rhinolophidae' = "Rhinolophus mehelyi" ,
                                             'Rhinolophidae' =  "Rhinolophus pearsonii" ,
                                             'Rhinolophidae' = "Rhinolophus pusillus" ,    
                                             'Rhinolophidae' = "Rhinolophus sinicus" ,
                                             'Rhinolophidae' =  "Rhinolophus thomasi"   ,
                                             'Miniopteridae' = "Miniopterus schreibersii",
                                             'Molossidae' = "Chaerephon plicatus",
                                             'Vespertilionidae' = "Tadarida teniotis"  ,
                                             'Vespertilionidae' = "Nyctalus leisleri" ,      
                                             'Vespertilionidae' = "Plecotus auritus",         ))
#
rangefamily %>% 
  filter(!(rangefamily$scenario=="Present no msdm"), !(allrangesb$gcm=="CanESM5") ) %>%
ggplot(aes(x =period , y = `Potential range mi`, group=binomial )) +
  geom_line(aes(color = family, size=1.5)) +
  geom_point(aes(color = family, size=3)) +
  scale_color_manual(values= randomcoloR::distinctColorPalette(5) ) +
  theme_bw()+
  coord_trans(y="log2")+
  theme(legend.position = "right") +
  xlab( 'Period') +
  ylab( 'Potential range area (log million sqkm)') +
  guides(size = "none") +
  facet_wrap(.~scenario)+
  labs(color = "Species") + 
  theme_bw(base_size = 15)+
  theme(plot.title.position = "plot", axis.title=element_text(size=15),
        axis.text.y = element_text(face = "bold", size = 16, angle = 0, vjust = 0.5, hjust=1), 
        axis.text.x = element_text(face = "bold", size = 16, angle = 0, vjust = 0.5, hjust=1) ,
        legend.text = element_text(face = "italic", size = 12))

ggsave(filename = fig_range, width =28, height = 39, units = 'cm', dpi=600, rcompo)

table(allrangesb$period)

table(allrangesb$gcm)

table(allrangesb$scenario)

# Facetted 
# Original fig 5
rangefamily %>% 
  #filter(!(rangefamily$scenario=="Present no msdm"), !(allrangesb$gcm=="CanESM5") ) %>%
  filter(rangefamily$scenario=="ssp585" & rangefamily$gcm=="BCC-CSM2-MR") %>% 
  ggplot(aes(x =period , y = `Potential range mi`, group=binomial )) +
  facet_wrap(.~binomial)+
  geom_line(aes(color = binomial), size=1.2) +
  geom_point(aes(color = binomial, size=3)) +
  scale_color_manual(values=palette) +
  xlab( 'Period') +
  ylab( 'Potential range area (log million sqkm)') +
  theme(legend.position = "top") +theme_bw() + coord_trans(y="log2")+
    guides(size = "none")+
  theme(plot.title.position = "plot", axis.title=element_text(size=15),
        axis.text.y = element_text(face = "bold", size = 8, angle = 0, vjust = 0.5, hjust=1), 
        axis.text.x = element_text(face = "bold", size = 14, angle = 90, vjust = 0.5, hjust=1),
        legend.text = element_text(face = "italic", size = 12),
        strip.text = element_text(face = "italic", size = 12, colour = "black", angle = 0))
# New code

unique(rangefamily$period)
       
head(rangefamily)

rangefamily 

# Figure s12----------------------------------------------------------------------------------------------------
# old fig 5

setwd(here())
setwd('hotspots')

# Exporting Data for this fig so we do not need to run the slow code again -----------------------------------
#write.xlsx(rangefamily, 'ranges_data_Fig5.xlsx')

rangefamily <- read.xlsx( 'ranges_data_Fig5.xlsx', sheetIndex = 1)


#head(rangefamily)
setwd(here())
setwd('hotspots')
rangefamily$`Potential range mi` <- rangefamily$Potential.range.mi

fig5r1 <- rangefamily %>% 
 mutate(periodp = recode_factor(period,
                                   "Present" =  "Present"   ,
                                   "2021-2040" = "2040"  ,  
                                   "2041-2060"=  "2060"    , 
                                   "2061-2080"=  "2080"   ,  
                                   "2081-2100" =  "2100"     ,  
                                   "Present no msdm" =  "Present no msdm") ) %>% 
  filter(!(rangefamily$period=="Present no msdm"), !(rangefamily$gcm=="CanESM5") ) %>%
   ggplot(aes(x =periodp , y = `Potential range mi`, group=scenario )) +
  facet_wrap(.~binomial, nrow=6)+
  geom_line(aes(color = scenario), size=1.2) +
  geom_point(aes(color = scenario, size=3)) +
  xlab( '') +
  ylab(   expression( sqrt("Area in million "  ~km^2 ))    ) +
   labs(color = "")+
  coord_trans(y="sqrt")+
  scale_fill_manual(values = c("goldenrod2","firebrick3"), na.value="dodgerblue4", labels = c( "Projection SSP245","Projection SSP585", 'Prediction')) +
  scale_color_manual(values = c("goldenrod2","firebrick3"),na.value="dodgerblue4", labels = c("Projection SSP245","Projection SSP585", 'Prediction')) +
  guides(size = "none")+
  theme_bw(base_size = 15) +
  theme(legend.position = "top", plot.title.position = "plot", axis.title=element_text(size=15),
        axis.text.y = element_text(face = "bold", size = 14, angle = 0, vjust = 0.5, hjust=1), 
        axis.text.x = element_text(face = "bold", size = 14, angle = 90, vjust = 0, hjust=1),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "italic", size = 11, colour = "black", angle = 0)) 


fig5r1

ggsave(fig5r1, filename = 'Fig_ranges_sup_R1.png', width =26, height = 38, units = 'cm', dpi=400)


#######################################

# Liquid gain or lost area
p <- allranges_table %>%  
  filter((allranges_table$period=="Present") ) %>% arrange(binomial)

p 

# Not restricted by msdm

pn <- allranges_table %>%  
  filter((allranges_table$period=="Present no msdm") ) %>% arrange(binomial)

options(digits = 3, scipen = 999)

pn$`Potential range`- p$`Potential range` 

p$`accessible area sqkm` -pn$`Potential range`

# Make the loop for all
# Caption for now: Ranges for the period "2081-2100" , "ssp585"  for the "BCC-CSM2-MR" GCM

f2100_585 <- allranges_table %>% 
  filter(allranges_table$period=="2081-2100" & allranges_table$scenario=="ssp585" & allranges_table$gcm=="BCC-CSM2-MR" ) %>% arrange(binomial) %>% 
  dplyr::select(binomial, Future_range = 'Potential range')

p2 <- left_join(p,f2100_585, by='binomial' )

p2$dif2100_585<- p2$`Future_range`-p2$`Potential range` 

f2100_245 <- allranges_table %>% 
  filter(allranges_table$period=="2081-2100" & allranges_table$scenario=="ssp245" & allranges_table$gcm=="BCC-CSM2-MR" ) %>% arrange(binomial) %>% 
  dplyr::select(binomial, Future_range = 'Potential range')

p3 <- left_join(p2,f2100_245 , by='binomial' )

p3$dif2100_245 <- p3$`Future_range.y`-p3$`Potential range` 

# Negative values is range contraction
# Positive values is range expansion

hist(p2$dif2100)

table_status <- p3 %>% 
  mutate(shift_ssp585_2100 = factor(ifelse(dif2100_585 < 0, "contraction", "expansion") ) ) %>% 
  mutate(shift_ssp245_2100 = factor(ifelse(dif2100_245 < 0, "contraction", "expansion") ) ) %>% 
  arrange(shift_ssp585_2100) %>% 
  dplyr::select(binomial, 'Potential range', Future_range.x, shift_ssp585_2100, 'accessible area sqkm', Future_range.y, shift_ssp245_2100) %>% 
  arrange(binomial)


table(table_status$shift_ssp245_2100)

table(table_status$shift_ssp585_2100)

# Now generating percentages of shift in comparison to present

table_status$pct_shift_ssp585_2100 <-  100*(table_status$`Future_range.x`/ table_status$`Potential range` ) -100


# /1000000 to report in millions of sqkm

table_status$Potrange_misqkm <- p3$`Potential range` /1000000

table_status %>% arrange(pct_shift_ssp585_2100)

# 1.13 million sqkm for R affinis versus 1.9 million sqkm in Sanches et al.

###################### Averaging through scenarios
# Continue from here

allranges_table %>%
  group_by(binomial, period, scenario) %>% summarise_at(vars(`Potential range`), funs(mean, max, sd)) %>% 
  filter(scenario=="ssp245" )

allranges_table %>%
  group_by(binomial, period, scenario) %>% summarise_at(vars(`Potential range`), funs(mean, max, sd)) %>% 
  filter(scenario=="ssp585" )

rfutmean585 <- allranges_table %>%
  filter(allranges_table$period=="2081-2100" & allranges_table$scenario=="ssp585") %>%  
  group_by(binomial, period, scenario) %>% summarise_at(vars(`Potential range`), funs(mean)) 

rfutmean245 <- allranges_table %>%
  filter(allranges_table$period=="2081-2100" & allranges_table$scenario=="ssp245") %>%  
  group_by(binomial, period, scenario) %>% summarise_at(vars(`Potential range`), funs(mean)) 

hist(rfutmean245$`Potential range` - rfutmean585$`Potential range`)

summary(rfutmean245$`Potential range` - rfutmean585$`Potential range`)

100 - 100*(rfutmean585$`Potential range`/ rfutmean245$`Potential range` )

plot(rfutmean585$`Potential range`, rfutmean245$`Potential range`)


both_scenarios_avg <- left_join(rfutmean245,rfutmean585, by='binomial' )

both_scenarios_avg$area_loss585 <- both_scenarios_avg$`Potential range.y` -both_scenarios_avg$`Potential range.x`

both_scenarios_avg$area_loss585


################## Exporting

setwd(projdir)

if (!dir.exists('range_tables')) {dir.create('range_tables')}

setwd('range_tables')

options(scipen=999, digits= 2)

write.xlsx(table_status, tabname, row.names = FALSE) 


tabnameall <- paste0('all_ranges',version_suffix, '.xlsx')

write.xlsx(allrangesb, tabname, row.names = FALSE) 

##############################################################################################################
# Bar Figure to do

rfutmean

fr <- allcompm %>%   
  filter(! (allcompm$binomial=="Rhinolophus blythi") ) %>% 
  ggplot(aes(y=reorder(binomial,sum), x=sum, fill=has_sdm))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  xlab( 'IUCN polygon area (sqkm)') +
  ylab('Sarbecovirus host species') +
  geom_vline(aes(xintercept= mean(na.omit(sum))),
             color="black", linetype="dashed", size=1)+
  labs(fill='Modelled?')+
  ggtitle( 'A.' )+
  coord_flip()+
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=15),axis.text.x = element_text(face = "italic", size = 13.5, angle = 90, vjust = 0.5, hjust=1) )

fr

setwd('hotspots')

fig_range <- paste0('Figure_range_', version_suffix, '.jpg')

rcompo <- gridExtra::grid.arrange(fr,rmodel, nrow = 2, ncol=1)

rcompo

#ggsave(filename = fig_range, width =28, height = 39, units = 'cm', dpi=600, rcompo)
