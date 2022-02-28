# Range overlap shifts SSP585---------------------------------------------------------------------------------

options(digits = 3, scipen = 999)

setwd(here())

source('00_packages.R')

source('01_settings.R')

# Open richness map ---------------------
setwd(aa)

all_aa <- stack(list.files(pattern='.tif$'))

setwd(binrasterdir)

all <- stack(list.files(pattern='.tif$'))

allm <- mask(all, all_aa)

t1 <- allm$rhinolophus_affinis + allm$rhinolophus_malayanus

table(values(t1)) #1037 px overlapping

t1[values(t1)!=2] <- NA

sum(values(area(t1, na.rm=TRUE)), na.rm=TRUE) ##760080

allm[values(allm)==0] <- NA # this step changes everything

# Numerical proof for running L30 before the loop

table(values(allm))

t2 <- allm$rhinolophus_affinis + allm$rhinolophus_malayanus #311859

plot(t2)

sum(values(area(t2, na.rm=TRUE)), na.rm=TRUE) 

# RM range max 338876
# So RM overlaps with RA in 92% of its range :)

sum(values(area(t2, na.rm=TRUE)), na.rm=TRUE)  /338876

# RA range max 1138906.151
# And RA overlaps with RM in 27% of its range

sum(values(area(t2, na.rm=TRUE)), na.rm=TRUE)  /1138906.151


#calculating overlap ------------------
recebe <- data.frame()

for(i in names(allm))
  for(j in rev(names(allm)) )
  {
    if (i == j){
      next
    }
    summed <- allm[[i]] + allm[[j]]
    
    values(summed)[values(summed) != 2] = NA
    
    ar <- area(summed, na.rm=TRUE)
    
    meui <- i
    meuj <- j
    
    areasoma <- sum(values(ar),  na.rm = TRUE)
    
    aux <- data.frame(sp1= meui, sp2=meuj , area_overlapped= areasoma)
    recebe <- rbind(recebe, aux)
    
    print(i)
    print(j) 
  }

head(recebe)

getwd()

rexpo <- recebe %>% arrange(desc(area_overlapped))

setwd(here())

setwd('range_tables')

write.xlsx(rexpo, 'range_overlap_present_R1.xlsx', row.names = FALSE)

# Then plot a heatmap ---------------------------------------------------------------------------------------=
# Add percentages --------------------------------------------------------------------------------------------

setwd('range_tables')

withself <- read.xlsx('range_overlap_present_R1.xlsx', sheetIndex = 1)

omatrix <- reshape(withself, direction="wide", idvar="sp2", timevar="sp1")

omatrix

withself$sp1 <- sub('_', ' ', stringr::str_to_sentence(withself$sp1))

withself$sp2 <- sub('_', ' ', stringr::str_to_sentence(withself$sp2))

col.range=c(10000,max(withself$area_overlapped))

oheat <- ggplot(data=withself, aes(y = sp1, x=sp2, fill =area_overlapped ) ) +
  geom_tile(alpha=0.75) + scale_fill_viridis( option = "D", direction=-1,  trans = "log",
                                              limits=col.range ) + 
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=13,  face = "italic"))+
  ylab('') +xlab('') +labs(fill = "Area overlap (sqkm)        ") 

oheat

fign <- paste0('Fig_overlap_present_R1_', version_suffix, '.jpg')

ggsave(filename = fign, oheat, width =33, height = 29, units = 'cm', dpi=600)

#### Adding future
withselff <- read.xlsx('range_overlap_future_R1.xlsx', sheetIndex = 1)
head(withselff)

omatrixf <- reshape(withself, direction="wide", idvar="sp2", timevar="sp1")

omatrixf

withselff$sp1 <- sub('_', ' ', stringr::str_to_sentence(withselff$sp1))

withselff$sp2 <- sub('_', ' ', stringr::str_to_sentence(withselff$sp2))

oheatfut <- ggplot(data=withselff, aes(y = sp1, x=sp2, fill =area_overlapped_future ) ) +
  geom_tile(alpha=0.75) + scale_fill_viridis( option = "D", direction=-1,  trans = "log",
                                              limits=col.range) + 
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=13,  face = "italic"))+
  ylab('') +xlab('') +labs(fill = "Future area overlap (sqkm)") 

oheatfut
getwd()

compo <- gridExtra::grid.arrange(oheat, oheatfut, ncol = 2)
compo
#fign <- paste0('Fig_overlap_future', version_suffix, '.jpg')
#ggsave(filename = fign, oheatfut, width =33, height = 29, units = 'cm', dpi=600)

# Left join

withdif <- left_join(withself, withselff)

withdif$dif <- withdif$area_overlapped_future -withdif$area_overlapped
hist(withdif$dif)

summary(withdif$dif)
col.range1=c(min(withdif$dif),max(withdif$dif))

#496657 
withdif$area_overlapped_future
withdif$area_overlapped

# Negative values is area overlap loss in the future
withdif$dif
plot(withdif$dif   ~ scale(withdif$dif))
#limits=col.range1
oheatdif<- ggplot(data=withdif, aes(y = sp1, x=sp2, fill =scale(dif) ) ) +
  geom_tile(alpha=0.75) + scale_fill_viridis( option = "D", direction=-1               ) + 
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=13,  face = "italic"))+
  ylab('') +xlab('') +labs(fill = "Shift in future area overlap (scaled sqkm)") 

oheatdif

getwd()
summary(withdif$dif)

oheatdifr<- ggplot(data=withdif, aes(y = sp1, x=sp2, fill =dif ) ) +
  geom_tile(alpha=0.75) + scale_fill_viridis( option = "D", direction=-1    ,
                                              breaks=c(400000, 0, -500000 ,-1000000, -1500000,-2000000) ) + 
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=13,  face = "italic"))+
  ylab('') +xlab('') +labs(fill = "Shift in future area overlap (sqkm)") 

oheatdifr
figr <- paste0('Fig_overlap_supplents_', version_suffix, '.jpg')
#ggsave(filename = figr, oheatdifr, width =33, height = 29, units = 'cm', dpi=600)


#categories of overlap shift---------------------------------------------------------------------------------
withdif$difpct <- withdif$area_overlapped_future / withdif$area_overlapped 

require(tidyverse)
require(janitor)

withdif$category <- ifelse(withdif$difpct == 'NaN', "Never overlaps",
                           ifelse(withdif$difpct == 'Inf', "Starts overlapping in the future", 
                                  ifelse(withdif$difpct == 0, "Stops overlaping in the future",
                                         ifelse(withdif$difpct <  1, "Overlaps less in the future",
                                                ifelse(withdif$difpct > 1, "Overlaps more in the future", NA)))))

table(withdif$difpct == 0)
0/0
1/0
0/1

withdif$difpct == 'Inf'
withdif

mypali <- viridisLite::viridis(5)

oheatdifpct <- ggplot(data=withdif, aes(y = sp1, x=sp2, fill =category ) ) +
  geom_tile(alpha=0.75) + scale_fill_manual(values=mypali )+
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=13,  face = "italic"))+
  ylab('') +xlab('') +labs(fill = "Shift in area overlap") 

oheatdifpct

head(withdif)

fign <- paste0('Fig_overlap_shift_', version_suffix, '.jpg')
#ggsave(filename = fign, oheatdifpct, width =33, height = 29, units = 'cm', dpi=600)
#write.xlsx(withdif, 'range_overlap_shift.xlsx', row.names = FALSE)

# Real percent (difference)

oheatdifpctr1 <- ggplot(data=withdif, aes(y = sp1, x=sp2, fill = difpct ) ) +
  geom_tile(alpha=0.75) + 
  #scale_fill_manual(values=mypali )+
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=13,  face = "italic"))+
  ylab('') +xlab('') +labs(fill = "Shift in % area overlap") 

oheatdifpctr1

# Filter

head(withdif)

withdif %>% arrange(dif)

table(withdif$category)

stops <- withdif %>%  filter(category == 'Stops overlaping in the future')

stops

less <- withdif %>%  filter(category == 'Overlaps less in the future')

hist(less$dif)


230+138+110+24+4

23*23

506-529

nrow(less)

more <- withdif %>%  filter(category == 'Overlaps more in the future')
hist(more$dif)

more %>%  arrange(-difpct)

max(more$difpct)

# Summary table merge
list.files()
  
trange <- read.xlsx('Table_Range_results_40o_ss6_maxent_15rep.xlsx', sheetIndex = 1)

head(trange)
head(withdif)

withdif$species <- withdif$sp1

suptable <- left_join(withdif, trange)

head(suptable)

suptable$pct_range_over_present <- suptable$area_overlapped / suptable$Potential.range

# Add percentages, export supplementary info

#-------------------------------------------------------------------------------------------------------------