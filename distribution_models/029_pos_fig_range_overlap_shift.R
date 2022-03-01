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

setwd(here())

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
        axis.text.x =  element_text(angle = 90, hjust = 0.99, size=13,  face = "italic"))+ #element_blank() ) +
  ylab('') +xlab('') +labs(fill = "Area overlap (sqkm)        ") 

oheat

fign <- paste0('Fig_overlap_present_R1_', version_suffix, '.jpg')

ggsave(filename = fign, oheat, width =33, height = 29, units = 'cm', dpi=600)

#### Adding future SSP585 ---------------------------

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
        axis.text.x = element_text(angle = 90, hjust = 0.99, size=13,  face = "italic"))+
  ylab('') +xlab('') +labs(fill = "Future area overlap (sqkm)") 

oheatfut

compo <- gridExtra::grid.arrange(oheat, oheatfut, ncol = 1, nrow=2)
compo

figcompo <- paste0('Fig_overlap_present_future_', version_suffix, '.jpg')
ggsave(filename = figcompo, compo, width =26, height = 37, units = 'cm', dpi=600)

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
nrow(withdif)

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
summary(withdif$area_overlapped_future)

withdif$species <- withdif$sp1

suptable <- left_join(withdif, trange)

head(suptable)

# % of overlap in regard to present range of column species

suptable$pct_range_overlap_present <- suptable$area_overlapped / suptable$Potential.range

# % of overlap in regard to future rage of column species

suptable$pct_range_overlap_future <- suptable$area_overlapped_future / suptable$Future.range.SSP.585.BCC

hist(suptable$pct_range_overlap_future )

# Add percentages, export supplementary info
options(digits=3)

summary(suptable$pct_range_overlap_present)
quantile(suptable$pct_range_overlap_present)


suptable %>%  mutate(pct_range_present_cat = ifelse(pct_range_overlap_present <= 0.5, "<50%", ">50%"), 
         pct_range_present_cat = factor(pct_range_present_cat)) %>%
  tabyl(pct_range_present_cat)

suptable %>%  mutate(pct_range_future_cat = ifelse(pct_range_overlap_future <= 0.5, "<50%", ">50%"), 
                     pct_range_future_cat = factor(pct_range_future_cat)) %>%
  tabyl(pct_range_future_cat)

head(suptable)

supexpo <- suptable %>% arrange(species)

#exporting ordered by species

ncol(supexpo)
head(supexpo)
options(digits = 3)

write.xlsx(supexpo, 'range_overlap_shift_SSP585.xlsx', row.names = FALSE)

#Summary

supexpo$type <- 'SSP585'
supexpo245 <- read.xlsx('range_overlap_shift_SSP245.xlsx', sheetIndex = 1)
supexpo245$type <- 'SSP245'

ncol(supexpo)
ncol(supexpo245)

colnames(supexpo) == colnames(supexpo245)
head(supexpo)
head(supexpo245)
colnames(supexpo245) <- colnames(supexpo)

bigsup <- rbind(supexpo, supexpo245)

head(bigsup)
require(ggplot2)
#

bigsup %>%  
  drop_na() %>% 
  ggplot(aes(x=pct_range_overlap_present,  y=pct_range_overlap_future, col=type)) + #label=species
  geom_point(size=3)+  geom_smooth(se = FALSE, method = lm)+
  theme_bw()+
  ggtitle( '' )+
  xlab('% present overlap'  )+
  scale_fill_manual(values = c("gold3", "firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
  scale_color_manual(values = c("gold3","firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
    ylab('% overlap in 2100'  )+ #geom_text(size=4, fontface = "italic") +
  labs(col='Scenario')+
  theme(plot.title.position = "plot",axis.title=element_text(size=15),
        legend.position="bottom", legend.direction="horizontal")
#
head(bigsup)

require(ggrepel)

figover <- bigsup %>%  
  drop_na() %>% 
  ggplot(aes(x=pct_range_overlap_present,  y=pct_range_overlap_future, label=sp2)) + #label=species
  geom_point(size=1)+  geom_smooth(se = FALSE, method = lm)+
  facet_wrap(~species*type)+
  theme_bw()+
  ggtitle( '' )+
  xlab('Estimated % present overlap'  )+
  #scale_fill_manual(values = c("gold3", "firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
  #scale_color_manual(values = c("gold3","firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
  ylab('Estimated % overlap in 2100'  )+ geom_text_repel(size=3, fontface = "italic") +
  labs(col='Scenario')+
  theme(plot.title.position = "plot",axis.title=element_text(size=15),
        legend.position="bottom", legend.direction="horizontal", strip.background = element_blank(), 
        strip.text = element_text(size = 10,  face = "italic"))

figover
# just ssp585

figover585 <- bigsup %>%  filter(type == 'SSP585') %>% 
  drop_na() %>% 
  ggplot(aes(x=pct_range_overlap_present,  y=pct_range_overlap_future, label=sp2)) + #label=species
  geom_point(size=1)+  geom_smooth(se = FALSE, method = lm)+
  facet_wrap(~species, ncol=3)+
  theme_bw()+
  ggtitle( '' )+
  ylim(-0.25, 1)+
  xlim(-0.25, 1)+
  xlab('Estimated % present overlap'  )+
  #scale_fill_manual(values = c("gold3", "firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
  #scale_color_manual(values = c("gold3","firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
  ylab('Estimated % overlap in 2100 (SSP585)'  )+ geom_text_repel(size=3, fontface = "italic") +
  theme(plot.title.position = "plot",axis.title=element_text(size=15),
        legend.position="bottom", legend.direction="horizontal", strip.background = element_blank(), 
        strip.text = element_text(size = 10,  face = "italic"))

figover585

figover245 <- bigsup %>%  filter(type == 'SSP245') %>% 
  drop_na() %>% 
  ggplot(aes(x=pct_range_overlap_present,  y=pct_range_overlap_future, label=sp2)) + #label=species
  geom_point(size=1)+  geom_smooth(se = FALSE, method = lm)+
  facet_wrap(~species, ncol=3)+
  theme_bw()+
  ggtitle( '' )+
  ylim(-0.25, 1)+
  xlim(-0.25, 1)+
  xlab('Estimated % present overlap'  )+
  #scale_fill_manual(values = c("gold3", "firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
  #scale_color_manual(values = c("gold3","firebrick3"), labels = c( "Future SSP245", "Future SSP585")) +
  ylab('Estimated % overlap in 2100 (SSP245)'  )+ geom_text_repel(size=3, fontface = "italic") +
  theme(plot.title.position = "plot",axis.title=element_text(size=15),
        legend.position="bottom", legend.direction="horizontal", strip.background = element_blank(), 
        strip.text = element_text(size = 10,  face = "italic"))

figover245

#gridExtra::grid.arrange(oheat, oheatfut, ncol = 1, nrow=2)

figbigsup <- paste0('Fig_overlap_pct_supplents_', version_suffix, '.jpg')
ggsave(filename = figbigsup, figover, width =33, height = 29, units = 'cm', dpi=600)

figbigsup5 <- paste0('Fig_overlap_pct585_sup_', version_suffix, '.jpg')
ggsave(filename = figbigsup5, figover585, width =20, height = 30, units = 'cm', dpi=600)

figbigsup2 <- paste0('Fig_overlap_pct245_sup_', version_suffix, '.jpg')
ggsave(filename = figbigsup2, figover245, width =20, height = 30, units = 'cm', dpi=600)

#exporting bigsup
head(bigsup)

write.xlsx(bigsup, 'Table_Supplementary_overlap_shift_LARGE.xlsx', row.names = FALSE)

# Summarise
skim(bigsup)

head(bigsup)

a5 <- bigsup %>%   filter(type=='SSP585') %>% 
  group_by(species) %>% 
  summarize(mean_pct_overlap_present = mean(pct_range_overlap_present), 
            mean_pct_overlap_future = mean(pct_range_overlap_future),
            median_pct_overlap_present = median(pct_range_overlap_present), 
            median_pct_overlap_future = median(pct_range_overlap_future),
            sd_pct_overlap_present = sd(pct_range_overlap_present), 
            sd_pct_overlap_future = sd(pct_range_overlap_future),
            min_pct_overlap_present = min(pct_range_overlap_present),
            min_pct_overlap_future585 = min(pct_range_overlap_future),
            max_pct_overlap_present = max(pct_range_overlap_present),
            max_pct_overlap_future585 = max(pct_range_overlap_future) )

a2 <- bigsup %>%   filter(type=='SSP245') %>% 
  group_by(species) %>% 
  summarize(
    #mean_pct_overlap_present = mean(pct_range_overlap_present), 
     #       mean_pct_overlap_future = mean(pct_range_overlap_future),
      #      median_pct_overlap_present = median(pct_range_overlap_present), 
       #     median_pct_overlap_future = median(pct_range_overlap_future),
        #    sd_pct_overlap_present = sd(pct_range_overlap_present), 
         #   sd_pct_overlap_future = sd(pct_range_overlap_future),
            #min_pct_overlap_present = min(pct_range_overlap_present),
            min_pct_overlap_future245 = min(pct_range_overlap_future),
            #max_pct_overlap_present = max(pct_range_overlap_present),
            max_pct_overlap_future245 = max(pct_range_overlap_future) )

a2
a5  
  
af <- left_join(a5,a2)
colnames(af)
af <- af %>%  relocate(min_pct_overlap_future245, .after= min_pct_overlap_present) %>% 
  relocate(max_pct_overlap_future245, .after=max_pct_overlap_present)

colnames(af)


options(digits=3)

write.xlsx(af, 'Table_supplementary_summary_overlap.xlsx')

#-------------------------------------------------------------------------------------------------------------