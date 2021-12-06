# SDM ensembles performance

source('00_packages.R')

source('01_settings.R')

setwd(enmresultsdir)

aux <- read.table(file = 'Evaluation_table.txt', header = TRUE)  

points <- read.table(file = 'Number_Unique_Occurrences.txt', header = TRUE)  

# Input file


input <- read.table(file = 'Occurrences_Filtered.txt',  header = TRUE)

head(input)

psp <- input %>%
  group_by(.id) %>%
  tally()

psp$sp <- psp$.id

psp$splab <- sub('_', ' ', stringr::str_to_sentence(psp$sp))
aux$splab <- sub('_', ' ', stringr::str_to_sentence(aux$Sp))

aux

aux %>% filter(Algorithm == "WMEA") %>%  
  dplyr::select(splab, Algorithm, TSS, TSS_SD,Boyce , Boyce_SD )  %>% 
  arrange(desc(TSS) )

options(digits=3)

setwd(projdir)

#dir.create('performance')
setwd('performance')

require(dplyr)
require(Rcpp)
set.seed(42)

p <- aux %>% filter(Algorithm == "WMEA") %>%  
  dplyr::select(splab, Algorithm, TSS, TSS_SD, TSS_SD, Boyce , Boyce_SD )  %>% 
  arrange(desc(TSS) )
p 
head(psp)
ppsp <- merge(psp, p, by=c('splab'))

po <- ppsp %>% arrange(desc(TSS) )

po <- po[!po$splab == 'Vespertilio sinensis',]

chat3=(deviance(mpo) / df.residual(mpo))

# Merge with points
head(points)

points$splab <- sub('_', ' ', stringr::str_to_sentence(points$Species))

write.xlsx(po,'performance.xlsx', row.names = FALSE)

plo <- ggplot(po, aes(n, TSS, label = splab)) +
  geom_point(color = "black", alpha=0) +geom_text( ) + labs(title = "", x='Number of locations') + theme_bw()
plo

ftss <- paste0('Fig_tss_', version_suffix, '.jpg')

ggsave(filename = ftss, plo, width =32, height = 22, units = 'cm', dpi=600)

############################## IUCN intersected
# Compare to IUCN intersected data
#
##
##
####

setwd(enmresultsdiri)

auxi <- read.table(file = 'Evaluation_table.txt', header = TRUE)  

# Non cleaned
pointsi <- read.table(file = 'Number_Unique_Occurrences.txt', header = TRUE)  
head(pointsi)
pointsi$splab <- sub('_', ' ', stringr::str_to_sentence(pointsi$Species))

# Input file

inputi <- read.table(file = 'Occurrences_Filtered.txt',  header = TRUE)

psp <- inputi %>%
  group_by(.id) %>%
  tally()

psp$sp <- psp$.id

psp$splab <- sub('_', ' ', stringr::str_to_sentence(psp$sp))
auxi$splab <- sub('_', ' ', stringr::str_to_sentence(auxi$Sp))


auxi %>% filter(Algorithm == "WMEA") %>%  
  dplyr::select(splab, Algorithm, TSS, TSS_SD,Boyce , Boyce_SD )  %>% 
  arrange(desc(TSS) )

options(digits=3)
setwd(projdir)
setwd('performance')

pi <- auxi %>% filter(Algorithm == "WMEA") %>%  
  dplyr::select(splab, Algorithm, TSS, TSS_SD, TSS_SD, Boyce , Boyce_SD )  %>% 
  arrange(desc(TSS) )


ppspi <- merge(psp, pi, by=c('splab'))

poi <- ppspi %>% arrange(desc(TSS) )

poi <- poi[!poi$splab == 'Vespertilio sinensis',]

# Export

write.xlsx(poi,'performancei.xlsx', row.names = FALSE)

ploi <- ggplot(poi, aes(n, TSS, label = splab)) +
  geom_point(color = "black", alpha=0) +geom_text( ) + labs(title = "", x='Number of locations') + theme_bw()
ploi

ftss <- paste0('Fig_tss_i', version_suffix, '.jpg')

ggsave(filename = ftss, ploi, width =32, height = 22, units = 'cm', dpi=600)

# Final table for article

#po and poi

po$n

poi$n

#Check

po$.id <- NULL
poi$.id <- NULL

colnames(poi) <- paste0(colnames(poi) , '_i')
colnames(poi) 
poi$splab <- poi$splab_i
poi$splab_i<- NULL

popoi <- left_join(po, poi, by=c('splab'))

head(popoi)

popoi$pct_tss_improved_after_i <-  100*( popoi$TSS_i / popoi$TSS) -100

write.xlsx(popoi,'Table_S4_performance.xlsx', row.names = FALSE)

