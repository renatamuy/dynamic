#' ----
#' Contents
#' relative importance plot
#' ----

# prepare r ---------------------------------------------------------------

setwd(here::here())

source('00_packages.R')

source('01_settings.R')

#dir.create('varimp')

figvar <- paste0('Fig_varimp',version_suffix, '.tif' )

setwd(algo) # remove version object from setting as it conflicts with R version syntax
  
aux <- data.frame()

for(f in list.files() ){

  setwd(paste0(f,'/Response Curves & Variable Importance/' ))
  a <- read.table(file = 'VariableImportance.txt', skip=1)  
  aux <- rbind(aux, a)
  
  print('opened')
  setwd(algo)
  
}

table(aux$V3)
colnames(aux)

colnames(aux) <- c('varid', 'sp', 'Algorithm', 'Covariate', 'Importance')

table(aux$Algorithm)

require(tidyverse)
unique(aux$Covariate)

# Setting glm effects
aux$Covariate2 <- ifelse(aux$Covariate == "I(bio_1^2)", 'bio_1', ifelse(aux$Covariate =="I(bio_12^2)" , "bio_12" ,
                                                      ifelse(aux$Covariate == "I(bio_15^2)" , 'bio_15',
                                                             ifelse(aux$Covariate== "I(bio_17^2)" , 'bio_17',
                                                                    ifelse(aux$Covariate=="I(bio_4^2)" , 'bio_4',
                                                                           ifelse(aux$Covariate=="I(karstm_10min^2)", 'karstm_10min',
                                                                                  ifelse(aux$Covariate=="I(treecover_10min^2)", 'treecover_10min', aux$Covariate)
                                                                                  ))))))
       

#"I(bio_1^2)"           "I(bio_12^2)"          "I(bio_15^2)"          "I(bio_17^2)"          "I(bio_4^2)"           "I(karstm_10min^2)"    "I(treecover_10min^2)"

library("plyr")


aux$splab <- sub('_', ' ', stringr::str_to_sentence(aux$sp))
head(aux)

# Only if you used glm
auxag <- aggregate(aux[,c('Importance')], by=list(aux$Algorithm, aux$Covariate2, aux$splab), "sum")


head(auxag)

want <- unique(aux$Covariate)
want

#t <- read.table('D://OneDrive - Massey University//_env//bat_ensembles_OBR_nothin//Algorithm//BIO//Response Curves & Variable Importance//')

##############################################################################################################
# Stacked plot including GLM quadratic terms
##############################################################################################################

setwd(projdir)
setwd('varimp')


##############################################################################################################
# Correcting after aggregating Importances of GLM
head(auxag)
colnames(auxag) <- c('Algorithm', 'Covariate', 'splab', 'Importance')

auxag <- auxag[!auxag$splab == 'Vespertilio sinensis',]

##############################################################################################################
unique(auxag$Covariate)


auxag1 <- auxag %>% mutate(Covariate = fct_recode(Covariate,
                                                "Annual mean temperature"    =   "bio_1",
                                                "Annual precipitation"    =   "bio_12",
                                                "Precipitation seasonality"    =   "bio_15",
                                                "Temperature seasonality"    =   "bio_4",
                                                "Distance to karst or cave"    =   "karstm",
                                                "% Primary forest"  =   "primf") )
                                                 
head(auxag)


ord <- auxag1 %>%
arrange(splab, desc(Importance))

204/12

ra <- rep(c(1,1,2,2,3,3,4,4,5,5,6,6), nrow(ord) / (2*length(unique(ord$Covariate)))  )

ra

length(ra)

ord$rank <- ra
table(ord$splab)
library(wesanderson)
pald <- wes_palette("Zissou1", 100, type = "continuous")

# Final figure 
setwd(projdir)
setwd('varimp')
figrankname <- paste0('Figure_varimp', version_suffix, '.jpg')

# Averaging by algorithm ensemble
figranknameav <- paste0('Figure 1_family_', version_suffix, '.jpg')

# Aggregate by 3 grouping vars

ord2 <- aggregate(Importance ~ splab+Covariate, data = ord, FUN = mean, na.rm = FALSE) 
ord2
ord3 <- ord2 %>%  arrange(splab, desc(Importance))
ord3
nrow(ord3)
ord3$rank <- rep(c(1,2,3,4,5,6), nrow(ord3) /6 )
ord3$rank

head(ord3) 
unique(ord3$splab)

require(tidyverse)
require(janitor)
ord4 <- ord3 %>%  mutate(family = fct_recode(splab,
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
#%>%   tabyl(family)

#aes(colour=df2$Count))
require(RColorBrewer)
ord4$splabf <- as.factor(ord4$splab)

levels(ord4$splabf)

ord4 %>% arrange(family) 

ord4$compo <-paste(ord4$family,paste0('-'), ord4$splabf)
figranka <- ord4 %>%
  ggplot(aes(y = compo, x=Covariate, fill=rank, label = paste0(round(Importance, 2)*100, "%") )) +
  geom_tile(alpha=0.78) +
  scale_fill_viridis( option = "E", direction=-1) + 
  geom_text(size=4, fontface = "bold") +
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic") , 
        axis.text.x = element_text(angle = 30, hjust = 0.99, size=14))+
  ylab('') +ggtitle('')+xlab('')

figranka

setwd(here::here())
setwd('varimp')
ggsave(filename = figranknameav, figranka, width =23, height = 27, units = 'cm', dpi=600,)

#-------------------------------------------------------------------------------------------------------------