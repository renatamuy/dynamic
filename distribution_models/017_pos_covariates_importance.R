##############################################################################################################
# Relative importance plot
##############################################################################################################

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


#tiff(filename=figvar, 
 #    res=400, width = 32, height = 40, units = "cm" )

aux %>% 
  ggplot() + aes(Algorithm, Importance, fill = Covariate2, label = paste0(round(Importance, 2)*100, "%"), alpha=0.91) + 
  geom_col() +
  facet_wrap( . ~splab, ncol = 3) +
  geom_text(position=position_stack(0.5)) + theme(strip.text= element_text(face = "italic", size = 14),
                                                 axis.title=element_text(size = 14),
                                                 axis.text=element_text(size = 10) ,
                                                 axis.text.x = element_text(angle=0,
                                                                            vjust=0.01,
                                                                            hjust = 0)) +scale_alpha(guide = 'none')+
  scale_fill_viridis(discrete = TRUE, option = "D")


dev.off()

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

#tiff(filename = paste0('Fig_stacked_varimp_', version_suffix, '.tif'), 
#     res=400, width = 30, height = 38, units = "cm" )

auxag1 %>% 
  ggplot() + aes(Algorithm, Importance, fill = Covariate, label = paste0(round(Importance, 2)*100, "%"), alpha=0.9) + 
  geom_col() +
  facet_wrap( . ~splab, ncol = 3) +
  geom_text(position=position_stack(0.5)) + 
  theme(   strip.text= element_text(face = "italic", size = 13),
                                                  axis.title=element_text(size = 14),
                                                  axis.text=element_text(size = 10) ,
                                                  legend.text=element_text(size=10),
                                                  axis.text.x = element_text(angle=0,
                                                                             vjust=0.01,
                                                                             hjust = 0), legend.position="right") +
  xlab('Algorithm') + ylab('Average importance') +
  scale_fill_viridis(discrete = TRUE, option = "D")+ scale_alpha(guide = 'none')

dev.off()

# Importance of covariates by species

auxag1 %>% 
  ggplot() + aes(Algorithm, Importance, fill = splab, label = paste0(round(Importance, 2)*100, "%"), alpha=0.9) + 
  geom_col() +
  facet_wrap( . ~Covariate, ncol = 3) +
  geom_text(position=position_stack(0.5)) + 
  theme(   strip.text= element_text(face = "italic", size = 13),
           axis.title=element_text(size = 14),
           axis.text=element_text(size = 10) ,
           legend.text=element_text(size=10),
           axis.text.x = element_text(angle=0,
                                      vjust=0.01,
                                      hjust = 0), legend.position="right") +
  xlab('Algorithm') + ylab('Average importance') +
  scale_fill_viridis(discrete = TRUE, option = "D")+ scale_alpha(guide = 'none') + theme_bw()


# Species on the y reorder(splab, Importance)
auxag1 %>%   ggplot() + aes(y=splab, x=Importance, fill=Covariate, label = paste0(round(Importance, 2)*100, "%"), alpha=0.9) + 
  geom_col() +
  facet_wrap( . ~Algorithm) +
  geom_text(position=position_stack(0.1)) + 
  xlab('Algorithm') + ylab('Average importance') +
  scale_fill_viridis(discrete = TRUE, option = "D")+ scale_alpha(guide = 'none') + theme_bw()+
  theme(   strip.text= element_text(face = "bold", size = 13),
         axis.text.y = element_text(face = "italic", size = 13.5),
         axis.title=element_text(size = 14),
         legend.text=element_text(size=10), legend.position="right")

#


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
figrankname <- paste0('Figure 1_varimp_', version_suffix, '.jpg')


figrank <- ord %>% arrange(Importance) %>% 
ggplot(aes(y = splab, x=Covariate, fill =rank, label = paste0(round(Importance, 2)*100, "%") ) ) +
  facet_wrap( . ~Algorithm)+
  geom_tile(alpha=0.75) + scale_fill_viridis( option = "E", direction=-1) + geom_text(size=4, fontface = "bold") + theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=10, face = "italic"), axis.text.x = element_text(angle = 45, hjust = 0.99, size=11))+ ylab('Species') 

figrank

#ggsave(filename = figrankname, figrank, width =26, height = 30, units = 'cm', dpi=600)


# Averaging by algorithm ensemble
figranknameav <- paste0('Figure 1_varimp_AVERAGE2', version_suffix, '.jpg')

# Aggregate by 3 grouping vars

ord2 <- aggregate(Importance ~ splab+Covariate, data = ord, FUN = mean, na.rm = FALSE) 
ord2
ord3 <- ord2 %>%  arrange(splab, desc(Importance))
ord3
nrow(ord3)
ord3$rank <- rep(c(1,2,3,4,5,6), nrow(ord3) /6 )
ord3$rank

figranka <- ord3 %>%
  arrange(Importance) %>% 
  ggplot(aes(y = splab, x=Covariate, fill =rank, label = paste0(round(Importance, 2)*100, "%") )) +
  geom_tile(alpha=0.78) + scale_fill_viridis( option = "E", direction=-1) + geom_text(size=4, fontface = "bold") + theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=10, face = "italic"), axis.text.x = element_text(angle = 45, hjust = 0.99, size=11))+ ylab('Species') 

figranka

ggsave(filename = figranknameav, figranka, width =21, height = 30, units = 'cm', dpi=600,)


wantsp <- c('Rhinophus affinis', 'Rhinolophus rex')
#filter(splab == wantsp)
aux %>% filter(Covariate == want) %>% ggplot(aes(x=Importance, y=splab, colour=Algorithm))+
  geom_point(alpha = 0.9,size = 3, stroke = 1) +  facet_wrap( . ~forcats::fct_infreq(Covariate) ) + scale_color_viridis(discrete = TRUE, option = "D")+ theme_bw() 


# Importance per algorithm no matter species
aux %>% filter(Covariate == want) %>% ggplot(aes(y=Importance, x=Algorithm, colour=Covariate)) +
  geom_point(alpha = 0.9,size = 3, stroke = 1) +   scale_color_viridis(discrete = TRUE, option = "D")+ theme_bw() 

aux %>% filter(Covariate == want) %>% ggplot(aes(y=Importance, x=Covariate, colour=Algorithm)) +
  geom_point( alpha = 0.9,size = 3, stroke = 1)+   scale_color_viridis(discrete = TRUE, option = "A")+ theme_bw() 

# Violin plot

# Anova highlighting pairwise comparison with a certain reference group
aux %>% filter(Covariate == want) %>% ggplot(aes(y=Importance, x=Covariate)) +
  geom_violin( alpha = 0.9,size = 1) +
  stat_compare_means(label = "p.signif",ref.group = "bio_4")+
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="steelblue1") + theme_bw()

#  stat_summary(fun=mean, geom="point", shape=23, size=2, color="black")+
# Per sp

getwd()

####################################################################################################
# Geom_violin with order of importance from most to least important order and anova
# Reference group displayed is temperature seasonality (bio 4) as it was the most importante variable
# Apparently the importance is super spread among covariates (because algorithms vary a lot)
####################################################################################################

# Make better labels for covariates!

setwd('varimp')

figanova <- paste0('Fig_varimp_anova',version_suffix, '.tif' )

aux <- aux[!aux$splab == 'Vespertilio sinensis',]

# only useful if you use many algorithms

#tiff(filename =figanova, 
#     res=400, width = 34, height = 40, units = "cm" )


aux %>% 
  mutate(Covariate = fct_reorder(Covariate, desc(Importance) )) %>%  # x first always
  filter(Covariate == want) %>% ggplot(aes(y=Importance, x=Covariate)) +
  geom_violin( alpha = 0.9,size = 1) +
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="#37476b")+
  facet_wrap( . ~splab, ncol = 4) +
  stat_compare_means(label = "p.signif",ref.group = "bio_4")+
  geom_hline(yintercept = mean(aux$Importance), linetype = 2)+ 
   theme_bw()+ theme(strip.text = element_text(size=16, face = "italic"),
                     axis.text.y = element_text(size=16),
                     axis.text.x = element_text(size=16, angle=90,
                                                vjust=0.00,
                                                hjust = 0),
                     axis.title = element_text(size=16))
dev.off()

############################

# Importance highlighted per algorithm
aux %>% filter(Covariate == want) %>% ggplot(aes(x=Importance, y=Algorithm, colour=splab))+
  geom_point(alpha = 0.8,size = 3, stroke = 1) +  facet_wrap( . ~forcats::fct_infreq(Covariate) ) +
  scale_color_viridis(discrete = TRUE, option = "D")+ theme_bw() 


# Importance highlighted per species
auxs <- aux %>% group_by(sp, Covariate ) %>% 
  summarise(mean = mean(Importance), n = n()    )

maximp <- aux %>% 
group_by(sp, Covariate ) %>% 
summarise(Value = max(Importance))

#figalgcor<- paste0('Fig_varimp_algorithm_convergence',version_suffix, '.tif' )


#tiff(filename =figalgcor,  res=400, width = 34, height = 40, units = "cm" )


aux %>% filter(Covariate == want) %>% ggplot(aes(y=Importance, x=Covariate, colour=Algorithm))+
  geom_jitter(width = 0.1, height = 0, alpha = 0.8,size = 3, stroke = 1) +   facet_wrap( . ~splab ) +
  scale_color_viridis(discrete = TRUE, option = "D")+ theme_bw() +theme(strip.text= element_text(face = "italic", size = 14),
                                                                        axis.title=element_text(size = 14),
                                                                        axis.text=element_text(size = 10) ,
                                                                        axis.text.x = element_text(angle=90,
                                                                                                  vjust=0.01,
                                                                                                 hjust = 0)) 

dev.off()

# 

inp <- ord3
inp$rank <- NULL

dwide <- pivot_wider(inp, id_cols = splab, names_from= Covariate,values_from = Importance)

nrow(dwide)
colnames(dwide)

library(factoextra)


justc <- as.data.frame(dwide[,2:7])
row.names(justc) <- dwide$splab

row.names(dwide)
nrow(dwide)
res.pca <- prcomp(justc, scale = TRUE)

fviz_eig(res.pca)

head(dwide)

# habillage color by trait

figpca<- paste0('Fig_pca_varimp_',version_suffix, '.jpg' )

figbiplot <- fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "steelblue", # Variables color
                col.ind = "black" ,  label = "all", labelsize = 5,
                pointsize = 4,
                col.quanti.sup = "blue",title='A. PCA - Biplot')

jpeg(filename =figpca,  res=400, width = 28, height = 28, units = "cm" )

figbiplot

dev.off()

dist_mat <- dist(dwide, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = "ward.D2")
d1 <- as.dendrogram(hclust_avg)


jpeg(filename =figclust,  res=400, width = 28, height = 28, units = "cm" )
par(mar = c(2,2,2,10))
d1 %>% plot(main="Similarity in variable importance (euclidean distance, hierarchichal clustering )", horiz = TRUE, cex=2)
dev.off()

library("ggdendro")

hclustgg <- ggdendrogram(d1, rotate = TRUE, theme_dendro = FALSE)+
  ggtitle("B. Similarity in variable importance")+
  xlab('Species') + ylab('Euclidean distance')+
  theme_bw()+
  theme(axis.text.y= element_text(face = "italic", size = 16),
        axis.text.x= element_text( size = 16),
        title = element_text( size = 16)) 

hclustgg

figclust<- paste0('Fig_clust_varimp_',version_suffix, '.jpg' )
ggsave(filename = figclust,width =24, height = 37, units = 'cm', dpi=600,
       gridExtra::grid.arrange(figbiplot,hclustgg, nrow=2))


