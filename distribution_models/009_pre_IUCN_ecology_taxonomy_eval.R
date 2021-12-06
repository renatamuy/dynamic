# Eda sarbecobats habitat
# 18/05/2021
# R version 4.0.3 (2020-10-10)
# Ecology and Taxonomy of Sarbecovirus bat hosts 
# IUCN assessment downloaded in April 2021
# Highlights of this code: gghighlight, args(plotweb) and just neat code

source('00_packages.R')

source('01_settings.R')

setwd(projdir)

setwd('IUCN_assessment_list_chiroptera')

d <- read.xlsx('habitat_chiroptera.xlsx', sheetIndex = 1)

unique(d$name)

setwd(projdir)
setwd('dynamic_master/')

master <- read.delim("master.txt", header = TRUE)
v <- master$Master_sp_list

h <- d[d$name %in% v,]

nrow(h)

h$result.habitat

hd <- data.frame(table(h$result.habitat, h$name))
head(hd)
hd$Freq

unique(hd$Var1)
hd$Var <- substr(hd$Var1, 1, 9)
  unique(hd$Var)

  str(hd)
  hd$sp <- as.character(hd$Var2)
  #Then turn it back into a factor with the levels in the correct order
  hd$sp <- factor(hd$sp, levels=unique(hd$sp))
  
  
  hd$f <- ifelse(hd$Freq >0, 1,0)  

  hd$Habitat <- sub('Caves and','Caves', hd$Var)
  hd$Habitat <- sub('Rocky are','Rocky areas', hd$Habitat)
  hd$Habitat <- sub('Desert - ','Desert', hd$Habitat)
  hd$Habitat <- sub('Forest - ','Forest', hd$Habitat)
  hd$Habitat <- sub('Savanna -','Savanna', hd$Habitat)
  
  
  fig_habi <- ggplot(data=hd, aes(x=Habitat, size = Freq,  y=Var2)) +
       geom_point(color='darkgreen') + theme_bw() +
     scale_x_discrete(guide = guide_axis(angle =0)) +xlab('Habitat') + ylab('Species')+
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16,face="bold"), axis.text.y = element_text(size=16,face="italic") ) +
    gghighlight(Freq > 0) + theme(legend.position = 'none')
  
  fig_habi
  
  fig_habname <- paste0('Fig_habitats_', version_suffix, '.jpg')
  
  setwd(projdir)
  
  ggsave(filename = fig_habname, width =33, height = 36, units = 'cm', dpi=600, fig_habi)
  

  ggplot(data=hd, aes(x=Var1, size = Freq,  y=Var2)) + geom_point(color='darkgreen') + theme_bw()+
   scale_x_discrete(guide = guide_axis(angle =45)) + xlab('Habitat') + ylab('Species')+
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=16,face="bold"))+
        gghighlight(Freq > 0) + theme(legend.position = 'none')
  
  
table(h$result.suitability, h$name)


unique(h[h$result.suitability=='Marginal', 'name']) # Not that sensitive species

h[h$result.suitability == 'Marginal', 'result.habitat']

unique(h[h$result.suitability=='Unknown', 'name']) # Not that sensitive species

# Threat level
setwd('D://OneDrive - Massey University//PD_2021//IUCN_ecology//IUCN_assessment_list_chiroptera')
dhistory <- read.xlsx('history_chiroptera.xlsx', sheetIndex = 1)

hh <- dhistory[dhistory$name %in% v,]

hht <- data.frame(table(hh$result.code, hh$name))

head(hht)

# Which bat used more habitats?

ggplot(data=hht, aes(x=Var1, size = Freq,  y=Var2)) + geom_point(color='firebrick3') + theme_bw()+
  scale_x_discrete(guide = guide_axis(angle =45)) + xlab('IUCN category') + ylab('Species')+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  gghighlight(Freq > 0 & Var1 == 'EN' | Var1 == 'NT' | Var1 == 'VU' ) 

list.files()

#######

dnar <- read.xlsx('narrative_chiroptera.xlsx', sheetIndex = 1)

colnames(dnar)

dn <- dnar[dnar$name %in% v,]

write.xlsx(dn, 'narrative_read.xlsx', sheetName = 'sarbecoIUCN' )

dnp <-  data.frame(table(dn$result.populationtrend, dn$name))

head(dnp)

#dnp[dnp$Freq == 0, 'Freq'] <- NA

ggplot(data=dnp, aes(x=Var1, size = Freq,  y=Var2)) + geom_point(color='firebrick4') + theme_bw()+
  scale_x_discrete(guide = guide_axis(angle = 45)) + xlab('Population trend') + ylab('Species')+
  theme(axis.text=element_text(size = 20),
        axis.title=element_text(size = 20,face="bold"))+
  gghighlight(Freq > 0 & Var1 == 'decreasing') # Amazing!
 
# Read and think
list.files()

dc <- read.xlsx('country_chiroptera.xlsx', sheetIndex = 1)

dcc <- dc[dc$name %in% v,]


args(plotweb)

visweb(table(dcc$result.country, dcc$name))
plotweb(table(dcc$result.country, dcc$name),
        col.high = c("firebrick3"),
        col.low = 'black',
        high.lab.dis = 0.01,
        abuns.type='additional',
        text.rot = 90,
        arrow="up.center",
        ybig=1.2,
        y.width.high = .06,
        high.spacing = 0.011, 
        y.lim = c(-1,2),
        labsize = 1.7)



# Countries where they occur
g <- graph_from_incidence_matrix(table(dcc$result.country, dcc$name))

figloc <- 'D://OneDrive - Massey University//PD_2021//manuscript_sarbecovirus_hosts//Supplements//'
setwd(figloc)
tiff(filename = 'bat_country_network.tif', 
     res=300, units = 'cm', width=30, height = 30)
plot(g, layout = layout_with_kk, #layout_in_circle, layout_nicely, layout_on_sphere, layout_in_circle, layout_with_fr
     vertex.shape = "circle",
     vertex.label.dist=0.1,
     vertex.label.family= "Arial", vertex.label.cex= .9,
     vertex.label.color= "black", 
     edge.width= 2,
     vertex.size= 8, 
     vertex.color=c("lightpink","khaki1")[V(g)$type+1],
     edge.curved=0.3)
dev.off()

# Generate taxonomic table 


c_ncbi <- classification(v, db='ncbi')

c_ncbi[[1]][23:28,]


df <- data.frame(matrix(unlist(c_ncbi), nrow=length(c_ncbi), byrow=TRUE))


c_eol <- classification(v, db='eol') #shows a lot of stuff, including wikimedia commons, ITIS and Globi, 
#and subspecies!

#Chaerephon plicatus

#ncbi, itis, eol, tropicos, gbif, nbn, worms, natserv, bold, wiki, or pow

# what is a infraspecies?



