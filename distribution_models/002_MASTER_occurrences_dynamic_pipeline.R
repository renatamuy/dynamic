##########################################################################
# Binding script
##########################################################################

# prepare R --------------------------------------------------------------------------------------------------

setwd(here::here())

source('00_packages.R')

# My dynamic large file to update

setwd('dynamic_copies')

list.files()

# This file always changes to the most updated master data, which is already CC, naturally

file_all <- read.xlsx("master_bat_data_2021_09_D15.xlsx", sheetIndex = 1)

head(file_all)

table(file_all$base)

# Adding filtered missing occurrences

ruber <- read.csv('occurrences_missing_sarbecobats_spocc_2021_cc.csv')

colnames(file_all)
colnames(ruber)

ruberb <- ruber[,-c(9:18)]
colnames(ruberb)
ruberb$sp <- NULL

ruberbb <- ruberb %>% relocate(date, .before = base )

colnames(ruberbb) <-c("sp", "long"  , "lat"  ,  "date" ,  "base" ,  "key"   , "year" ) 
colnames(ruberbb)

setdiff(colnames(file_all), colnames(ruberbb))

colnames(file_all)


head(ruberbb)

ruberbb$splab <- sub('_', ' ', stringr::str_to_sentence(ruberbb$sp))

file_all <- file_all %>% relocate(lat, .before = date )

head(file_all)
head(ruberbb)

# BINDING!!!! Feed the object file_all

binded <- rbind(file_all, ruberbb)

nrow(binded)
length(unique(binded$sp))


# Raw completeness

gsarbecobats <- ggplot(data=binded) + 
  geom_bar(mapping = aes(x=forcats::fct_infreq(splab), fill = base),
           width = 0.9, position = position_dodge())+  scale_fill_viridis_d()+  
  theme_bw() +
  theme(        axis.text.y = element_text( face="italic", angle=0
                                            colour="black", size=rel(1)))+
  ylab("Number of unique locations after cc")+xlab("Species")+
  coord_flip() 


gsarbecobats

ggsave("gsarbecobats_completeness.png", gsarbecobats)


gs <- ggplot(data=binded) + 
  geom_bar(mapping = aes(x=forcats::fct_infreq(splab)),
           width = 0.9, position = position_dodge())+
  theme_bw() +
  theme(        axis.text.y = element_text( face="italic", angle=0,
                                   vjust=0.0,
                                   hjust = 0.0,
                                   colour="black", size=rel(1)))+
  ylab("Number of unique locations after cc")+xlab("Species")+
  coord_flip() 

gs

ggsave("gsarbecobats_completeness_bw.png", gs)


# Exporting data merged for inspection

setwd('../dynamic_master/')
datadate <- readline(prompt="Enter date for version: ")
prefixo <- "master_bat_data_"
write.xlsx(binded, paste0(prefixo, datadate,'.xlsx'), row.names= FALSE)
data.table::fwrite(binded, paste0(prefixo, datadate,'.csv'), row.names = FALSE)

##############################################################################################################

