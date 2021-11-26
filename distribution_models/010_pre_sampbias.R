#########################
#https://rdrr.io/github/azizka/sampbias/man/calculate_bias.html
require("devtools")
#install_github("azizka/sampbias")
library(sampbias)
require(dplyr)
require(sampbias)

source('00_packages.R')
source('01_settings.R')

setwd(dynmaster)

#"D:/OneDrive - Massey University/_env/sdm_inputs/CC_master_bat_data_2021_09_D16.txt"

#f2 <- read.table( "D:/OneDrive - Massey University/_env/sdm_inputs/CC_master_bat_data_2021_09_D16.txt", header = TRUE)

f2 <-  read.table('D:/OneDrive - Massey University/_env/results_40o_ss6_maxent_15rep/Occurrences_Filtered.txt', header = TRUE)

head(f2)
f2$long <- f2$x
f2$lat <- f2$y

table(f2$.id)

#f2$sp <- c('sarbecovirus_hosts')

f2$sp <- f2$.id

occ  = f2 %>%
  mutate( decimalLongitude = long )%>%
  mutate(decimalLatitude = lat )

head(occ)

occ$decimalLongitude <- as.numeric(occ$decimalLongitude)
occ$decimalLatitude <- as.numeric(occ$decimalLatitude)

setwd(projdir)
dir.create('sampbias_025dd_species/')
setwd('sampbias_025dd_species/')

#Results might change with increas- ing resolution, since roads and rivers might have a stronger effect on higher resolutions (facilitating most the access to their immediate vicinity), whereas cities and airports might have a stronger effect on the larger scale (facilitating access to a larger area)

for(s in unique(occ$sp))
{
  
  occs <- occ[ occ$sp %in% s ,] 
  print(s)
  if(nrow(occs) > 40) {
    
    bias_hosts <- calculate_bias(x = occs, 
                                 res = 0.25,
                                 verbose = TRUE, 
                                 buffer=2,
                                 terrestrial = TRUE,
                                 mcmc_burnin=5000,
                                 mcmc_iterations=50000)
    
    
    # Saving files
    
    dir.create(s)
    
    setwd(s)
    
    filenamer <- paste0("bias_", s, ".RData")
    
    save(bias_hosts, file = filenamer)
    
    filename <- paste0("bias_", s, ".xlsx")
    xlsx::write.xlsx(summary(bias_hosts), file = filename)
    
    
    bias_hosts_proj <- project_bias(bias_hosts)
    #order changes! so just export the stack
    crs(bias_hosts_proj) <- CRS('+init=EPSG:4326')
   
     new_names <- names(bias_hosts_proj)
   
    writeRaster(bias_hosts_proj, filename=paste0(new_names, "_", s), bylayer=TRUE,format="GTiff")
    
    filenamem <- paste0("map_bias_", s, ".png")
    
    png(filename= filenamem, res=400, width = 28, height = 20, units = "cm")
    map_bias(bias_hosts_proj)
    dev.off()
    
    filenamep <- paste0("fig_bias_", s, ".png")
    
    png(filename= filenamep, res=300, width = 20, height = 15, units = "cm")
    plot(bias_hosts)
    dev.off()
    setwd('../')
    }
  else{ print('Low sample size for bias calc')}
}

