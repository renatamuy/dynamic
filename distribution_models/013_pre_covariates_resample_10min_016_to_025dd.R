# Resample everything from 10 min (~0.167dd) to 0.25 dd 

# 0.25 dd

require(raster)

setwd('D://OneDrive - Massey University//_env//land_cover//LULC_raw_sdm//')

list.files()

primf <- raster::raster('CMIP6_Land_Use_Harmonization_primf_2015.tif')

#  Present 10 min to resample

setwd('D://OneDrive - Massey University//_env//_env_10min//')

s <- raster::stack(list.files())

plot(s)

all_27km <- resample(s, primf, method='bilinear')

names(all_27km)

setwd('D://OneDrive - Massey University//_env//')

dir.create('_env_27km')

setwd('_env_27km')

writeRaster(all_27km, filename=names(all_27km), bylayer=TRUE,format="GTiff")

# Scaled data

all_27kms <- scale(all_27km)

setwd('D://OneDrive - Massey University//_env//')

dir.create('_env_27km_scaled')

setwd('_env_27km_scaled')

writeRaster(all_27kms, filename=names(all_27kms), bylayer=TRUE,format="GTiff")

##############################################################################################################
# CMIP6 - takes 1.21 hours
##############################################################################################################

start <- Sys.time()

setwd('D://OneDrive - Massey University//_env//cmip6_20km//')

for(f in unique(list.files())){
  

  print(paste0('Starting with ', f))
  print('*')
  print('*')
  print('*')
  
  setwd(f)
  
  le <- length(list.dirs() ) 
  
  # Position one is for getting up
  
  dirs <- list.dirs()[ 2:le]
            
           for(d in dirs) {
    
    setwd(d)
      
    print(d)
          
    print(list.files())
    
    ###########################################################################################
    
    s <- raster::stack(list.files())
    
    all_27km <- resample(s, primf, method='bilinear')
    
    dn <- paste0(sub('./', '', d), '_27km') 
    
    dir.create(dn)
    
    setwd(dn)
    
    writeRaster(all_27km, filename=names(all_27km), bylayer=TRUE,format="GTiff")
    
    print('Resampled Raw Data data exported! Navigating up now!')
    
    setwd('../')
    
    # Scaled data
    
    all_27kms <- scale(all_27km)
    
    dns <- paste0(sub('./', '', d), '_27kms') 
    
    dir.create(dns)
    
    setwd(dns)
    
    # Exporting scaled data
    
    writeRaster(all_27kms, filename=names(all_27kms), bylayer=TRUE,format="GTiff")
    
    print(dns)
    
    print('Resampled AND scaled data exported! Navigating up now!')
    
    setwd('../')
    
    #################################################################################
    
    setwd('../')
    

           } 
  
  print(getwd())
  print('_')
  print('_')
  print('_')
  print(paste0('All done for ', f))
  setwd('../')
  }
    

print(Sys.time()- start )


#Time difference of 1.218524 hours

##############################################################################################################
# Rescaling LUH2 data in 5 seconds..
##############################################################################################################

start <- Sys.time()

setwd('D://OneDrive - Massey University//_env//land_cover//LULC_to_scale//')

for(f in unique(list.files())){
  
  
  print(paste0('Starting with ', f))
  print('*')
  print('*')
  print('*')
  
  setwd(f)
  

  # Position one is for getting up
  
  dirs <- list.files()#[ 2:le]
  
  for(d in dirs) {
    
    setwd(d)
    
    print(d)
    
    print(list.files())
    ####################################################
    s <- raster::raster(list.files())
    
    # Scaled data
    
    all_27kms <- scale(s)

    nome <- 'primf'
    dns <- paste0(sub('./', '', d), '_27kms') 
    
    dir.create(dns)
    
    setwd(dns)
    
    # Exporting scaled data
    
    writeRaster(all_27kms, filename=nome, bylayer=TRUE,format="GTiff")
    
    print(dns)
    
    print('Resampled AND scaled data exported! Navigating up now!')
    
    setwd('../')
    
    ######################################################
    setwd('../')
    
    
  } 
  
  print(getwd())
  print('_')
  print('_')
  print('_')
  print(paste0('All done for ', f))
  setwd('../')
}


print(Sys.time()- start )

#Time difference of 4.039588 secs
