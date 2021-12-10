# Script unstack climate change bioclimatic variables for all scenarios 20 km == 10 minutes

require(raster)
require(stringr)
library(plyr)

##########################################################
# Script unstack

setwd('D://OneDrive - Massey University//_env//cmip6_20km//')

start <- Sys.time()

for(f in unique(list.files())){print(f)
  
  setwd(  paste0('./', f)  )
  print(getwd())
  
  # list within
  
  for(s in unique(list.files(pattern="*.tif")))  { print(s)
  
  r <- stack(s)
  
  names(r) <- paste0('bio_', 1:19)
  
  foldernamea <- sub('.tif','', s)
  
  foldernameb <- sub('wc2.1_10m_bioc_','', foldernamea)
  
  dir.create(foldernameb)
  
  setwd(paste0(foldernameb, '/') )
  
  print(paste0('Exporting rasters to ', foldernameb) )
  
  for(i in unique(names(r))    ){
    
    filename <- paste0(names(r[[i]]), '.tif')
    
    print(filename)

    writeRaster(r[[i]], filename = filename, format='GTiff')
    
    #format="ascii",overwrite=TRUE
    
  }
  
  print(paste0('Unique bioclim rasters exported for the raster stack ', s, '. Now back to main folder ' , f ) )
  print('*')
  print('*')
  print('*')
  
  setwd(paste0( 'D://OneDrive - Massey University//_env//cmip6_20km//', f)) 
   
    }
  # return
  
  print(paste0('All done for the entire folder ', f))
  print('_')
  print('_')
  print('_')
  
  setwd('D://OneDrive - Massey University//_env//cmip6_20km//')
 
}

print(Sys.time()- start )
# Time difference of 46.49677 mins
####################################

