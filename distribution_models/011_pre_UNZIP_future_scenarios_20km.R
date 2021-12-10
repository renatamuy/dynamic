# Unzipping CMIP6 10 min == 18.5 km ~ 20km
# bc = 19 bioclimatic variables

require(stringr)
library(plyr)

getwd()

setwd('D://OneDrive - Massey University//_env//cmip6_20km//')

list.files()

for(f in unique(list.files())){print(f)
  setwd(  paste0('./', f)  )
  print(getwd())
  # list within
  list.files()
  # Unzip all this 
  ldply(.data = list.files(pattern="*.zip"), .fun = unzip, exdir = getwd(), junkpaths = TRUE)
  # return
  
  setwd('D://OneDrive - Massey University//_env//cmip6_20km//')
  
  }
##########################################################