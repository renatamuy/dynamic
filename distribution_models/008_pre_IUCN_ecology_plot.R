##############################################################################################################
# Beta cov hosts ecology 
# API download from IUCN database in 2021
##############################################################################################################



source('00_packages.R')

source('01_settings.R')

setwd('dynamic_master/')

list.files()

b <- read.delim("master.txt", header = TRUE)

head(d)

# get eco info

setwd("D://OneDrive - Massey University//PD_2021//IUCN_ecology//")

k = 'f3036aa948ec81107d8c9146d582eade02d419e238644a1c6dc08061da005062'

aux_hab <- data.frame()
aux_narrative <- data.frame()
aux_history<- data.frame()
aux_country<- data.frame()

options(show.error.messages = FALSE)

for(i in 1:length(b)  ) {
  print(paste0("running ", b[i,1]))
  # test
  testing <-  try(data.frame(rl_habitats(b[i,1], key = k ))[,1:4] )
  if( class(testing) == 'try-error'){  
    i <- i+1  }
  print("tested")
  c_hab <-  data.frame(rl_habitats(b[i,1], key = k ))[,1:4] 
  # Narrative: taxonomic notes, rationale, geo range, pop, habitat, threats, conservation
  c_narrative <- data.frame(rl_narrative(b[i,1], key = k))[,1:11] 
  c_history <- data.frame(rl_history(b[i], key = k) )[,1:3] # category per year
  c_country <- data.frame(rl_occ_country(b[i,1], key = k))
  aux_hab <- rbind(aux_hab, c_hab )
  aux_narrative <- rbind(aux_narrative, c_narrative)
  aux_history <- rbind(aux_history, c_history)
  aux_country <- rbind(aux_country, c_country)
}

c_citation <- rl_citation(key=k)
# Set name for your search

setwd(projdir)
mysearch <- "_list_sarbecobats_2021"
namedir <- paste0("./IUCN_assessment", mysearch )

if (!dir.exists(namedir)){
  dir.create(namedir)
} else {
  print("Directory already exists!")
}

setwd(namedir)
# export all
write.xlsx(c_citation, "search_citation.xlsx", row.names = FALSE)
write.xlsx(aux_hab, "habitat.xlsx", row.names = FALSE)
write.xlsx(aux_narrative, "narrative.xlsx", row.names = FALSE)
write.xlsx(aux_history, "history.xlsx", row.names = FALSE)
write.xlsx(aux_country, "country.xlsx", row.names = FALSE)

#############################################################################################################

