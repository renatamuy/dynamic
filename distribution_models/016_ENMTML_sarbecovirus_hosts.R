###################################
# Script ENMTML
# R version 4.1.0 (2022-08-11)
###################################

warning('SLOW')

memory.size()
Sys.getenv("R_ARCH")
memory.limit()

ls()
rm(list = ls())
gc() 
options(digits=7, scipen=999)
memory.limit(size= 1.75e13)

library(ENMTML)
library(rstan)
library(raster)
library(spData)
library(spdep)
require(rgeos)
library(spDataLarge)

setwd(here())

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
parallel::detectCores()

d_climate <- file.path('_env_27km_ss6/')

f_occ <- "dynamic_master/CC_master_bat_data_2021_09_D16.txt"

# Subset of scaled variables

d_proj <-  file.path('_env_fut_27km_ss6/') 

start <- print(Sys.time())

ENMTML( pred_dir = d_climate,
        proj_dir = d_proj,
        result_dir = 'results', 
        occ_file = f_occ,
        sp = 'sp',
        x = 'long',
        y = 'lat',
        min_occ = 40,
        thin_occ = c(method='USER-DEFINED', distance='13'),
        eval_occ = NULL,
        colin_var= NULL, 
        imp_var = TRUE,
        sp_accessible_area=c(method='MASK', filepath='D://OneDrive - Massey University//_env//olson/wwf_terr_ecos.shp'), 
        pseudoabs_method=c(method='GEO_ENV_CONST', width='50'),
        pres_abs_ratio = 1,
        part= c(method ='BOOT', replicates = '10', proportion = '0.75'), 
        save_part = FALSE,
        save_final = TRUE,
        algorithm = c('MXS','MXD'), 
        thr = c(type=c('MAX_TSS')),
        msdm=c(method='OBR'), 
        ensemble=c(method=c('W_MEAN'), metric='TSS'),
        extrapolation = TRUE,
        cores = 8)

end <- print(Sys.time())

print(end-start)

###############################################################################################################
# IUCN intersected

setwd(here())

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
parallel::detectCores()

d_climate <- file.path('_env_27km_ss6/')

f_occ <- "dynamic_master/CC_IUCN_intersect_master_bat_data_2021_09_D16_TAB.txt"

# Subset of variables scaled

d_proj <-  file.path('_env_fut_27km_ss6/') 

start <- print(Sys.time())

ENMTML( pred_dir = d_climate,
        proj_dir = d_proj,
        result_dir = 'results_iucn_intersected', 
        occ_file = f_occ,
        sp = 'sp',
        x = 'long',
        y = 'lat',
        min_occ=40,
        thin_occ=c(method='USER-DEFINED', distance='13'),
        eval_occ = NULL,
        colin_var= NULL, 
        imp_var = TRUE,
        sp_accessible_area=c(method='MASK', filepath='D://OneDrive - Massey University//_env//olson/wwf_terr_ecos.shp'), 
        pseudoabs_method=c(method='GEO_ENV_CONST', width='50'),
        pres_abs_ratio = 1,
        part= c(method ='BOOT', replicates = '10', proportion = '0.75'), 
        save_part = FALSE,
        save_final = TRUE,
        algorithm = c('MXS','MXD'),
        thr = c(type=c('MAX_TSS')),
        msdm=c(method='OBR'), 
        ensemble=c(method=c('W_MEAN'), metric='TSS'),
        extrapolation = TRUE,
        cores = 8)

end <- print(Sys.time())

print(end-start)

#------------------------------------------------------------------------