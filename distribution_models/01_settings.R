# Pre-processing settings from Muylaert et al. ---------------------------------------------------------------

# Messages
message("Settings from Muylaert et al. dynamic pipeline.")
message("Make sure you are in distribution_models R project")

# Directory

library(here)
setwd(here())
master <- "CC_master_bat_data_2021_09_D16.txt"
dynmaster <- paste0(here(), "/dynamic_master/")

# Post processing settings
projdir <- paste0(here(), "")

# Name version based on results file from ENMTML
version_suffix <- "results_40o_ss6_maxent_15rep"

# Results not intersected by IUCN
enmresultsdir <- paste0(here(), "/results_40o_ss6_maxent_15rep/")
binrasterdir <- paste0(enmresultsdir, "/Ensemble//W_MEAN//MSDMPosterior//BIN//")

# Not spatially restricted by MSDM
binrasterdirn <- paste0(here(), "/results_40o_ss6_maxent_15rep/Ensemble/W_MEAN/MAX_TSS/")
nonrestrictedrasters <- paste0(enmresultsdir, "Ensemble//W_MEAN//MAX_TSS//")
aa <- paste0(enmresultsdir, "//Extent_Masks//")

# IUCN intersected data results
versioni <- "results_iucni_40o_ss6_maxent_15rep"
enmresultsdiri <- paste0(here(), "/results_iucni_40o_ss6_maxent_15rep/")
binrasterdiri <- paste0(enmresultsdiri, "Ensemble//W_MEAN//MSDMPosterior//BIN//")
aai <- paste0(enmresultsdiri, "//Extent_Masks//")

# Settings for climate change projection files
other_suffix <- "_27kms"
periods_fut <- "2021-2040|2041-2060|2061-2080|2081-2100" # NO SPACE between terms
scenarios <- c("_ssp245_", "_ssp585_")
pathways <- "ssp245|ssp585"

# The sub() function will replace the first occurrence leaving the other as it is. On the other hand, the gsub() function will replace all the strings or values with the input strings.
s1 <- sub(other_suffix, "", list.files())
s2 <- sub("2021-2040|2041-2060|2061-2080|2081-2100", "", s1)
gcms <- sub("_ssp245_|_ssp585_", "", s2)

# Algorithms
algo <- paste0(here(), "//", version_suffix, "//Algorithm")

# -------------------------------------------------------------------------

# Settings for climate change projection files
other_suffix <- "_27kms"
periods_fut <- "2021-2040|2041-2060|2061-2080|2081-2100" # NO SPACE between terms
scenarios <- c("_ssp245_", "_ssp585_")
pathways <- "ssp245|ssp585"

s1 <- sub(other_suffix, "", list.files())
s2 <- sub("2021-2040|2041-2060|2061-2080|2081-2100", "", s1)
gcms <- sub("_ssp245_|_ssp585_", "", s2)

# Figure names 
figfut <- paste0("Figure_Future_Richness", version_suffix, ".jpg")
figfutnolog <- paste0("Figure_Future_Richness_nolog_", version_suffix, ".jpg")

# Table names
tabname <- paste0("Table_Range_", version_suffix, ".xlsx")

# end ---------------------------------------------------------------------

