# Correlation betweein intersected and non intersected data
options(digits = 3, scipen = 999)

source('00_packages.R')

source('01_settings.R')

# Open files from ENMs

setwd(aa)
all_aa <- stack(list.files(pattern='.tif$'))

plot(all_aa)

setwd(binrasterdir)

all <- stack(list.files(pattern='.tif$'))

# over accessible area

allm <- mask(all, all_aa)

richness <- sum(allm, na.rm=TRUE) 

plot(richness)


# Open i files (Input data intersected by ranges)

setwd(aai)
all_aai <- stack(list.files(pattern='.tif$'))

setwd(binrasterdiri)

alli <- stack(list.files(pattern='.tif$'))

# over accessible area

allmi <- mask(alli, all_aai)

plot(allmi)
#

richnessi <- sum(allmi, na.rm=TRUE) 

# Cor

plot(values(richness) - values(richnessi))

rdif <- richness - richnessi

summary(richnessi$layer)
        
plot(richness)

plot(rdif)

# Excess in richness

table(values(rdif))

# Basically the continental ares where rdif==0 are the well sampled areas. So calculate area of whatever is not converging between maps, 
# everything that is different from ZERO, even negative values


# Take care because there are lot of zeroes in the world...

plot(table(values(rdif)), xlab='Difference between number of species', ylab='Number of pixels')

cor(values(richness), values(richnessi), method='pearson')

cor.test(values(richness), values(richnessi), method=c("pearson", "kendall", "spearman"))

##############################################################################################################