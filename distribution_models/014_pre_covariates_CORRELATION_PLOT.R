# Raster correlation
# https://statnmap.com/2018-01-27-spatial-correlation-between-rasters/

source('00_packages.R')

# Settings for dynamism ######################################################################################

envrasters <- 'D://OneDrive - Massey University//_env//_env_27km_scaled///'

projdir <- 'D:/OneDrive - Massey University/PD_2021/distribution_models/'

version_suffix <- '_40oc'

of <- paste0('correlation_covariates', version_suffix , '.html')

ofs <- paste0('correlation_covariates_scaled', version_suffix , '.html')


setwd(envrasters)
rastlist <- list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)

allrasters <- stack(rastlist)

plot(allrasters)

d <- data.frame( rasterToPoints( allrasters ) )

d$x <- NULL
d$y <- NULL

#dir.create('raster_correlation/')
setwd(projdir)
setwd('raster_correlation')

#create_report(d, output_file = of)
#create_report(scale(d), output_file = ofs)

round(cor(d),   digits = 2 )


# All Bioclims

colnames(d)

labs <-c('Annual Mean Temperature', ' Annual Precipitation',
         'Precipitation Seasonality',
         ' Precipitation of Driest Quarter',
         'Temperature Seasonality',
         'Distance to karst', '% Forest')

colnames(d) <- labs
#cor pearson tests for a linear relationship between the two variables 
#require(ggplot2)
#ggplot(d , aes(x = bio_1, y = bio_12)) +
 # geom_point(colour = "#0c4c8a") +
#  theme_minimal()

# Corrplot
# https://rpkgs.datanovia.com/rstatix/reference/cor_plot.html


cor.mat <- d %>%
  cor_mat()


cor.lower.tri <- cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle()

png('six_covar_correlation_plot.png')

cor.lower.tri %>%
  cor_plot(method = "number")

dev.off()


hist(d$` Precipitation of Driest Quarter`)

hist(d$` Annual Precipitation`)

# Export
require(ggplot2)

dm <- reshape2::melt(d)
head(dm)
hist(d$`Annual Mean Temperature`)
ggplot(na.omit(dm) ) + geom_histogram(aes(x=value)) +
  facet_grid(~variable)

# Drop bio17
# Bio 17 is skewed Precipitation of the driest quarter
# Bio 12 annual precipitation is correlated with bio17 prec driest quarter
# Karst is skewed


d$` Precipitation of Driest Quarter` <- NULL


cor.mat <- d %>%
  cor_mat()


cor.lower.tri <- cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle()

jpeg('five_covar_correlation_plot.jpg', res=600, width=10, height =10)

cor.lower.tri %>%
  cor_plot(method = "number")

dev.off()



