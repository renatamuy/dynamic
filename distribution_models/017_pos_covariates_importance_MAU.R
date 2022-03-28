#' ----
#' Contents
#' relative importance plot
#' ----

# prepare r ---------------------------------------------------------------

# directory
setwd(here::here())

# options
options(digits = 3, scipen = 999)

# source
source("00_packages.R")
source("01_settings.R")

# import
#dir.create("varimp")

figvar <- paste0("Fig_varimp", version_suffix, ".tif")

setwd(algo) # remove version object from setting as it conflicts with R version syntax

a <- purrr::map_dfr(.x = dir(pattern = ".txt", recursive = TRUE, full.names = TRUE), 
                    .f = read_tsv) %>% 
  tidyr::separate(col = Overall, into = c("Covariate", "Importance"), sep = "\t") %>% 
  janitor::clean_names() %>% 
  dplyr::select(-1) %>% 
  dplyr::rename(species = algorithm,
                algorithm = variables,
                variable = covariate) %>% 
  dplyr::mutate(species = species %>% stringr::str_to_title() %>% stringr::str_replace("_", " "),
                importance  = round(as.numeric(importance), 2))
a

dplyr::count(a, algorithm, variable)

# setting glm effects
a <- a %>% 
  dplyr::mutate(variable = stringr::str_replace(variable, "I\\(", "")) %>% 
  dplyr::mutate(variable = stringr::str_replace(variable, "\\^2\\)", ""))
a

# comentado

# library("plyr")
# 
# 
# a$splab <- sub("_", " ", stringr::str_to_sentence(a$Sp))
# head(a)
# 
# # Only if you used glm
# auxag <- aggregate(aux[, c("Importance")], by = list(aux$Algorithm, aux$Covariate2, aux$splab), "sum")
# 
# 
# head(auxag)
# 
# want <- unique(aux$Covariate)
# want
# 
# t <- read.table("D://OneDrive - Massey University//_env//bat_ensembles_OBR_nothin//Algorithm//BIO//Response Curves & Variable Importance//")

# stacked plot including GLM quadratic terms ------------------------------

# setwd(projdir)
# setwd("varimp")
# 
# # Correcting after aggregating Importances of GLM
# head(auxag)
# colnames(auxag) <- c("Algorithm", "Covariate", "splab", "Importance")
# 
# auxag <- auxag[!auxag$splab == "Vespertilio sinensis", ]


# plot --------------------------------------------------------------------

# recode variables
a_recod <- a %>% 
  dplyr::mutate(variable = fct_recode(variable, 
                                      "BIO01" = "bio_1", 
                                      "BIO12" = "bio_12", 
                                      "BIO15" = "bio_15", 
                                      "BIO04" = "bio_4", 
                                      "Karst" = "karstm", 
                                      "Forest" = "primf"))

a_recod

# add family
fct_sort <- function(.f, .fun = sort) {
  f = forcats:::check_factor(.f) # Not needed in dev version
  fct_relevel(f, .fun(levels(f)))
}

a_sp_recod <- a_recod %>% 
  dplyr::mutate(species = forcats::fct_recode(species, 
                                              "Hipposideridae - Aselliscus stoliczkanus" = "Aselliscus stoliczkanus", 
                                              "Hipposideridae - Hipposideros armiger" = "Hipposideros armiger", 
                                              "Hipposideridae - Hipposideros galeritus" = "Hipposideros galeritus", 
                                              "Hipposideridae - Hipposideros larvatus" = "Hipposideros larvatus", 
                                              "Hipposideridae - Hipposideros pomona" = "Hipposideros pomona", 
                                              "Hipposideridae - Hipposideros ruber" = "Hipposideros ruber", 
                                              "Rhinolophidae - Rhinolophus affinis" = "Rhinolophus affinis", 
                                              "Rhinolophidae - Rhinolophus blasii" = "Rhinolophus blasii", 
                                              "Rhinolophidae - Rhinolophus euryale" = "Rhinolophus euryale", 
                                              "Rhinolophidae - Rhinolophus ferrumequinum" = "Rhinolophus ferrumequinum", 
                                              "Rhinolophidae - Rhinolophus hipposideros" = "Rhinolophus hipposideros", 
                                              "Rhinolophidae - Rhinolophus macrotis" = "Rhinolophus macrotis", 
                                              "Rhinolophidae - Rhinolophus malayanus" = "Rhinolophus malayanus", 
                                              "Rhinolophidae - Rhinolophus mehelyi" = "Rhinolophus mehelyi", 
                                              "Rhinolophidae - Rhinolophus pearsonii" = "Rhinolophus pearsonii", 
                                              "Rhinolophidae - Rhinolophus pusillus" = "Rhinolophus pusillus", 
                                              "Rhinolophidae - Rhinolophus sinicus" = "Rhinolophus sinicus", 
                                              "Rhinolophidae - Rhinolophus thomasi" = "Rhinolophus thomasi", 
                                              "Miniopteridae - Miniopterus schreibersii" = "Miniopterus schreibersii", 
                                              "Molossidae - Chaerephon plicatus" = "Chaerephon plicatus", 
                                              "Vespertilionidae - Tadarida teniotis" = "Tadarida teniotis", 
                                              "Vespertilionidae - Nyctalus leisleri" = "Nyctalus leisleri", 
                                              "Vespertilionidae - Plecotus auritus" = "Plecotus auritus")) %>% 
  dplyr::mutate(species = fct_sort(species)) %>% 
  dplyr::mutate(species = forcats::fct_rev(species))
a_sp_recod$species %>% levels

# summarise
a_sp_recode_mean <- a_sp_recod %>% 
  dplyr::group_by(species, variable) %>% 
  dplyr::summarise(importance = mean(importance))
a_sp_recode_mean

# Final figure 
setwd(projdir)
setwd("varimp")
figrankname <- paste0("Figure 1_varimp_family_", version_suffix, ".png")

figrank <- ggplot(a_sp_recode_mean, 
                  aes(x = variable, y = species, fill = importance*100,
                      label = paste0(round(importance, 2)*100, "%"))) +
  geom_tile(alpha = .9) + 
  scale_fill_viridis(option = "E", direction = 1) + 
  geom_text(size = 4) + 
  # facet_wrap( . ~algorithm) +
  theme_bw(base_size = 15) +
  theme(plot.title.position = "plot", 
        axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 10, face = "italic")) + 
  labs(x = "", y = "", fill = "Importance") 
figrank

#ggsave(filename = figrankname, figrank, width = 26, height = 30, units = "cm", dpi = 600)

# Averaging by algorithm ensemble
figranknameav <- paste0("Figure 1_family_", version_suffix, ".png")

# Aggregate by 3 grouping vars

ord2 <- aggregate(Importance ~ splab + Covariate, data = ord, FUN = mean, na.rm = FALSE) 
ord2
ord3 <- ord2 %>% arrange(splab, desc(Importance))
ord3
nrow(ord3)
ord3$rank <- rep(c(1, 2, 3, 4, 5, 6), nrow(ord3)/6)
ord3$rank

head(ord3) 
unique(ord3$splab)

require(tidyverse)
require(janitor)
ord4 <- ord3 %>% mutate(family = fct_recode(splab, 
                                            "Hipposideridae" = "Aselliscus stoliczkanus" , 
                                            "Hipposideridae" = "Hipposideros armiger" , 
                                            "Hipposideridae" = "Hipposideros galeritus" , 
                                            "Hipposideridae" = "Hipposideros larvatus", 
                                            "Hipposideridae" = "Hipposideros pomona" , 
                                            "Hipposideridae" = "Hipposideros ruber" , 
                                            "Rhinolophidae" = "Rhinolophus affinis", 
                                            "Rhinolophidae" = "Rhinolophus blasii", 
                                            "Rhinolophidae" = "Rhinolophus euryale" , 
                                            "Rhinolophidae" = "Rhinolophus ferrumequinum" , 
                                            "Rhinolophidae" = "Rhinolophus hipposideros" , 
                                            "Rhinolophidae" = "Rhinolophus macrotis", 
                                            "Rhinolophidae" = "Rhinolophus malayanus" , 
                                            "Rhinolophidae" = "Rhinolophus mehelyi" , 
                                            "Rhinolophidae" = "Rhinolophus pearsonii" , 
                                            "Rhinolophidae" = "Rhinolophus pusillus" , 
                                            "Rhinolophidae" = "Rhinolophus sinicus" , 
                                            "Rhinolophidae" = "Rhinolophus thomasi" , 
                                            "Miniopteridae" = "Miniopterus schreibersii", 
                                            "Molossidae" = "Chaerephon plicatus", 
                                            "Vespertilionidae" = "Tadarida teniotis" , 
                                            "Vespertilionidae" = "Nyctalus leisleri" , 
                                            "Vespertilionidae" = "Plecotus auritus", ))
#%>% tabyl(family)


#aes(colour = df2$Count))
require(RColorBrewer)
ord4$splabf <- as.factor(ord4$splab)

levels(ord4$splabf)

ord4 %>% arrange(family) 

ord4$compo <- paste(ord4$family, paste0("-"), ord4$splabf)
figranka <- ord4 %>%
  ggplot(aes(y = compo, x = Covariate, fill = rank, label = paste0(round(Importance, 2)*100, "%"))) +
  geom_tile(alpha = 0.78) +
  scale_fill_viridis( option = "E", direction = -1) + 
  geom_text(size = 4, fontface = "bold") +
  theme_bw()+
  theme(plot.title.position = "plot", axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 10, face = "italic") , 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size = 11))+
  ylab("Family - Species") +ggtitle("A.")

figranka

ggsave(filename = figranknameav, figranka, width = 23, height = 30, units = "cm", dpi = 600,)

# Violin plot

####################################################################################################
# Geom_violin with order of importance from most to least important order and anova

setwd("varimp")

figanova <- paste0("Fig_varimp_anova", version_suffix, ".tif")

aux <- aux[!aux$splab == "Vespertilio sinensis", ]

# only useful if you use many algorithms

#tiff(filename = figanova, 
# res = 400, width = 34, height = 40, units = "cm")

aux %>% 
  mutate(Covariate = fct_reorder(Covariate, desc(Importance))) %>% # x first always
  filter(Covariate == want) %>% ggplot(aes(y = Importance, x = Covariate)) +
  geom_violin( alpha = 0.9, size = 1) +
  stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "#37476b")+
  facet_wrap( . ~splab, ncol = 4) +
  stat_compare_means(label = "p.signif", ref.group = "bio_4")+
  geom_hline(yintercept = mean(aux$Importance), linetype = 2)+ 
  theme_bw()+ theme(strip.text = element_text(size = 16, face = "italic"), 
                    axis.text.y = element_text(size = 16), 
                    axis.text.x = element_text(size = 16, angle = 90, 
                                               vjust = 0.00, 
                                               hjust = 0), 
                    axis.title = element_text(size = 16))
dev.off()

############################

# Importance highlighted per algorithm
aux %>% filter(Covariate == want) %>% ggplot(aes(x = Importance, y = Algorithm, colour = splab))+
  geom_point(alpha = 0.8, size = 3, stroke = 1) + facet_wrap( . ~forcats::fct_infreq(Covariate)) +
  scale_color_viridis(discrete = TRUE, option = "D")+ theme_bw() 


# Importance highlighted per species
auxs <- aux %>% group_by(sp, Covariate) %>% 
  summarise(mean = mean(Importance), n = n())

maximp <- aux %>% 
  group_by(sp, Covariate) %>% 
  summarise(Value = max(Importance))

#figalgcor <- paste0("Fig_varimp_algorithm_convergence", version_suffix, ".tif")
#tiff(filename = figalgcor, res = 400, width = 34, height = 40, units = "cm")

aux %>% filter(Covariate == want) %>% ggplot(aes(y = Importance, x = Covariate, colour = Algorithm))+
  geom_jitter(width = 0.1, height = 0, alpha = 0.8, size = 3, stroke = 1) + facet_wrap( . ~splab) +
  scale_color_viridis(discrete = TRUE, option = "D")+ theme_bw() +theme(strip.text = element_text(face = "italic", size = 14), 
                                                                        axis.title = element_text(size = 14), 
                                                                        axis.text = element_text(size = 10) , 
                                                                        axis.text.x = element_text(angle = 90, 
                                                                                                   vjust = 0.01, 
                                                                                                   hjust = 0)) 

dev.off()

inp <- ord4
inp$rank <- NULL

dwide <- pivot_wider(inp, id_cols = splab, names_from = Covariate, values_from = Importance)

nrow(dwide)
colnames(dwide)

library(factoextra)


justc <- as.data.frame(dwide[, 2:7])
row.names(justc) <- dwide$splab

row.names(dwide)
nrow(dwide)
res.pca <- prcomp(justc, scale = TRUE)

fviz_eig(res.pca)

head(dwide)

# habillage color by trait

figpca <- paste0("Fig_pca_varimp_", version_suffix, ".png")

figbiplot <- fviz_pca_biplot(res.pca, repel = TRUE, 
                             col.var = "steelblue", # Variables color
                             col.ind = "black" , label = "all", labelsize = 5, 
                             pointsize = 4, 
                             col.quanti.sup = "blue", title = "B. PCA - Biplot")

jpeg(filename = figpca, res = 400, width = 28, height = 28, units = "cm")

figbiplot

dev.off()

figboth <- paste0("Fig_varimp_pca_", version_suffix, ".png")

ggsave(filename = figboth, gridExtra::grid.arrange(figranka, figbiplot, nrow = 2, heights = c(0.55, 0.45)), 
       width = 26, height = 39, units = "cm")
