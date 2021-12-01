#' ----
#' Range overlap
#' ---- 

# prepare r ---------------------------------------------------------------

# source
source("00_packages.R")
source("01_settings.R")

# directory
setwd(here())

# options
options(digits = 3, scipen = 999)

# richness map ------------------------------------------------------------

# import species
setwd(aa)
all_aa <- raster::stack(dir(pattern = ".tif$"))

# import species binary
setwd(binrasterdir)
all <- raster::stack(dir(pattern = ".tif$"))

# mask
allm <- raster::mask(all, all_aa)

# area
recebe <- tibble::tibble()

for(i in names(allm)){
  
  for(j in names(allm)){
    
    if(i == j){
      next
    }
    
    summed <- allm[[i]] + allm[[j]]
    
    values(summed)[values(summed) != 2] = NA
    
    ar <- area(summed, na.rm = TRUE)
    
    areasoma <- sum(values(ar), na.rm = TRUE)
    
    aux <- tibble::tibble(sp1 = i, sp2 = j , area_overlapped = areasoma)
    recebe <- rbind(recebe, aux)
    
    print(i)
    print(j) 
    
  }
  
}

recebe

rexpo <- recebe %>% 
  arrange(desc(area_overlapped))

# export
setwd(projdir)
setwd("range_tables")

writexl::write_xlsx(rexpo, "range_overlap_present.xlsx")


# triangular matrix - mau
comb <- combn(names(allm), 2, simplify = FALSE)
areas <- tibble::tibble()

for(i in comb){
  
  summed <- allm[[i[1]]] + allm[[i[2]]]
  
  values(summed)[values(summed) != 2] <- NA
  
  ar <- area(summed, na.rm = TRUE)
  
  areasoma <- sum(values(ar), na.rm = TRUE)
  
  areas <- dplyr::bind_rows(areas,
                            tibble::tibble(sp1 = i[1], sp2 = i[2], 
                                           area_overlapped = areasoma))
  
  print(i)
  
}

areas

# heatmap present ---------------------------------------------------------

# data
withself <- read.xlsx("range_overlap_present.xlsx", sheetIndex = 1)

omatrix <- reshape(withself, direction = "wide", idvar = "sp2", timevar = "sp1")
omatrix

withself$sp1 <- sub("_", " ", stringr::str_to_sentence(withself$sp1))
withself$sp2 <- sub("_", " ", stringr::str_to_sentence(withself$sp2))

# plot
col.range = c(10000, max(withself$area_overlapped))

oheat <- ggplot(data = withself, aes(y = sp1, x = sp2, fill = area_overlapped)) + 
  geom_tile(alpha = 0.75) + 
  scale_fill_viridis(option = "D", direction = -1, 
                     trans = "log", limits = col.range) + 
  theme_bw() + 
  theme(plot.title.position = "plot", axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 13, face = "italic"), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size = 13, face = "italic")) + 
  labs(x = "", y = "", fill = "Area overlap (sqkm) ") 
oheat

# plot - mau
heat_areas <- areas %>% 
  dplyr::mutate(sp1 = sp1 %>% stringr::str_replace("_", " ") %>% 
                  stringr::str_to_sentence(),
                sp2 = sp2 %>% stringr::str_replace("_", " ") %>% 
                  stringr::str_to_sentence()) %>% 
  ggplot(aes(y = sp1, x = sp2, fill = area_overlapped)) + 
  geom_tile(alpha = 0.75) + 
  scale_fill_viridis(option = "D", direction = -1, 
                     trans = "log10", limits = col.range) + 
  theme_bw() + 
  theme(plot.title.position = "plot", axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 13, face = "italic"), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size = 13, face = "italic")) + 
  labs(x = "", y = "", fill = "Area overlap (sqkm) ") 
heat_areas

fign <- paste0("Fig_overlap_present_", version_suffix, ".jpg")
ggsave(filename = fign, oheat, width = 33, height = 29, units = "cm", dpi = 600)


# heatmap future ---------------------------------------------------------

# data
withselff <- read.xlsx("/home/mude/data/onedrive/manuscritos/03_preparacao/renata/02_dados/range_tables/range_overlap_future.xlsx", sheetIndex = 1)
head(withselff)

omatrixf <- reshape(withself, direction = "wide", idvar = "sp2", timevar = "sp1")
omatrixf

withselff$sp1 <- sub("_", " ", stringr::str_to_sentence(withselff$sp1))
withselff$sp2 <- sub("_", " ", stringr::str_to_sentence(withselff$sp2))

# plot
oheatfut <- ggplot(data = withselff, 
                   aes(y = sp1, x = sp2, fill = area_overlapped_future)) + 
  geom_tile(alpha = 0.75) + 
  scale_fill_viridis(option = "D", direction = -1, trans = "log10", 
                     limits = col.range) + 
  theme_bw() + 
  theme(plot.title.position = "plot", axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 13, face = "italic"), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size = 13, face = "italic")) + 
  labs(x = "", y = "", fill = "Future area overlap (sqkm)") 
oheatfut
getwd()

compo <- gridExtra::grid.arrange(oheat, oheatfut, ncol = 2)
compo

#fign <- paste0("Fig_overlap_future", version_suffix, ".jpg")
#ggsave(filename = fign, oheatfut, width = 33, height = 29, units = "cm", dpi = 600)


# Left join

withdif <- left_join(withself, withselff)

withdif$dif <- withdif$area_overlapped_future -withdif$area_overlapped
hist(withdif$dif)

summary(withdif$dif)
col.range1 = c(min(withdif$dif), max(withdif$dif))

#496657 
withdif$area_overlapped_future
withdif$area_overlapped

# Negative values is area overlap loss in the future
withdif$dif
plot(withdif$dif ~ scale(withdif$dif))
#limits = col.range1
oheatdif <- ggplot(data = withdif, aes(y = sp1, x = sp2, fill = scale(dif))) + 
  geom_tile(alpha = 0.75) + scale_fill_viridis(option = "D", direction = -1) + 
  theme_bw() + 
  theme(plot.title.position = "plot", axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 13, face = "italic"), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size = 13, face = "italic")) + 
  ylab("") + xlab("") + labs(fill = "Shift in future area overlap (scaled sqkm)") 

oheatdif

getwd()
summary(withdif$dif)

oheatdifr <- ggplot(data = withdif, aes(y = sp1, x = sp2, fill = dif)) + 
  geom_tile(alpha = 0.75) + scale_fill_viridis(option = "D", direction = -1 , 
                                               breaks = c(400000, 0, -500000 , -1000000, -1500000, -2000000)) + 
  theme_bw() + 
  theme(plot.title.position = "plot", axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 13, face = "italic"), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size = 13, face = "italic")) + 
  ylab("") + xlab("") + labs(fill = "Shift in future area overlap (sqkm)") 

oheatdifr
figr <- paste0("Fig_overlap_supplents_", version_suffix, ".jpg")
ggsave(filename = figr, oheatdifr, width = 33, height = 29, units = "cm", dpi = 600)



# overlap present-future --------------------------------------------------

withdif$difpct <- withdif$area_overlapped_future / withdif$area_overlapped 
withdif$difpct

nrow(withdif)

withdif$difpct

withdif$category <- ifelse(withdif$difpct == "NaN", "Never overlaps", 
                    ifelse(withdif$difpct == "Inf", "Starts overlapping in the future", 
                    ifelse(withdif$difpct == 0, "Stops overlaping in the future", 
                    ifelse(withdif$difpct < 1, "Overlaps less in the future", 
                    ifelse(withdif$difpct > 1, "Overlaps more in the future", NA)))))

table(withdif$difpct == 0)

withdif$difpct == "Inf"
withdif

mypali <- viridisLite::viridis(5)

oheatdifpct <- ggplot(data = withdif, aes(y = sp1, x = sp2, fill = category)) + 
  geom_tile(alpha = 0.75) + scale_fill_manual(values = mypali) + 
  theme_bw() + 
  theme(plot.title.position = "plot", axis.title = element_text(size = 11), 
        axis.text.y = element_text(size = 13, face = "italic"), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size = 13, face = "italic")) + 
  ylab("") + xlab("") + labs(fill = "Shift in area overlap") 
oheatdifpct

head(withdif)

fign <- paste0("Fig_overlap_shift_", version_suffix, ".jpg")
ggsave(filename = fign, oheatdifpct, width = 33, height = 29, units = "cm", dpi = 600)
write.xlsx(withdif, "range_overlap_shift.xlsx", row.names = FALSE)


# end ---------------------------------------------------------------------