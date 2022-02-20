library("praise")
library("imager")
library("OpenImageR")
library("stringr")
library("openxlsx")
library("here")
library("ggplot2")
library("foreach")
library("raster")
library("sp")
library("mgcv")
library("cowplot")
library("RcppRoll")
library(doParallel)

font_size <- 14
add <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=font_size),
        axis.title=element_text(size=font_size),
        axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"),
        legend.text=element_text(size=font_size))


###################################################
########### Neural Network ########################
###################################################


csvs <- list.files("data/boletus_csv", "*csv", full.names = T)
lITV <- list()

#lowIm <- which.min(file.size(paste0("boletus_Bilder/B.e", str_extract_all(csvs, "\\d+")[[1]][1], ".jpg")))
#im <- imager::load.image(paste0("boletus_Bilder/B.e", str_extract_all(csvs, "\\d+")[[lowIm]][1], ".jpg"))

for(i in 1:length(csvs)){
  
  print(i)
  mask <- read.csv2(csvs[i])
  im <- load.image(paste0("data/boletus_Bilder/B.e", str_extract_all(csvs[i], "\\d+")[[1]][1], ".jpg"))
  
  mask <- data.frame(t(mask))
  colnames(mask) <- paste0("col", 1:ncol(mask))
  rownames(mask) <- paste0("row", 1:nrow(mask))
  
  
  mask2 <- as.cimg(array(1, c(nrow(mask),ncol(mask), 1 ,1)))
  mask2[,,,1] <- 0
  mask2[,,,1][mask == 1] <- 1
  
  
  mask <- resize(mask2, 500, 500)
  im <- resize(im, 500, 500)
  
  im_hsl <- imager::RGBtoHSL(im)
  l <- im_hsl[,,,3]
  
  lITV[[i]] <- l[mask == 1]
  
  
}
names(lITV) <- unlist(str_extract_all(csvs, "\\d+"))

min.pix.l <- length(lITV[[which.min(lapply(lITV, length))]])

s <- foreach(i = 1:length(lITV)) %dopar% {
  lITV[[i]][ceiling(seq(1, length(lITV[[i]]), length(lITV[[i]])/50))]
}
names(s) <- names(lITV)

nn <- reshape::melt(data.frame(do.call(cbind, s)))
nn$variable <- gsub("X", "", nn$variable)
nn <- aggregate(value ~ variable, data = nn, FUN = mean)
names(nn) <- c("ID", "mushroom")


#################### iNat data ####################
###################################################
# downloaded from iNaturalist directly
# (match via IF in image_url) 
coord <- read.csv2("data/observations-119685.csv",
                   sep = ",")
coord$image_ID <- str_extract(coord$image_url, "\\d+")


#################### CLICK Data ###################
###################################################

cl <- read.xlsx("data/bh.xlsx")

#################### MATCH ########################
###################################################
aml <- data.frame(nn, coord[match(nn$ID, coord$image_ID), ])
aml <- aml[!is.na(aml$ID), ]
aml <- aml[, !str_detect(names(aml), "ID\\.1")]
aml <- data.frame(L_click = cl[match(aml$ID, cl$ID), 2], aml)

colnames(aml)[1:3] <- c("L_click", "ID", "L_nn")
aml <- data.frame(aml)


#################### Data Prep  ###################
###################################################
str(aml)
aml$L_click <- as.numeric(aml$L_click)
aml$L_nn <- as.numeric(aml$L_nn)
aml$longitude <- as.numeric(aml$longitude)
aml$latitude <- as.numeric(aml$latitude)
aml <- aml[!is.na(aml$latitude), ]

## Add WORLDCLIM / macroclimatic variables
r <- raster::getData("worldclim",var="bio", res=2.5)
co <- SpatialPoints(coords = aml[, c("longitude", "latitude")])
co@proj4string@projargs <- r@crs@projargs
res <- raster::extract(r, co)
res <- data.frame(res)
aml <- data.frame(aml, res)

t <- raster::getData('worldclim', var='tmean', res=2.5)
res <- raster::extract(t, co)
res <- data.frame(res)
aml <- data.frame(aml, res)
aml$month <- paste0("tmean", lubridate::month(as.Date(aml$observed_on)))
for(i in 1:nrow(aml))
  aml$temp_month[i] <- aml[i , match(aml$month[i], names(aml))]

# t <- getData('worldclim', var='tmin', res=2.5)
# res <- raster::extract(t, co)
# res <- data.frame(res)
# aml <- data.frame(aml, res)
# aml$month2 <- paste0("tmin", lubridate::month(as.Date(aml$observed_on)))
# for(i in 1:nrow(aml))
#   aml$temp_min_month[i] <- aml[i , match(aml$month2[i], names(aml))]



#################### PLOT       ###################
###################################################

stat <- summary(lm(L_nn ~ L_click, data = aml))

p2s <- summary(lm(bio1/10 ~ L_nn, data = aml))
p3s <- summary(lm(bio4 ~ L_nn, data = aml))
p4s <- summary(lm(temp_month/10 ~ L_nn, data = aml))

p1 <- ggplot(aml[aml$latitude > 25, ], aes(x = L_click*100, y = L_nn*100)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylim(0,100) +
  xlim(0,100)+
  add +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", round(stat$r.squared[1],4)*100,"%","beta=", round(stat$coefficients[2], 2), "t=", round(stat$coefficients[6], 2), "p<0.001"))+
  labs(x = "Helligkeit [%] (Methode nach Krah et al. (2019)", y = "Helligkeit [%] (Neur. Netzwerk)")
p1 



p2 <- ggplot(aml[aml$latitude > 25, ], aes(x = bio1/10, y = L_nn*100)) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE) +
  ylim(0,100) +
  add +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", round(p2s$r.squared[1],4)*100,"%","t=", round(p2s$coefficients[6], 2), "p=0.094"))+
  labs(x = "Jaehrl. Durchschnittstemp. [°C]", y = "Helligkeit [%] (NN)")
  #theme(axis.title.y.left = element_blank()) 
p2


p3 <- ggplot(aml[aml$latitude > 25, ], aes(x = bio4, y = L_nn*100)) +
  geom_point() +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_smooth(method = "gam", se = FALSE) +
  ylim(0,100) +
  add +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", round(p3s$r.squared[1], 4)*100,"%","t=", round(p3s$coefficients[6], 2), "p=0.17"))+
  labs(x = "Temperatursaisonalitaet [Stand.abw.*100]", y = "Helligkeit [%] (NN)")
  #theme(axis.title.y.left = element_blank()) 
p3


p4 <- ggplot(aml[aml$latitude > 25, ], aes(x = temp_month/10, y = L_nn*100)) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE) +
  ylim(0,100) +
  add +
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", round(p4s$r.squared[1], 4)*100,"%","t=", round(p4s$coefficients[6], 2), "p=0.024"))+
  labs(x = "monatl. Durchschnittstemp. [°C]", y = "Helligkeit [%] (NN)")
  #theme(axis.title.y.left = element_blank()) 
p4
          
#p5 <- plot_grid(p2, p3, p4, label = c("a)", "b)", "c)"), nrow = 1, ncol = 3)

## Save plots
#ggsave(p1, file = paste0("fig/L_compare_methods", Sys.Date(), ".pdf"), width = 4, height = 4)
#ggsave(p5, file = paste0("fig/Boletus_edulis_L_Temp", Sys.Date(), ".pdf"), width = 12, height = 4)

library(ggpubr)
my_plot_list <- list(p4,p2,p3)

p5 <- ggarrange(plotlist = my_plot_list, labels = c("a)", "b)", "c)"), nrow = 2, ncol =2)

# p5 <- annotate_figure(p5,left = text_grob("Helligkeit [%] (NN)",rot = 90, size = 12.5, face = "bold"))
p5

praise()
