library("praise")
library("imager")
library("OpenImageR")
library("stringr")
library("openxlsx")
library("here")
library("ggplot2")
library("foreach")
library("doParallel")


###################################################
########### Neural Network ########################
###################################################

registerDoParallel(cores = 6)

csvs <- list.files("data/boletus_csv/", "*csv", full.names = T)
lITV <- list()

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
  
  
  res <- foreach(grain = seq(5, 100, 5)) %:% 
    foreach(col = seq(5, 500, grain), .combine = "cbind") %:% 
    foreach(row = seq(5, 500, grain), .combine = "rbind") %dopar% {
      
      ifelse(mask[,,,1][col, row] == 1,  im[,,,1][col, row], 0)
    }
  
  im_hsl <- imager::RGBtoHSL(im)
  l <- im_hsl[,,,3]
  
  lITV[[i]] <- cbind.data.frame(lightness = c(l[mask == 0], l[mask == 1]),
                                type = c(rep("background", length(l[mask == 0])), rep("mushroom", length(l[mask == 1]))),
                                ID = str_extract_all(csvs[i], "\\d+")[[1]][1])
  
  lITV[[i]] <- data.frame(t(aggregate(lightness ~ type, data = lITV[[i]], FUN = mean)), ID = str_extract_all(csvs[i], "\\d+")[[1]][1])
  lITV[[i]] <- lITV[[i]][-1,]
  # lITV[[i]] <- do.call(cbind, lapply(split(lITV[[i]], factor(lITV[[i]]$type)), function(x)  x[sample(nrow(x), 1000), ]))
  # lITV[[i]] <- lITV[[i]][, c(1, 3, 4)]
  # lITV[[i]] <- c(mean(lITV[[i]][, 1]), lITV[[i]][1, 2], mean(lITV[[i]][, 3]))
  
}


lITV <- data.frame(do.call(rbind, lITV))
names(lITV) <- c("background", "mushroom", "ID")
nn <- lITV



###################################################
#################### CLICK ########################
###################################################

cl <- read.xlsx("data/bh.xlsx")



#################### MATCH ########################
###################################################
aml <- data.frame(cl, nn[match(cl$ID, nn$ID), ])
aml <- aml[, !str_detect(names(aml), "ID\\.1")]
colnames(aml) <- c("ID", "L_click", "lat", "lon", "L_nn_back", "L_nn")


#################### PLOT  ########################
###################################################

str(aml)
aml$L_nn <- as.numeric(aml$L_nn)
aml$L_nn_back <- as.numeric(aml$L_nn_back)

#p8s <- summary(lm(lat ~ L_nn, data = aml))
#p9s <- summary(lm(lon ~ L_nn, data = aml))
p10s <- summary(lm(lat > 25 ~ L_nn, data = aml))
p11s <- summary(lm(log10(lon) ~  L_nn, data = aml))


library(mgcv)
p8<-ggplot(aml[aml$lat > -90, ], aes(x = lat, y = L_nn*100)) +
  geom_point() +
  #scale_x_continuous(breaks = scales::pretty_breaks(n =5)) +
  geom_smooth(method = "gam", se = FALSE) +
  ylim(0,100)+
  xlim(-45,60)+
  add+
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", (round(p8s$r.squared[1], 6)*100),"%","t=", round(p8s$coefficients[6], 2), "p=0.92"))+
  labs(x = "Breitengrad [Dezimalgrad°]", y = "Helligkeit [%] (NN)")
p8
p10<-ggplot(aml[aml$lat > 25, ], aes(x = lat, y = L_nn*100, )) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE) +
  ylim(0,100)+
  xlim(35,60)+
  add+
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", round(p10s$r.squared[1], 4)*100,"%","t=", round(p10s$coefficients[6], 2), "p=0.30"))+
  labs(x = "Breitengrad [Dezimalgrad°]", y = "Helligkeit [%] (NN)")
p10


p9 <- ggplot(aml[aml$lon > -180, ], aes(x = lon, y = L_nn*100)) +
  geom_point() +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_smooth(method = "gam", se = FALSE) +
  ylim(0,100)+
  xlim(-180,180)+
  add+
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", round(p9s$r.squared[1], 6)*100,"%","t=", round(p9s$coefficients[6], 2), "p=0.77"))+
  labs(x = "Längengrad [Dezimalgrad°]", y = "Helligkeit [%] (NN)")
p9


p11 <- ggplot(aml[aml$lon > 0, ], aes(x = lon, y = L_nn*100, )) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE) +
  ylim(0,100)+
  #xlim(0,180)+
  scale_x_continuous(trans = 'log10') + 
  #annotation_logticks(sides = "b")+
  add+
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1, label = paste("R² =", round(p11s$r.squared[1], 4)*100,"%","t=", round(p11s$coefficients[6], 2), "p=0.09"))+
  labs(x = "lg(Längengrad) [Dezimalgrad°]", y = "Helligkeit [%] (NN)")
p11

library(ggpubr)

#plot_row <- list(p8,p9,p10,p11)
#ggarrange(plotlist = plot_row, labels = c("a)", "b)","c)","d)" ), nrow = 2, ncol = 2)
#plot_row

my_plot_list2 <- list(p10,p11)
p6 <- ggarrange(plotlist = my_plot_list2, labels = c("a)", "b)"), nrow = 1, ncol = 2)
p6

