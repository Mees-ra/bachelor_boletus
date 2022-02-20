library("praise")
library("imager")
library("OpenImageR")
library("stringr")
library("openxlsx")
library("here")

csvs <- list.files("data/boletus_csv", "*csv", full.names = T)

lITV <- list()
for(i in 1:327){
  
  print(i)
  mask <- read.csv2(csvs[i])
  im <- load.image(paste0("data/boletus_Bilder/B.e", str_extract_all(csvs[i], "\\d+")[[1]][1], ".jpg"))
  
  im_hsl <- imager::RGBtoHSL(im)
  l <- im_hsl[,,,3]
  
  lITV[[i]] <- cbind.data.frame(lightness = c(l[mask == 0], l[mask == 1]),
                            type = c(rep("background", length(l[mask == 0])), rep("mushroom", length(l[mask == 1]))),
                            ID = str_extract_all(csvs[i], "\\d+")[[1]][1])
  
  lITV[[i]] <- do.call(cbind, lapply(split(lITV[[i]], factor(lITV[[i]]$type)), function(x)  x[sample(nrow(x), 100), ]))
  lITV[[i]] <- lITV[[i]][, c(1, 3, 4)]
}


lITV <- do.call(rbind, lITV)
names(lITV) <- c("background","ID", "mushroom")
#lITV <- data.frame(background = lITV[lITV$type == "background", 1], lITV[lITV$type == "mushroom", c(1, 3)])

library(ggplot2)
#p1 <- ggplot(lITV, aes(x = mushroom, y = background)) +
  #geom_point(size=0.1) +
  #geom_smooth(method = "lm")+
  #theme_bw()+
 # labs(y = "Hintergrundhelligkeit", x = "Fruchtk?rperhelligkeit ")
#p1

summary(lm(mushroom ~ background, data = lITV))


## match latitude... 
env <- read.xlsx("data/b1.xlsx")


env$ID <- str_extract(env$image_url, "\\d+")
env <- env[match(lITV$ID, env$ID), ]
lITV <- data.frame(lITV, env[, c("latitude", "longitude")])
lITV[] <- apply(lITV, 2, as.numeric)

                      
p6 <- ggplot(lITV > 25, aes(x = latitude, y = mushroom*100)) +
  geom_point(size=0.1) +
  geom_smooth(method = "gam") +
  theme_classic() +
  labs(x = "Breitengrad (Dezimalgrad)", y = "Helligkeit (Neur. Netzwerk) [%]")
p6

p7 <- ggplot(lITV, aes(x = longitude > 25, y = mushroom*100)) +
  geom_point(size=0.1) +
  geom_smooth(method = "gam") +
  theme_classic() +
  labs(x = "Längengrad (Dezimalgrad)", y = "Helligkeit (Neur. Netzwerk) [%]")
p7

plot_row <-cowplot::plot_grid( p6, p7)

 # now add the title
#title <-cowplot:: ggdraw() +
  #cowplot::draw_label(
   # "Amanita muscaria",
    #fontface = 'bold',
    #x = 0.4,
    #hjust = 0
 # ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
cowplot::plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
praise()
