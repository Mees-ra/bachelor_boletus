library("imager")
library("OpenImageR")
library("stringr")
library("foreach")
library("openxlsx")
library("here")
library("magrittr")

amanita <- list.files("A_muscaria_pics")

csvs <- list.files("boletus_csv", ".csv", full.names = T)



lITV <- list()
for(i in 1:504){
  
  print(i)
  mask <- read.csv2(csvs[i])
  im <- imager::load.image("C:/Users/insert username/Bachelor/11069_Boletus_edulis/Bol_edulis")
  paste0("C:/Users/Ramees Khawar/Bachelor/11069_Boletus_edulis/Bol_edulis", str_extract_all(csvs[i], "\\d+")[[1]][2], "*.jpg")
  
  im_hsl <- imager::RGBtoHSL(im)
  l <- im_hsl[,,,3]
  
  lITV[[i]] <- cbind.data.frame(lightness = c(l[mask == 0], l[mask == 1]), ##Background und Maske zuordnen
                                type = c(rep("background", length(l[mask == 0])), rep("mushroom", length(l[mask == 1]))),
                                ID = str_extract_all(csvs[i], "\\d+")[[1]][2])
  
  lITV[[i]] <- do.call(cbind, lapply(split(lITV[[i]], factor(lITV[[i]]$type)), function(x)  x[sample(nrow(x), 100), ]))
  lITV[[i]] <- lITV[[i]][, c(1, 3, 4)]
}


lITV <- do.call(rbind, lITV)
names(lITV) <- c("background","ID", "mushroom")
# lITV <- data.frame(background = lITV[lITV$type == "background", 1], lITV[lITV$type == "mushroom", c(1, 3)])

library(ggplot2)
p1 <- ggplot(lITV, aes(x = mushroom, y = background)) +
  geom_point() +
  geom_smooth(method = "lm")
p1

summary(lm(mushroom ~ background, data = lITV))


## match latitude... 
env <- read.xlsx("Regions/A-muscaria.xlsx")

env$ID <- str_extract(env$image_url, "\\d+")
env <- env[match(lITV$ID, env$ID), ]
lITV <- data.frame(lITV, env[, c("latitude", "longitude")])
lITV[] <- apply(lITV, 2, as.numeric)


p6 <- ggplot(lITV, aes(x = longitude, y = mushroom*100)) +
  geom_point() +
  geom_smooth(method = "gam") +
  theme_classic() +
  labs(x = "Longitude", y = "Mushroom color lightness")
p6

cowplot::plot_grid(p1, p2)