library("openxlsx")
library("here")
library("ggplot2")

write.xlsx(mush,"data/ah.xlsx",asTable = F)

env <- read.xlsx("data/b1.xlsx")

mush <- read.xlsx("data/bhex.xlsx") 

env$ID <- str_extract(env$image_url, "\\d+")
env <- env[match(mush$ID, env$ID), ]
mush <- data.frame(mush, env[, c("latitude", "longitude")])
mush[] <- apply(mush, 2, as.numeric)


h1 <-ggplot(mush, aes(x = latitude)) +
     geom_histogram(binwidth = 5,color="black",fill="steelblue") +
     scale_x_continuous(breaks = seq(-50, 70,10), lim = c(-50,70))+
     scale_y_continuous(breaks = seq(0, 60,10), lim = c(0,60))+
     labs(x = "Breitengrad (Dezimalgrad°)", y = "Anzahl Pilze",title = "Histogramm Amanita muscaria")+
     theme_bw()
h1

h2 <-ggplot(mush, aes(x = latitude)) +
  geom_histogram(binwidth = 5,color="black",fill="steelblue") +
  scale_x_continuous(breaks = seq(-50, 70,10), lim = c(-50,70))+
  scale_y_continuous(breaks = seq(0, 80,10), lim = c(0,80))+
  labs(x = "Breitengrad (Dezimalgrad°)", y = "Anzahl Pilze",title = "Histogramm Boletus edulis")+
  theme_bw()
h2
