library(imager)
library(OpenImageR)
library("here")

img <- "C:/Users/x/Bachelor/Amanita_muscariaTRUE10421901.jpg"
#Testbild
predimg <-"C:/Users/x/Bachelor/predAmanita_muscariaTRUE10421901.jpg.jpg"
#Testbild mit Maske
predimcsv <-"C:/Users/x/Bachelor/Amanita_muscariaTRUE10421901.jpg_pred.csv"
#CSV-Datei mit Maske

val2 <- "C:\\Users\\x\\Bachelor\\RCNN_mushroom_test\\RCNN_mushroom_test\\customImages\\val2"
#Ort des Testbilds

a <- imager::load.image(img)
a_f <- imager::load.image(paste0(predimg))
b <- read.csv2(paste0(predimcsv))
b <- t(b)

ahsl <- imager::RGBtoHSL(a)
al <- ahsl[,,1,3]

# 
# par(mfrow = c(3,1))
# plot(a)
# plot(a_f)
# hist(al[b == 0]/sum(al[b == 0]), border = "blue", density = 10)
# hist(al[b == 1], add = TRUE, border ="red", density = 10)
# 

l_dat <- cbind.data.frame(lightness = c(al[b == 0], al[b == 1]), type = c(rep("background", length(al[b == 0])), rep("mushroom", length(al[b == 1]))))
l_dat$type <- factor(l_dat$type)
library(ggplot2)

ggplot(l_dat, aes(x = lightness, group = type, color = type)) +
  geom_density(size = 3) +
  theme_bw() +
  ggtitle("Boletus", img)




