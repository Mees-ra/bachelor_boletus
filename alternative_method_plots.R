library("praise")
library("imager")
library("OpenImageR")
library("stringr")
library("openxlsx")
library("here")
library(ggplot2)

## match latitude... 

env <- read.xlsx("data/b1.xlsx")

mush <- read.xlsx("data/bhex.xlsx") 

env$ID <- str_extract(env$image_url, "\\d+")
env <- env[match(mush$ID, env$ID), ]
mush <- data.frame(mush, env[, c("latitude", "longitude")])
mush[] <- apply(mush, 2, as.numeric)

#x=longitude
p4 <- ggplot(mush, aes(x = latitude, y = mushroom*100)) +
  geom_point(size=0.1) +
  geom_smooth(method = "gam") +
  theme_classic() +
  labs(x = "Breitengrad (Dezimalgrad°)", y = "Fruchtkörperhelligkeit (%)")
p4

p5 <- ggplot(mush, aes(x = longitude, y = mushroom*100)) +
  geom_point(size=0.1) +
  geom_smooth(method = "gam") +
  theme_classic() +
  labs(x = "Längengrad (Dezimalgrad°)", y = "Fruchtkörperhelligkeit")
p5

plot_row <-cowplot::plot_grid(p1, p2, p3)

# now add the title
title <-cowplot:: ggdraw() +
  cowplot::draw_label(
    "Amanita muscaria",
    fontface = 'bold',
    x = 0.4,
    hjust = 0
  ) +
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