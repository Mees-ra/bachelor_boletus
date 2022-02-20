library("reshape2")
library("grid")
library("gridGraphics")
library("imager")
library("stringr")
library("openxlsx")
library("praise")

##DIRECTORY??

csvs <- list.files("hat funktioniert/ergebnisse", "*csv", full.names = T)
 
 for(i in 1:411){
   mask <- read.csv2(csvs[i], header = FALSE)

   plotMask <- function (image, mask, mask_rgb = c(244, 37, 37, 0.42), 
                      out = paste0("hat funktioniert/ergebnisse/predB.e",str_extract_all(csvs[i], "\\d+")[[1]][1], ".jpg.jpg")){
  
  mask <- data.frame(t(mask))
  colnames(mask) <- paste0("col", 1:ncol(mask))
  rownames(mask) <- paste0("row", 1:nrow(mask))
  
  mask <- data.frame(key = rownames(mask), mask)
  mask2 <- melt(mask)
  mask2$variable <- as.numeric(gsub("col", "", mask2$variable))
  mask2$key <- as.numeric(gsub("row", "", mask2$key))
  names(mask2) <- c("x", "y", "value")
  
  mask2_1 <- data.frame(mask2, cc = 1)
  mask2_1$value[mask2_1$value == 0] <- NA
  mask2_1$value[mask2_1$value == 1] <- mask_rgb[1]
  
  mask2_2 <- data.frame(mask2, cc = 2)
  mask2_2$value[mask2_2$value == 0] <- NA
  mask2_2$value[mask2_2$value == 1] <- mask_rgb[2]
  
  mask2_3 <- data.frame(mask2, cc = 3)
  mask2_3$value[mask2_3$value == 0] <- NA
  mask2_3$value[mask2_3$value == 1] <- mask_rgb[3]
  
  mask2_4 <- data.frame(mask2, cc = 4)
  mask2_4$value[mask2_4$value == 0] <- NA
  mask2_4$value[mask2_4$value == 1] <- mask_rgb[4]
  
  
  mask2 <- rbind(mask2_1, mask2_2, mask2_3, mask2_4)
  mask2 <- data.frame(mask2, z = 1)
  mask2 <- mask2[, c("x", "y", "z", "cc", "value")]
  
  mask3 <- as.cimg(mask2)
  
  if(length(out)>0){
    jpeg(out, width = width(im)*2+100, height = height(im)*2)
  }
  par(mfrow = c(1,2))
  plot.new()
  rasterImage(im, 0, 0, 1, 1)
  plot.new()
  title("Masked")
  rasterImage(im, 0, 0, 1, 1)
  rasterImage(mask3, 0, 0, 1, 1)
  if(length(out)>0){
    dev.off()
  }
  
}
  
 
im <- load.image(paste0("hat funktioniert/B.e",str_extract_all(csvs[i], "\\d+")[[1]][1], ".jpg"))

mask <- read.csv2(csvs[i], header = FALSE)

plotMask(im, mask, c(244, 37, 37, 0.9), 
         out =paste0("hat funktioniert/ergebnisse/predB.e",str_extract_all(csvs[i], "\\d+")[[1]][1], ".jpg.jpg"))

}

praise()


