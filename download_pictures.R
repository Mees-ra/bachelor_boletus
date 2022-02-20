## Download images for Amanita muscaria

library("openxlsx")
library("stringr")
library("foreach")
library("here")

## Read iNaturalist dataset (record meta information including image URL)
fds <- read.xlsx("C:/Users/insert user name/Bachelor/Regions/Boletus edulis.csv/boletus edulis.xlsx")

## Delete noisy data rows
fds$id <- str_extract(fds$id, "\\d+")
fds <- fds[!is.na(fds$id), ]

## Download
foreach(i = 1:nrow(fds)) %do% {
  print(i)
  urls <- str_split(fds$image_url[i],"/")
  download.file(fds$image_url[i],  
                destfile = paste0("B.e", str_extract_all(urls, "\\d+")[[1]][1], ".jpg"), 
                mode = "wb",quiet = T)
  
}
