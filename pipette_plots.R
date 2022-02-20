library("plotwidgets")
library("here")
library("openxlsx")
library("tidyverse")

# sample data
hexdata<-tibble::tribble(~Hex,"#F5A477")                       
                        

# apply to col2hsl function to each hex
my_outp<-hexdata%>%mutate(HSL=map(Hex,col2hsl)) 

#[[1]][[1]]

# create an output with seperate HSL columns
my_outp<-my_outp%>%unnest_wider(HSL)
colnames(my_outp)<-c("Hex","H","S","L")

write.xlsx(my_outp,"data/ah.xlsx",asTable = F)

#show the output
my_outp
 



