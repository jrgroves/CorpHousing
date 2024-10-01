#This script creates 1/4, 1/2, and 3/4 mile buffers around each of the properties in the sold data frame 
#using the 2021 parcel map from STL based on centroids created by the *sf* package. I am still working 
#on the intersection part to determine which properties are located within these buffers. Each is saved 
#as an .RData file in the Build/Output folder.

#Jeremy R. Groves
#September 18, 2024

rm(list=ls())

library(tidyverse)
library(sf)

load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/Buffer1_4.RData")
loc<-st_read("./Build/Data/2021/Parcels.shp")

#Convert parcel map to centroid map

cen.loc <- loc %>%
  select(PARENT_LOC, LOCATOR) %>%
  st_centroid() %>%
  mutate(PARID = LOCATOR) %>%
  distinct(PARID, .keep_all = TRUE) %>%
  filter(!is.na(PARID)) %>%
  rowid_to_column(., "ROW")

temp <- st_intersects(buffer.1, cen.loc)

for(i in seq(1,length(temp))){
  
  c<-unlist(temp1[i])
  temp.list <- list(cen.loc$PARID[which(cen.loc$ROW %in% c)])
  names(temp.list) <- buffer.1$PARID[i]
  ifelse(i==1, buff.list <- temp.list, buff.list <- c(buff.list, temp.list))
  
}

save(buff.list, file="./Build/output/bufflist1_4.RData")
