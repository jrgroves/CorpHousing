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
  distinct(PARID, .keep_all = TRUE)

temp <- st_intersects(buffer.1, cen.loc)

parcel <- SALES %>%
  mutate(nn_lnadj_price = NA)

for(i in seq(1, length(temp))){
  
  t<-(temp[[1]])
  
  tt<-as.data.frame(t)
  tt$p <- NA
  for(i in seq(1, length(t))){
    c<-t[i]
    tt$p<-replace(tt$p, c, cen.loc$PARID[c])
  }
  
  
  
  
  
    parcel[,k] = replace(parcel[,k], temp[[i]], map$GEOID[i])
  }
  k<-k+1
  
