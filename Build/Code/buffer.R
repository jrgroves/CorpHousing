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
loc<-st_read("./Build/Data/2021/Parcels.shp")

#Find unique PARIDs for sold properties

  sold <- SALES %>%
    select(PARID, saleyear) %>%
    distinct(PARID, .keep_all = TRUE)

#Convert parcel map to centroid map
  
  cen.loc <- loc %>%
    select(PARENT_LOC, LOCATOR) %>%
    st_centroid() %>%
    mutate(PARID = LOCATOR) %>%
    distinct(PARID, .keep_all = TRUE)

#Create Buffer Maps
  buffer.1 <- cen.loc %>%
    filter(PARID %in% sold$PARID) %>%
    st_buffer(., 1320) %>% #1/4 mile buffer
    distinct(PARID, .keep_all = TRUE)
  
  save(buffer.1, file="./Build/Output/Buffer1_4.RData")
  
  buffer.2 <- cen.loc %>%
    filter(PARID %in% sold$PARID) %>%
    st_buffer(., 2640) %>% #1/2 mile buffer
    distinct(PARID, .keep_all = TRUE)
  
  save(buffer.2, file="./Build/Output/Buffer1_2.RData")
  
  buffer.3 <- cen.loc %>%
    filter(PARID %in% sold$PARID) %>%
    st_buffer(., 3960) %>% #1/2 mile buffer
    distinct(PARID, .keep_all = TRUE)
  
  save(buffer.3, file="./Build/Output/Buffer3_4.RData")
 