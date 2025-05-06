#This script creates 1/4, 1/2, and 3/4 mile buffers around each of the properties in the sold data frame 
#using the 2021 parcel map from STL based on centroids created by the *sf* package. I am still working 
#on the intersection part to determine which properties are located within these buffers. Each is saved 
#as an .RData file in the Build/Output folder.

#Jeremy R. Groves
#September 18, 2024

#April 14, 2025: Updated with new 2025 data.

rm(list=ls())

library(tidyverse)
library(sf)

load(file="./Build/Output/Sales.RData")
loc<-st_read("F:/Data/Saint Louis GIS Data/gis_2025/Parcels_Current.shp")

#Find unique PARIDs for sold properties

  sold <- sales %>%
    select(PARID, saleyr) %>%
    distinct(PARID, .keep_all = TRUE)

#Convert parcel map to centroid map
  
  cen.loc <- loc %>%
    select(PARENT_LOC, LOCATOR) %>%
    st_centroid() %>%
    mutate(PARID = LOCATOR) %>%
    distinct(PARID, .keep_all = TRUE)

#Create Buffer Maps
  buffer <- cen.loc %>%
    filter(PARID %in% sold$PARID) %>%
    st_buffer(., 1320) %>% #1/4 mile buffer
    distinct(PARID, .keep_all = TRUE)

  save(buffer, file="./Build/Output/Buffer1_4.RData")
  
  buffer <- cen.loc %>%
    filter(PARID %in% sold$PARID) %>%
    st_buffer(., 2640) %>% #1/2 mile buffer
    distinct(PARID, .keep_all = TRUE)
  
  save(buffer, file="./Build/Output/Buffer1_2.RData")
  
  buffer <- cen.loc %>%
    filter(PARID %in% sold$PARID) %>%
    st_buffer(., 3960) %>% #1/2 mile buffer
    distinct(PARID, .keep_all = TRUE)
  
  save(buffer, file="./Build/Output/Buffer3_4.RData")
 