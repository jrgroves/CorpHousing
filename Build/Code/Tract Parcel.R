#This file intersects the parcel map from 2021 with the three main census tract
#maps for the ACS#

#Jeremy R. Groves
#September 27, 2024

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Get ACS TIGER files for three census years and link to the 2021 Parcel map
    
    loc<-st_read("./Build/Data/2021/Parcels.shp")

    parmap <- loc %>%
      select(PARENT_LOC, LOCATOR, PROP_ADD, PROP_ZIP) %>%
      st_centroid()
    
    rm(loc)

    parcel <- parmap %>%
      st_drop_geometry() %>%
      mutate(GEOID.00 = NA,
             GEOID.10 = NA,
             GEOID.20 = NA)
    k<-5
    y<-c(2009, 2010, 2020)
    for(i in y){
      acs <- get_acs(geography = "tract",
                     variables = "C17002_001",
                     year = i,
                     state = 29,
                     county = 189,
                     geometry = TRUE) #Geometry different for 2009, 2010-2019, 2020 + thus the three maps below
      map <- acs %>%
        mutate(CENSUS_TRA = substr(GEOID, 6, 11),
               mapyear = as.character(i)) %>%
        select(GEOID, mapyear)
      
      parmap <- st_transform(parmap, st_crs(map)) #projects STL map to match CENSUS maps
    
      temp <- st_intersects(map, parmap)
      
      for(i in seq(1, length(temp))){
        parcel[,k] = replace(parcel[,k], temp[[i]], map$GEOID[i])
      }
      k<-k+1

    }
 
#Save the files
    save(parcel, file="./Build/Code/tract_parcel.RData")
    