#Pulls ACS data
#This file needs the Owner Filter.R file to run first.


#Jeremy R. Groves
#June 20, 2024

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Set starting parameters####
  y <- seq(2009,2020)
  
  breaks <- seq(0, 1, by = 0.1)
  cols <- RColorBrewer::brewer.pal(11, "Spectral")
  var<-c("C17002_001","C17002_002","C17002_003","C17002_004","C17002_005","C17002_006","C17002_007",
         "C17002_008")

    #These correspond to the following Ratio of Income to Poverty Level over 12 months:
    #"Total", "under .50", ".5 to .99", "1 to 1.24", "1.25 to 1.49", "1.5 to 1.84", "1.85 to 1.99", "2 and over"


#Get ACS data from Census####
    for(i in y){
    acs <- get_acs(geography = "tract",
                         variables = var,
                         year = i,
                         state = 29,
                         county = 189,
                         geometry = FALSE) #Geometry different for 2009, 2010-2019, 2020 + thus the three maps below
    acs <- acs %>%
      pivot_wider(., id_cols = "GEOID", names_from = "variable", values_from = "estimate") %>%
      mutate(across(C17002_002:C17002_008, ~ .x / C17002_001),
             CENSUS_TRA = substr(GEOID, 6, 11),
             year = i) 
    
    ifelse(i==2009, 
           ACS <- acs, 
           ACS <- rbind(ACS, acs))
    }
    
    ACS <- ACS %>%
      mutate(B2 = 1 - C17002_008, #This creates a total under 2 times income/poverty level ratio
             mapyear = case_when(year<2010 ~ "2009",
                                 year>2009 & year<2020 ~ "2010",
                                 year>2019 ~ "2020",
                                 TRUE~"0"))
      

#Get ACS TIGER files for three census years and link to the 2021 Parcel map
    
    loc<-st_read("./Build/Data/2021/Parcels.shp")

    parmap <- loc %>%
      select(PARENT_LOC, LOCATOR, PROP_ADD, PROP_ZIP) %>%
      st_centroid()
    
    rm(loc)
    parmap <- st_transform(parmap, st_crs(MAP2)) #projects STL map to match CENSUS maps
    
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
 
    
    
       
    cen.map <- MAP
      rm(MAP)
  
    save(cen.map, file="./Build/Output/CenMap.RData")

    
#Create a MAP of Percent below 2X Poverty Level for 2009, 2010, and 2020 as test.#####
  MAP2 <- MAP %>%
    right_join(., ACS, by=c("GEOID", "mapyear")) %>%
    select(GEOID, mapyear, B2, geometry)
  
  ggplot(MAP2) +
    geom_sf(aes(fill = B2)) +
    scale_fill_stepsn(colors = cols,breaks = breaks, name = "Percent Below 2 times Poverty Level") +
    facet_wrap( ~mapyear)

