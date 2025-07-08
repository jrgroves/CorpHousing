#This script creates 1/4, 1/2, and 3/4 mile buffers around each of the properties in the sold data frame 
#using the 2021 parcel map from STL based on centroids created by the *sf* package. Each is saved 
#as an .RData file in the Build/Output folder.

#Jeremy R. Groves
#September 18, 2024

rm(list=ls())

library(tidyverse)
library(sf)

#Load Data
  load(file="./Build/Output/sal_own.RData")
  loc<-st_read("F:/Data/Saint Louis GIS Data/gis_2025/Parcels_Current.shp")
  
  #Create Centroid map of main parcels
  loc.cen <- loc %>%
    st_centroid() %>%
    select(LOCATOR, geometry)

#Filter our the parcels that are in the sales data and pull those from the map data
  working <- core %>%
    select(parid) %>%
    distinct() %>%
    left_join(., loc, by=c("parid" = "LOCATOR")) %>%
    filter(!is.na(LIVUNIT)) %>% #Removes 13 cases from the core data
    select(parid, geometry) %>%
    mutate(ID = row_number())
  
  buffer <- working %>%
    st_as_sf(., sf_column_name = "geometry") %>%
    st_make_valid() %>%
    st_centroid() %>%
    st_buffer(., 1320) %>%
    st_intersects(., loc.cen)
  #Change the names in the list with the parcel ids
    names(buffer) <- working$parid
  #Turn the list to a nested tibble
    df <- enframe(buffer)
  #Now unnest the tibble and create a data frame with the base observation from the core data and all of its neighbors
    buffer_1320 <- df %>%
      unnest(., value) %>%
      left_join(., working, by=c("value" = "ID")) %>%
      st_drop_geometry() %>%
      select(-value, -geometry) %>%
      rename("base.parid" = "name",
             "neigh.parid" = "parid")
  #Housekeeping
    rm(df, buffer)
    
#Combine the buffer list with the year of sale so we can get the right ownership information for the neighbors
    working <- core %>%
      select(parid, saleyr) %>%
      left_join(., buffer_1320, by=c("parid" = "base.parid"), relationship = "many-to-many")
    
  
###THIS IS WHERE YOU STOPPED SEE NOTES####



#load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/Buffer1_2.RData")
loc<-st_read("F:/Data/Saint Louis GIS Data/gis_2025/Parcels_Current.shp")

#Convert parcel map to centroid map

cen.loc <- loc %>%
  select(PARENT_LOC, LOCATOR) %>%
  st_centroid() %>%
  mutate(PARID = LOCATOR) %>%
  distinct(PARID, .keep_all = TRUE) %>%
  filter(!is.na(PARID)) %>% 
  rowid_to_column(., "ROW") %>%
  select(-c(PARENT_LOC, LOCATOR))

temp <- st_intersects(buffer, cen.loc)


loc.parid <- cen.loc %>%
  st_drop_geometry()
rm(cen.loc)

m<-length(temp)
  
  st <-seq(1, length(temp), 25000)
  en <- st - 1
    en <- en[2:length(en)]
    en[length(en)+1] <- length(temp)
  

for(k in seq(1,length(st),1)){
  j = 1
  for(i in seq(st[k],en[k])){
    c<-as_tibble(unlist(temp[i]))
    temp1 <- loc.parid %>%
      filter(ROW %in% c$value) %>%
      select(-ROW) %>%
      mutate(base.parid = buffer$PARENT_LOC[i])
    ifelse(j==1, buffer.list <- temp1, buffer.list <- rbind(buffer.list, temp1))
    j=j+1
  }
  assign(paste0("buffer.list",k), buffer.list)
}
  
gc()
rm(buffer, en, loc, loc.parid, m, st, temp, temp1, j, c, k, i, buffer.list)

save.image(file="./Build/output/bufflist1_2.RData")
