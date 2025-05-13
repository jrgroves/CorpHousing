#This script creates 1/4, 1/2, and 3/4 mile buffers around each of the properties in the sold data frame 
#using the 2021 parcel map from STL based on centroids created by the *sf* package. Each is saved 
#as an .RData file in the Build/Output folder.

#Jeremy R. Groves
#September 18, 2024

rm(list=ls())

library(tidyverse)
library(sf)

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
