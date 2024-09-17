#Processes Sales Data

#Jeremy R. Groves
#June 27, 2024

rm(list=ls())

library(tidyverse)
library(sf)

loc<-st_read("~/Corp/Build/Data/2021/Parcels.shp") #Parcel Shapefile
load(file="~/Build/Output/owners.RData")
load(file="~/Build/Output/Sales.RData")

#Create Buffer with full parcel database

center <- loc %>%
  select(LOCATOR, NBHD) %>%
  st_centroid(.) 

buffer <- center %>%
  st_buffer(., 1320) #1/4 mile buffer


#Pull in and process the owner data for each year

for(i in seq(2001,2001)){
  
  own<-get(paste0("own_dat",i))

  temp1 <- center %>%
    right_join(., own, by="LOCATOR") %>%
    mutate(owner = case_when(TENURE == "OWNER" ~ 1,
                             TRUE ~ 0),
           nonowner = 1 - owner) 
    
  
  #Limit to only parcels present in both datasets####
    buffer2 <- buffer %>%
      filter(LOCATOR %in% temp1$LOCATOR)
    
    tempid <- temp1 %>%
      filter(LOCATOR %in% buffer2$LOCATOR) %>%
      select(LOCATOR, NBHD)
      
    temp2 <- temp1 %>%
      filter(LOCATOR %in% buffer2$LOCATOR) %>%
      select(LOCATOR, LIVUNIT, Corporate, Trustee, Bank, Muni, Nonprof, Hoa, private, key, owner, nonowner)

  #Aggregate units within buffers using the aggregate inside of sf
    
    temp3<-aggregate(temp2,buffer2, mean, na.action = na.omit) 
    
    temp4 <- temp2 %>%
      cbind(tempid, .)

    assign(paste0("own_buf1",i), temp4)    
}   

rm(i, buffer, buffer2, loc, own, temp1, temp2, temp3, temp4, tempid)
rm(list = ls(pattern = "^own_dat"))

save.image(file="./Build/Output/owners.RData")
