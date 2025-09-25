#This will replicate the creation of the Wang 5 and 8 neighbors. While it is not necessarily adjacent, it calculates the 
#centroids and uses the 13 closest centroids.

#Jeremy Groves
#September 16, 2025

rm(list = ls())

library(spdep)
library(tidyverse)

#Load Data
load(file="./Build/Output/sal_own.RData")
loc<-st_read("F:/Data/Saint Louis GIS Data/gis_2025/Parcels_Current.shp")
load(file="./Build/Output/Own10.RData")
load(file="./Build/Input/neighbor.RData")

#filter out of OWN only what is needed
    own <- OWN %>%
      select(parid, co_zip, co_state, tenure, corporate, private, year) %>%
      mutate(other = ifelse(corporate == 0 & private == 0, 1, 0))
    #rm(OWN)

#Create Centroid map of main parcels
  loc.cen <- loc %>%
    st_centroid() %>%
    select(LOCATOR, PROPCLASS, LUC, LIVUNIT) %>%
    mutate(ID = row_number())
  
    c<- as.data.frame(st_coordinates(loc.cen))
  
  loc.cen2 <- st_drop_geometry(bind_cols(loc.cen, c)) %>%
    select(-c(LUC)) 

  rm(loc.cen, c, loc)
  
  buffer_1320 <- df %>% #df is the nested dataframe created by the buffer command
    #Join the centroid points to the base units using the LOCATOR.
    left_join(., loc.cen2, by = c("name" = "LOCATOR")) %>%
    rename("base.x" = "X",
           "base.y" = "Y") %>%
    select(c(-n, -PROPCLASS, -LIVUNIT)) %>%
    unnest(., value)  %>%
    #Next add the centroid points to the neighbors joining by the ID values
    left_join(., loc.cen2, by=c("value" = "ID")) %>%
    rename("Neighbor" = "LOCATOR") %>%
    select(-value) %>%
    #Calculate the distance between each point and its neighbors
    mutate(distance = sqrt((X - base.x)^2 + (Y - base.y)^2)) %>%
    #Remove own neighbors
    filter(distance > 0) %>%
    select(name, Neighbor, PROPCLASS, LIVUNIT, distance) %>%
    filter(LIVUNIT > 0 & PROPCLASS == "R")

#Merge with ownership information for neighbors
  wang <- core %>%
    filter(n > 1) %>% #Keep only repeated sales
    select(parid, po_livunit, yrblt, saleyr, new_con) %>%
    rename("name" = "parid") %>%
    left_join(., buffer_1320, by = "name", relationship = "many-to-many") %>%
    left_join(., own, by = c("Neighbor" = "parid", "saleyr" = "year")) %>%
    filter(!is.na(co_zip)) %>%
  #Find the 5 and 8 nearest neighbors to replicate the 5RP and 8RP measures
    arrange(name, saleyr, distance) %>%
    group_by(name, saleyr) %>%
    mutate(count = 1:n()) %>%
    ungroup() %>%
  #Remove object too far away
    filter(count < 14) %>%
    group_by(name, saleyr) %>%
    mutate(n = max(count)) %>%
    ungroup() %>%
  #Remove cases where there are less than five nearest neighbors
    filter(n > 4) %>%
  #Keep only what is going to be in the neighbor measures
    mutate(owner = ifelse(tenure == "OWNER", 1, 0),
           nonowner = ifelse(owner == 1, 0, 1)) %>%
    select(name, saleyr, new_con, LIVUNIT, owner, nonowner, corporate, private, other, count, n)
  
  
#Create the 5RP and 8RP measures as in Wang
 wang5 <- wang %>%
    filter(n > 4,
           count < 6) %>%
    select(-c(n, count))   %>%
    group_by(name, saleyr) %>%
    summarise(across(new_con : other, ~ mean(.x, na.rm=TRUE))) %>%
    mutate(sales = n()) %>%
    ungroup() %>%
    rename_with(~str_c("RP5_", .), new_con:other)
 
 wang8 <- wang %>%
   filter(count > 5,
          n == 13)  %>%  #This removes 944 cases related to 108 distinct observations
   select(-c(n, count))   %>%
   group_by(name, saleyr) %>%
   summarise(across(new_con : other, ~ mean(.x, na.rm=TRUE))) %>%
   mutate(sales = n()) %>%
   ungroup() %>%
   rename_with(~str_c("RP8_", .), new_con:other)


save(wang5, wang8, file = "./Build/Output/wang.RData")
