#This will replicate the creation of the Wang 5 and 8 neighbors. While it is not necessarily adjacent, it calculates the 
#centroids and uses the 13 closest centroids.

#Jeremy Groves
#September 16, 2025
#Updated: October 8, 2025: Fixed the neighbor errors and simplify program with new neighbor list. 

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
      select(parid, co_zip, co_state, tenure, corporate, private, year, class, po_livunit) %>%
      mutate(other = ifelse(corporate == 0 & private == 0, 1, 0),
             po_livunit = replace_na(po_livunit, 0))

#Create Centroid map of all parcels
  loc.cen <- loc %>%
    st_centroid() %>%
    select(LOCATOR, PROPCLASS, LUC, LIVUNIT) %>%
    rename("parid" = "LOCATOR") %>%
    mutate(X = st_coordinates(.)[,1],
           Y = st_coordinates(.)[,2]) %>%
    st_drop_geometry()

#Add base coordinates to sales data
  temp <- core %>%
    filter(n > 1) %>% #Limit to only those that sale more than once (for repeated sales)
    select(parid, saleyr) %>%
  #Join the centroid coordinates to the base parid
    left_join(., loc.cen, by = "parid") %>%
  #Rename and trim uneccessary data.
    rename("base.x" = "X",
           "base.y" = "Y")  %>%
    select(-c(PROPCLASS, LIVUNIT, LUC)) %>%
    filter(!is.na(base.x)) 
  
#Add neighbor coordinate data to neighbor data and find distances and sort
  core2 <- df %>%
    unnest(cols = c(value)) %>%
    rename("n.parid" = "value") %>%
    left_join(.,loc.cen, by = c("n.parid" = "parid"), relationship = "many-to-many") %>%
    filter(PROPCLASS == "R") %>%
  #Join with the base data from above
    right_join(temp, ., by = c("parid" = "base.parid"), relationship = "many-to-many") %>%
  #Calculate the distance between each point and its neighbors
    mutate(distance = sqrt((X - base.x)^2 + (Y - base.y)^2)) %>%
  #Remove own neighbors
    filter(distance > 0) %>%
  #Sort by distance for each parcel
    group_by(parid, saleyr) %>%
    arrange(parid, saleyr, distance) %>%
    mutate(closeness = seq(n())) %>%
    ungroup() %>%
    select(-c(base.x, base.y, X, Y, LUC, PROPCLASS, n))
  
#limit to the 5 nearest and the 13 nearest  

W5RP <- core2 %>%
  filter(closeness < 6) 

W8RP <- core2 %>%
  filter(closeness < 14 & closeness > 5)  

  
#Merge with ownership information for neighbors
  temp <- W5RP %>%
    left_join(., own, by = c("n.parid" = "parid", "saleyr" = "year"), relationship = "many-to-many") %>%
  #Turn Text into Dummies for averaging
    mutate(ten.own = case_when(tenure == "OWNER" ~ 1,
                               TRUE ~ 0),
           ten.nown = 1 - ten.own) %>%
    select(parid, saleyr, distance, ten.own, ten.nown, corporate, private, other, po_livunit) %>%
    group_by(parid, saleyr) %>%
    summarise(across(distance:po_livunit, ~ mean(.x, na.rm=TRUE))) %>%
    ungroup() %>%
    filter(!is.na(private)) %>%
    rename_with(~str_c("RP5_", .), distance:po_livunit) 
  
  core <- core %>%
    left_join(., temp, by = c("parid", "saleyr"))
 
  temp <- W8RP %>%
    left_join(., own, by = c("n.parid" = "parid", "saleyr" = "year"), relationship = "many-to-many") %>%
    #Turn Text into Dummies for averaging
    mutate(ten.own = case_when(tenure == "OWNER" ~ 1,
                               TRUE ~ 0),
           ten.nown = 1 - ten.own) %>%
    select(parid, saleyr, distance, ten.own, ten.nown, corporate, private, other, po_livunit) %>%
    group_by(parid, saleyr) %>%
    summarise(across(distance:po_livunit, ~ mean(.x, na.rm=TRUE))) %>%
    ungroup() %>%
    filter(!is.na(private)) %>%
    rename_with(~str_c("RP8_", .), distance:po_livunit) 
 
  core <- core %>%
    left_join(., temp, by = c("parid", "saleyr")) %>%
    filter(n > 1)

save(core, file = "./Build/Output/wang.RData")
