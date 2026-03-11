#This script creates one-quarter mile buffers around each of the properties in the sold data frame 
#using the 2025 parcel map from STL based on centroids created by the *sf* package. It then merges with the map
#shapefile to determine non-residential uses and then with OWN data frame for ownership of residential parcels.
#The data is aggregated and then saved as core660 to denote the one-eighth mile. This file is a modified version of
#the original buffer_create that uses a much faster way to aggregate the neighbors from the intersects() command.

#Jeremy R. Groves
#July 9, 2025
#October 15, 2025: Updated with corrected buffer and recalculated neighboring data.

rm(list=ls())

library(tidyverse)
library(sf)

#Load Data
  load(file="./Build/Output/sal_own.RData")
  loc<-st_read("F:/Data/Saint Louis GIS Data/gis_2025/Parcels_Current.shp")

#Create Centroid map of main parcels
  loc.cen <- loc %>%
    st_centroid() %>%
    select(LOCATOR, PROPCLASS, LUC, LIVUNIT) 

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
    st_buffer(., 660) 

  buffer2 <- loc.cen %>%
    st_join(., buffer) %>%
    filter(!is.na(ID)) %>%
    st_drop_geometry() %>%
    rename("base.parid" = "parid",
           "neigh.parid" = "LOCATOR") %>%
    filter(!is.na(PROPCLASS)) 

#Housekeeping
  rm(buffer, working, loc, loc.cen)
  gc()
  
  load(file="./Build/Output/Own10.RData")
  
  #filter out of OWN only what is needed
  own <- OWN %>%
    select(parid, po_livunit, po_zip, co_zip, co_state, tenure, corporate:other)
  rm(OWN)
    
  working <- core %>%
    select(parid, saleyr)%>%
    left_join(., buffer2, by=c("parid" = "base.parid"), relationship = "many-to-many") %>%
    filter(!is.na(PROPCLASS))  #Removes 20 cases; likely same as above in core with no match on map.
  #rm(buffer2) 
  
  gc()  
 
  #Join and create count of neighbors 
  working <- working %>% 
    left_join(., own, by=c("neigh.parid" = "parid", "saleyr" = "year")) %>%
    filter(!is.na(trustee)) 
  
  neighbors <- working %>%
    select(parid, saleyr) %>%
    mutate(neighbors = 1) %>%
    summarise(neighbors = sum(neighbors), .by = c(parid, saleyr)) %>%
    distinct()
   
  working <- working %>%   
    mutate(
      po_livunit = replace_na(po_livunit, 0),
      nonres = ifelse(is.na(corporate), 1, 0), #This creates a value for the nonresidential properties in neighbors
      across(.cols = corporate:key,
             ~ifelse(is.na(.), 0, .)),
      nonzip = ifelse(po_zip != co_zip, 1, 0),
      nonzip = ifelse(is.na(nonzip), 0, nonzip),
      owner = ifelse(tenure == "OWNER", 1, 0),
      owner = ifelse(is.na(owner), 0, owner),
      prop_agg = ifelse(PROPCLASS == "A", 1, 0),
      prop_com = case_when(PROPCLASS == "C" ~ 1,
                           PROPCLASS == "Y" ~ 1,
                           TRUE ~ 0),
      prop_res = ifelse(PROPCLASS == "R", 1, 0),
      prop_multi = case_when(PROPCLASS == "W" ~ 1,
                             PROPCLASS == "X" ~ 1,
                             PROPCLASS == "Z" ~ 1,
                             TRUE ~ 0)) %>%
    select(-c(tenure, co_state, co_zip, po_zip, po_livunit, key, PROPCLASS, neigh.parid, LUC, ID))  %>%
    summarise(across(LIVUNIT:prop_multi, mean), .by = c(parid, saleyr)) %>%
    rename_with(~str_c("nb_", .), LIVUNIT:prop_multi)
  rm(own)
  
 #Join to the core data and save as a new version of core
  core2 <- core %>%
    left_join(., working, by=c("parid", "saleyr")) %>%
    left_join(., neighbors, by = c("parid", "saleyr")) %>%
    filter(!is.na(neighbors),
           !is.na(nb_other),
           !is.na(post_other),
           !is.na(po_livunit)) %>%
    distinct()
  
  save(core2, file="./Build/Output/core_660.RData")
  
    