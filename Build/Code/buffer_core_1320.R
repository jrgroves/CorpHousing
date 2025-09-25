#This script creates one-quarter mile buffers around each of the properties in the sold data frame 
#using the 2025 parcel map from STL based on centroids created by the *sf* package. It then merges with the map
#shapefile to determine non-residential uses and then with OWN data frame for ownership of residential parcels.
#The data is aggregated and then saved as core1032 to denote the one-quarter mile. This file is a modified version of
#the original buffer_create that uses a much faster way to aggregate the neighbors from the intersects() command.

#Jeremy R. Groves
#July 9, 2025

rm(list=ls())

library(tidyverse)
library(sf)

#Load Data
  load(file="./Build/Output/sal_own.RData")
  loc<-st_read("F:/Data/Saint Louis GIS Data/gis_2025/Parcels_Current.shp")
  load(file="./Build/Output/Own10.RData")
  
  #filter out of OWN only what is needed
  own <- OWN %>%
    select(parid, po_livunit, po_zip, co_zip, co_state, tenure, corporate:year)
  rm(OWN)
  
  #Create Centroid map of main parcels
  loc.cen <- loc %>%
    st_centroid() %>%
    select(LOCATOR, PROPCLASS, LUC, LIVUNIT) %>%
    mutate(ID = row_number())
    

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
    df <- enframe(buffer) %>%
      mutate(n = lengths(value))
    
    save(df, file = "./Build/Input/neighbor.RData")
    
  #Now unnest the tibble and create a data frame with the base observation from the core data and all of its neighbors
    buffer_1320 <- df %>%
      unnest(., value)  %>%
      left_join(., loc.cen, by=c("value" = "ID")) %>%
      st_drop_geometry() %>%
      select(-value, -geometry, -LUC) %>%
      rename("base.parid" = "name",
             "neigh.parid" = "LOCATOR") %>%
      filter(!is.na(PROPCLASS)) #This drops about 30K cases which related to only about 381 parcel ids that are mostly
                                #in error, have the prefix PL or OL which do not show up in records, or 2025 properties.
  #Housekeeping
    rm(df, buffer)
    gc()
    
#Combine the buffer list with the year of sale so we can get the right ownership information for the neighbors
    working <- core %>%
      select(parid, saleyr) %>%
      left_join(., buffer_1320, by=c("parid" = "base.parid"), relationship = "many-to-many") %>%
      filter(!is.na(PROPCLASS)) %>% #Removes 20 cases; likely same as above in core with no match on map.
      left_join(., own, by=c("neigh.parid" = "parid", "saleyr" = "year"))  %>%
      mutate(
        po_livunit = replace_na(po_livunit, 0),
        nonres = ifelse(is.na(corporate), 1, 0), #This creates a value for the nonresidential properties in neighbors
        across(.cols = corporate:key,
               ~ifelse(is.na(.), 0, .)),
        nonzip = ifelse(po_zip != co_zip, 1, 0),
        nonzip = ifelse(is.na(nonzip), 0, nonzip),
        owner = ifelse(tenure == "OWNER", 1, 0),
        owner = ifelse(is.na(owner), 0, owner),
        nonowner = ifelse(owner == 1, 0, 1),
        prop_agg = ifelse(PROPCLASS == "A", 1, 0),
        prop_com = case_when(PROPCLASS == "C" ~ 1,
                             PROPCLASS == "Y" ~ 1,
                             TRUE ~ 0),
        prop_res = ifelse(PROPCLASS == "R", 1, 0),
        prop_multi = case_when(PROPCLASS == "W" ~ 1,
                               PROPCLASS == "X" ~ 1,
                               PROPCLASS == "Z" ~ 1,
                               TRUE ~ 0)) %>%
      select(-c(tenure, co_state, co_zip, po_zip, po_livunit, key, PROPCLASS, neigh.parid)) %>% 
      group_by(parid, saleyr) %>%
      summarise(across(n:prop_multi, mean)) %>%
      ungroup() %>%
      rename("neighbors" = "n",
             "nb_livunit" = "LIVUNIT") %>%
      rename_with(~str_c("nb_", .), corporate:prop_multi)
 
#Join to the core data and save as a new version of core
  core2 <- core %>%
    left_join(., working, by=c("parid", "saleyr")) %>%
    filter(!is.na(neighbors))

  save(core2, file="./Build/Output/core_1320.RData")

    
    