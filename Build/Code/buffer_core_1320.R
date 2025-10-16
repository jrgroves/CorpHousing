#This script creates one-quarter mile buffers around each of the properties in the sold data frame 
#using the 2025 parcel map from STL based on centroids created by the *sf* package. It then merges with the map
#shapefile to determine non-residential uses and then with OWN data frame for ownership of residential parcels.
#The data is aggregated and then saved as core1032 to denote the one-quarter mile. This file is a modified version of
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
  load(file="./Build/Output/Own10.RData")
  
  #filter out of OWN only what is needed
  own <- OWN %>%
    select(parid, po_livunit, po_zip, co_zip, co_state, tenure, class, corporate:year)
  rm(OWN)
  
  #Create Centroid map of main parcels
  loc.cen <- loc %>%
    st_centroid() %>%
    select(LOCATOR, PROPCLASS, LUC, LIVUNIT) 
  
  loc.cen2 <- loc.cen %>%
    st_drop_geometry()
  
#Filter our the parcels that are in the sales data and pull those from the map data
  working <- core %>%
    select(parid) %>%
    distinct() %>%
    left_join(., loc, by=c("parid" = "LOCATOR")) %>%
    filter(!is.na(LIVUNIT)) %>% #Removes 13 cases from the core data
    select(parid, geometry)

  buffer <- working %>%
    st_as_sf(., sf_column_name = "geometry") %>%
    st_make_valid() %>%
    st_centroid() %>%
    st_buffer(., 1320)
  
  buffer1320 <- buffer %>%
    st_contains(., loc.cen)
  
  #Change the names in the list with the parcel ids
    names(buffer1320) <- buffer$parid
   
  #Turn sparse matrix to nested tibble and then unnest
    temp<-enframe(buffer1320) %>%
      unnest(cols = c(value))
    
  #Replace position values from st_contains with parids and rename
    temp$value <- loc.cen2[temp$value,1]
    names(temp) <- c("base.parid", "n.parid")
   
  #Count neighbors and re-nest data for save. 
    df <- temp %>%
      group_by(base.parid) %>%
      summarise(value = list(n.parid)) %>%
      ungroup() %>%
      mutate(n = lengths(value))

    save(df, file = "./Build/Input/neighbor.RData")

    #Do not run below if you want to test the buffer
    rm(buffer, buffer1320, loc.cen, loc.cen2, loc)
    gc()
    
#Combine the buffer list with the year of sale so we can get the right ownership information for the neighbors
    working <- core %>%
      as_tibble() %>%
      left_join(., df, by = c("parid" = "base.parid"), relationship = "many-to-many") %>% #Joins the core sales data with the base parid of df
      select(parid, saleyr, value) %>%
      unnest(cols = c(value)) %>%
      rename(n.parid = value) %>%
      left_join(., own, by = c("n.parid" = "parid", "saleyr" = "year"), relationship = "many-to-many") %>%
      mutate(
        po_livunit = replace_na(po_livunit, 0),
        across(.cols = corporate:key,
               ~ifelse(is.na(.), 0, .)),
        nonzip = ifelse(po_zip != co_zip, 1, 0),
        nonzip = ifelse(is.na(nonzip), 0, nonzip),
        owner = ifelse(tenure == "OWNER", 1, 0),
        owner = ifelse(is.na(owner), 0, owner),
        nonowner = ifelse(owner == 1, 0, 1),
        prop_agg = case_when(class == "A" ~ 1,
                             TRUE ~ 0),
        prop_com = case_when(class == "C" ~ 1,
                             class == "Y" ~ 1,
                             TRUE ~ 0),
        prop_res = case_when(class == "R" ~ 1,
                             TRUE ~ 0),
        prop_multi = case_when(class == "W" ~ 1,
                               class == "X" ~ 1,
                               class == "Z" ~ 1,
                               TRUE ~ 0)) %>%
      select(-c(tenure, co_state, co_zip, po_zip, key, class, n.parid)) %>% 
      group_by(parid, saleyr) %>%
      summarise(across(po_livunit:prop_multi, mean)) %>%
      ungroup() %>%
      rename_with(~str_c("nb_", .), po_livunit:prop_multi)
 
#Join to the core data and save as a new version of core
  core2 <- core %>%
    left_join(., working, by=c("parid", "saleyr"))%>%
    filter(!is.na(nb_po_livunit)) #removes 20 units

  save(core2, file="./Build/Output/core_1320.RData")

  
    