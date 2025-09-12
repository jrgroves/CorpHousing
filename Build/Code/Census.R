#Pulls ACS data using the ACS API. Right now only contains data from 2010 to 2023 due to ACS limitations
#for data at the tract level. Then we use the xcoord and ycoord in the assessors data to intersect the OWN file
#with the census tract maps and merge the census data with the owners data to create a core_own dataset.

#This file needs the Owner Filter 2010.R file to run first.
#Outputs three files: census.RData, cen_own.RData, and core_cen.RData

#Jeremy R. Groves
#June 20, 2024
#Updated: May 9, 2025
#Updated: June 18, 2025: In this update I utilize the OWN file (specifically the one containing only 2010 to 2024) to intersect
#                        the census tiger maps for the geocodes for the tracts. I use the x and y coords included in the data from
#                        the assessor to avoid a problem with different parids between the GIS and the assessor files.
#Updated: July 11, 2025: Simplified the loop for API data and ensured match to all ownership data from OWN (cen_own) 
#                        and also merged with sales data to create the cen_core file

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Set starting parameters####
y <- seq(2010,2023)
vars<-c("B01003","B02001","B01002","B19013","B15002","B17023","B25010","B25008","S0101")

  #This recycles the years to solve the unequal length problem in map2()
  inputs <- data.frame(rep(vars, length(y)), y) %>%
    arrange(y)
    names(inputs) <- c("vars", "y")

#Get ACS data from Census####

    multi_year <- map2(
      inputs$y,inputs$vars,
      ~ get_acs(
        geography = "tract",
        table = .y,
        state = 29,
        county = 189,
        year = .x,
        geometry = FALSE
      )
    ) 

  names(multi_year) <- inputs$y
  ACS <- unnest(enframe(multi_year), value) %>%
    select(name, GEOID, variable, estimate) %>%
    rename("year" = "name")

#Reformatting and naming the Census data

census <- ACS %>%
  mutate(variable = case_when(variable == "B01003_001" ~ "pop",
                              variable == "B02001_002" ~ "white",
                              variable == "B02001_003" ~ "black",
                              variable == "B02001_004" ~ "asian",
                              variable == "B01002_001" ~ "age",
                              variable == "B19013_001" ~ "income",
                              variable == "B15002_011" ~ "m_hs",
                              variable == "B15002_012" ~ "m_sc1",
                              variable == "B15002_013" ~ "m_sc2",
                              variable == "B15002_014" ~ "m_asc",
                              variable == "B15002_015" ~ "m_bac",
                              variable == "B15002_016" ~ "m_mas",
                              variable == "B15002_017" ~ "m_pro",
                              variable == "B15002_018" ~ "m_doc",
                              variable == "B15002_028" ~ "f_hs",
                              variable == "B15002_029" ~ "f_sc1",
                              variable == "B15002_030" ~ "f_sc2",
                              variable == "B15002_031" ~ "f_asc",
                              variable == "B15002_032" ~ "f_bac",
                              variable == "B15002_033" ~ "f_mas",
                              variable == "B15002_034" ~ "f_pro",
                              variable == "B15002_035" ~ "f_doc",
                              variable == "B17023_002" ~ "pov1",
                              variable == "B17023_003" ~ "pov2",
                              variable == "B17023_004" ~ "pov3",
                              variable == "B17023_005" ~ "pov4",
                              variable == "B17023_006" ~ "pov5",
                              variable == "B17023_007" ~ "pov6",
                              variable == "B17023_008" ~ "pov7",
                              variable == "B17023_009" ~ "pov8",
                              variable == "B17023_010" ~ "pov9",
                              variable == "B17023_011" ~ "pov10",
                              variable == "B17023_012" ~ "pov11",
                              variable == "B17023_013" ~ "pov12",
                              variable == "B25008_002" ~ "own",
                              variable == "B25008_003" ~ "rent",
                              variable == "B25010_002" ~ "own_sz",
                              variable == "B25010_003" ~ "rent_sz",
                              variable == "S0101_C02_022" ~ "m_16u",
                              variable == "S0101_C03_022" ~ "f_16u",
                              variable == "S0101_C02_030" ~ "m_65o",
                              variable == "S0101_C03_030" ~ "f_65o",
                              TRUE ~ "delete")) %>%
  filter(variable != "delete",
         !is.na(estimate)) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(per_own = own / (own + rent),
         per_wht = white / pop,
         per_blk = black / pop,
         per_asn = asian / pop,
         per_oth = 1 - per_wht - per_blk - per_asn,
         per_u16 = (m_16u + f_16u) / pop,
         per_o65 = (m_65o + f_65o) / pop,
         per_hs = (m_hs + f_hs) / pop,
         per_scol = (m_sc1 + m_sc2 + f_sc1 + f_sc2) / pop,
         per_asdg = (m_asc + f_asc) / pop,
         per_bach = (m_bac + f_bac) / pop,
         per_advdg = (m_mas + m_pro + m_doc + f_mas + f_pro + f_doc) / pop,
         per_pov1 = (pov1 + pov2 + pov3) / pop,   #percent below 1 times the poverty line
         per_pov2 = (pov4 + pov5 + pov6 + pov7 + pov8) / pop, #percent between 1 times and below 2 times poverty
         per_pov3 = pov9 / pop) %>%  #percent between 2 and below 3 times poverty.
  select(GEOID, starts_with("per"), income, age, own_sz, rent_sz, year) %>%
  mutate(cen_yr = case_when(year < 2020 ~ 2010,
                            TRUE ~ 2020),
         year = as.numeric(year))

save(census, file="./Build/Output/census.RData")
  rm(ACS, multi_year, inputs, y, vars)

#Get ACS TIGER files for three census years and link to the OWN Data ####
    
  load("./Build/Output/Own10.RData")    
  
  own1 <- OWN %>%
    select(parid, xcoord, ycoord, year) %>%
    arrange(parid, year) %>%
    group_by(parid) %>%
    mutate(max.yr = max(year),
           xcoord = case_when(year < max.yr ~ NA,
                              TRUE ~ xcoord),
           ycoord = case_when(year < max.yr ~ NA,
                              TRUE ~ ycoord)) %>%
    fill(xcoord, .direction = "downup") %>%
    fill(ycoord, .direction = "downup") %>%
    ungroup() 
  
  own <- own1 %>%
    distinct(parid, .keep_all = T) %>%
    filter(!is.na(xcoord),
           !is.na(ycoord)) %>%
    st_as_sf(coords = c("xcoord", "ycoord"), crs = 102696, remove = T) %>%
    st_transform(crs = 4326)
  
  
  y<-c(2010, 2020) #Removed 2009 since not using data pre-2010
    for(i in y){
      acs <- get_acs(geography = "tract",
                     variables = "C17002_001",
                     year = i,
                     state = 29,
                     county = 189,
                     geometry = TRUE) #Geometry different for 2009, 2010-2019, 2020 + thus the three maps below
      
      map <- acs %>%
        mutate(map.id = row_number()) %>%
        select(map.id, GEOID) %>%
        st_make_valid()
      
        own <- st_transform(own, st_crs(map)) #projects STL map to match CENSUS maps
      
        #Intersect the ownership points with the census tract map
          temp <- st_intersects(own, map)
      
          names(temp) <- own$parid
          df <- enframe(temp)
          
      temp <- unnest(df, value) %>%
        left_join(., map, by=c("value" = "map.id")) %>%
        rename("parid" = "name") %>%
        st_drop_geometry() %>%
        select(-value, -geometry)

       own2 <- own %>%
         left_join(.,temp,  by = "parid") %>%
         st_drop_geometry() 
       
         #Some cases are edge and do not intersect
             temp <- own2 %>%
               filter(is.na(GEOID))
          #Now pull out of the own data frame those parids
             temp2 <- own %>%
               filter(parid %in% temp$parid)
         #Use nearest feature to find the nearest tract for those points
             temp3 <- as.data.frame(st_nearest_feature(temp2, map)) 
         #Now link the parids to the GEOIDs
             temp <- cbind(temp2$parid, temp3)
             names(temp) <- c("parid", "map.id")
             temp <- temp %>%
              left_join(., map, by="map.id") %>%
               st_drop_geometry() %>%
               select(-map.id, -geometry)
          #Now put these back
             own3 <- own2 %>%
               filter(!is.na(GEOID)) %>%
               bind_rows(., temp) 
             
    ifelse(i==2010,
           cen.own <- own3 %>%
             rename("GEOID.10" = "GEOID") %>%
             select(parid, GEOID.10),
           cen.own <- cen.own %>%
             left_join(., own3, by="parid") %>%
             rename("GEOID.20" = "GEOID") %>%
             select(parid, GEOID.10, GEOID.20)
    )
    }
  cen.own <- cen.own %>%
    select(parid, starts_with("GEOID"))
  #Now update the original OWN file with this information
  
  OWN1 <- own1 %>%
    left_join(., cen.own, by="parid") %>%
    mutate(GEOID = case_when(year < 2020 ~ GEOID.10,
                             TRUE ~ GEOID.20)) %>%
    select(-GEOID.10, -GEOID.20) %>%
    select(parid, year, GEOID)
      
   
#Merge the Census Data with the correct Tracts in OWN data
 load("./Build/Output/census.RData")
 
 cen_own <- OWN1 %>%
   left_join(., census, by=c("GEOID", "year")) %>%
   filter(year != 2024)
 
save(cen_own, file="./Build/Output/core_own.RData")  
  rm(i, own, OWN, own1, own2, OWN1)

#Get ACS TIGER files for three census years and link to the OWN Data ####

  load(file="./Build/Output/sal_own.RData")
  
  core_cen <- core %>%
    left_join(., cen.own, by = "parid") %>%
    mutate(year = saleyr,
           GEOID = case_when(year < 2020 ~ GEOID.10,
                             TRUE ~ GEOID.20)) %>%
    select(-GEOID.10, -GEOID.20) %>%
    left_join(., census, by = c("GEOID", "year"))
  
  save(core_cen, file="./Build/Output/core_cen.RData")
  
  
 