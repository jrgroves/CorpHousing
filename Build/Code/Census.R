#Pulls ACS data using the ACS API. Right now only contains data from 2010 to 2023 due to ACS limitations
#for data at the tract level. Then we use the xcoord and ycoord in the assessors data to intersect the OWN file
#with the census tract maps and merge the census data with the owners data to create a core_own dataset.

#This file needs the Owner Filter 2010.R file to run first.


#Jeremy R. Groves
#June 20, 2024
#Updated: May 9, 2025
#Updated: June 18, 2025: In this update I utilize the OWN file (specifically the one containing only 2010 to 2024) to intersect
#                        the census tiger maps for the geocodes for the tracts. I use the x and y coords included in the data from
#                        the assessor to avoid a problem with different parids between the GIS and the assessor files.

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Set starting parameters####
y <- seq(2010,2023)

breaks <- seq(0, 1, by = 0.1)
cols <- RColorBrewer::brewer.pal(11, "Spectral")

var<-c("B01003","B02001","B01002","B19013","B15002","B17023","B25010","B25008","S0101")

#Get ACS data from Census####

for(k in var){
   for(i in y){
      acs <- get_acs(geography = "tract",
                   table = k,
                   year = i,
                   state = 29,
                   county = 189,
                   cache_table = TRUE,
                   geometry = FALSE) #Geometry different for 2009, 2010-2019, 2020 + thus the three maps below

      temp <- acs %>%
        select(-moe, -NAME) %>%
        mutate(year = i)
      
      ifelse(i==2010, 
             TEMP <- temp, 
             TEMP <- rbind(TEMP, temp))
      
      rm(temp)
  }
  
  ifelse(k == "B01003",
         ACS <- TEMP,
         ACS <- rbind(ACS, TEMP))
}



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
  filter(variable != "delete") %>%
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
  mutate(cen_yr = case_when(year == 2009 ~ 2000,
                            year > 2009 & year < 2020 ~ 2010,
                            TRUE ~ 2020))

save(census, file="./Build/Output/census.RData")
rm(ACS, census, acs, TEMP, var, y, i, k)

#Get ACS TIGER files for three census years and link to the OWN Data ####
    
  load("./Build/Output/Own10.RData")    
  
  parcel <- OWN %>%
    select(parid, xcoord, ycoord, year) %>%
    mutate(xcoord = case_when(year <= 2023 ~ NA,
                              TRUE ~ xcoord),
           ycoord = case_when(year <= 2023 ~ NA,
                              TRUE ~ ycoord))%>%
    group_by(parid) %>%
    fill(xcoord, .direction = "downup") %>%
    fill(ycoord, .direction = "downup") %>%
    ungroup()%>%
    select(-year) %>%
    distinct(parid, .keep_all = T) %>%
    filter(!is.na(xcoord),
           !is.na(ycoord)) %>%
    mutate(#GEOID.00 = NA,
           GEOID.10 = NA,
           GEOID.20 = NA)   %>%
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
        mutate(CENSUS_TRA = substr(GEOID, 6, 11),
               mapyear = as.character(i)) %>%
        select(GEOID, mapyear)
  
      parcel <- st_transform(parcel, st_crs(map)) #projects STL map to match CENSUS maps
    
      temp <- st_intersects(parcel, map)
        temp[lengths(temp)==0] <- 999
        
        temp2 <- as.data.frame(temp) %>%
          mutate(col.id = case_when(col.id == 999 ~ NA,
                                  TRUE ~ col.id),
          GEOID = map$GEOID[col.id])
      
        ifelse(i==2010, parcel$GEOID.10<-temp2$GEOID, parcel$GEOID.20<-temp2$GEOID)
    }
  
  OWN1 <- parcel %>%
    right_join(., OWN, by="parid") %>%
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
#Merge the Census Data with the correct Tracts in OWN data
 load("./Build/Output/census.RData")
 
 core_own <- OWN1 %>%
   mutate(GEOID = case_when(year < 2019 ~ GEOID.10,
                            TRUE ~ GEOID.20)) %>%
   left_join(., census, by=c("GEOID", "year")) %>%
   filter(!is.na(per_own)) %>%
   select(-c(xcoord, ycoord, GEOID.10, GEOID.20))
 
save(core_own, file="./Build/Output/core_own.RData")  



