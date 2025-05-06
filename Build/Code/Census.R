#Pulls ACS data
#This file needs the Owner Filter.R file to run first.


#Jeremy R. Groves
#June 20, 2024

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(ipumsr)
library(sf)



#IPUMS NGIS Annual Tract Estimates downloaded from: https://www.nhgis.org/annual-tract-estimates#interpolation

dat <- read.csv(file = "./Build/Input/nhgis_tract_popest_2000_2019_29.csv")

data.race <- dat %>%
  filter(COUNTYA == "189") %>%
  select(-c("GEOID", "STATEA", "STATE", "COUNTYA", "COUNTY","TRACTA", "GEOGYEAR")) %>%
  group_by(GISJOIN, RACE) %>% 
  summarise(across(ESTIMATE_2000:ESTIMATE_2019, sum)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("ESTIMATE"), names_to = "YEAR", names_prefix = "ESTIMATE_", values_to = "POP") %>%
  pivot_wider(id_cols = c(GISJOIN, YEAR), names_from = RACE, values_from = POP) %>%
  mutate(pop = rowSums(.[,3:8]),
         per_wht = white / pop,
         per_blk = black / pop,
         per_asian = asian/pop,
         per_other = 1 - per_wht - per_blk - per_asian,
         other = aian + multi + nhopi) %>%
  select(-c("aian", "multi","nhopi"))

data.gender <- dat %>%
  filter(COUNTYA == "189") %>%
  select(-c("GEOID", "STATEA", "STATE", "COUNTYA", "COUNTY","TRACTA", "GEOGYEAR", "RACE")) %>%
  mutate(agenum = case_when(AGEGRP == "00_04" ~ "1",
                            AGEGRP == "05_09" ~ "1",
                            AGEGRP == "10_14" ~ "1",
                            AGEGRP == "15_19" ~ "1",
                            AGEGRP == "64_69" ~ "2",
                            AGEGRP == "70_74" ~ "2",
                            AGEGRP == "75_79" ~ "2",
                            AGEGRP == "80_84" ~ "2",
                            AGEGRP == "85_up" ~ "2",
                            TRUE ~ "3")) %>%
  group_by(GISJOIN, SEX, agenum) %>% 
  summarise(across(ESTIMATE_2000:ESTIMATE_2019, sum)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("ESTIMATE"), names_to = "YEAR", names_prefix = "ESTIMATE_", values_to = "POP") %>%
  mutate(AGEGRP = case_when(agenum == "1" ~ "und20",
                            agenum == "2" ~ "over64",
                            agenum == "3" ~ "a20_64")) %>%
  pivot_wider(id_cols = c(GISJOIN, YEAR), names_from = c(SEX, AGEGRP), values_from = POP) %>%
  mutate(female = F_und20 + F_over64 + F_a20_64,
         male = M_und20 + M_over64 + M_a20_64,
         under20 = F_und20 + M_und20,
         over64 = F_over64 + M_over64,
         per_fem = female / (female + male),
         per_und20 = under20/(female + male),
         per_over64 = over64 / (female + male)) %>%
  select(-starts_with("F_"), -starts_with("M_"))

data.est <- data.race %>%
  left_join(., data.gender, by=c("GISJOIN", "YEAR"))

rm(data.gender, data.race)

#Subsequent data (2020 - 2023) downloaded via API
#ACS 5-Year Data starting in 2006 downloaded via API 


#Set starting parameters####
  y <- seq(2009,2023)
  
  breaks <- seq(0, 1, by = 0.1)
  cols <- RColorBrewer::brewer.pal(11, "Spectral")
  var<-c("B02001_001","B02001_002" )

#Get ACS data from Census####
    for(i in y){
    acs <- get_acs(geography = "tract",
                         variables = var,
                         year = i,
                         state = 29,
                         county = 189,
                         geometry = FALSE) #Geometry different for 2009, 2010-2019, 2020 + thus the three maps below
    acs <- acs %>%
      mutate(variable = case_when(variable == "B02001_001" ~ "pop",
                                  variable == "B02001_002" ~ "white",)) %>%
      pivot_wider(., id_cols = "GEOID", names_from = "variable", values_from = "estimate")  %>%
      mutate(per_wht = white / pop,
             year = i,
             GISJOIN = paste0("G",substr(GEOID, 1, 2), "0", substr(GEOID, 3,5), "0", substr(GEOID, 6, nchar(GEOID)))) %>%
      select(-GEOID)
    
    ifelse(i==2009, 
           ACS <- acs, 
           ACS <- rbind(ACS, acs))
    }

#Testing and Comparing the estimates with annual ACS values####
   ACS$acs <- 1

   core <- data.est %>%
     select(GISJOIN, YEAR, white, per_wht, pop) %>%
     mutate(year = YEAR,
            acs = 0) %>%
     select(-YEAR)%>%
     rbind(., ACS)
   
   test <- core %>%
     select(GISJOIN) %>%
     distinct() %>%
     sample_n(8)
   
   test2 <- core %>%
     filter(GISJOIN %in% test$GISJOIN) %>%
     mutate(year = as.numeric(year),
            acs = as.character(acs))
   
   ggplot(test2) +
     geom_line(aes(x = year, y = per_wht, colour = GISJOIN, linetype = acs))
 

#Get ACS TIGER files for three census years and link to the 2025 Parcel map ####
    
    loc<-st_read("F:/Data/Saint Louis GIS Data/gis_2025/Parcels_Current.shp")

    parmap <- loc %>%
      select(PARENT_LOC, LOCATOR, PROP_ADD, PROP_ZIP) %>%
      st_centroid()
    
    rm(loc)
  
    parcel <- parmap %>%
      st_drop_geometry() %>%
      mutate(GEOID.00 = NA,
             GEOID.10 = NA,
             GEOID.20 = NA)
    k<-5
    y<-c(2009, 2010, 2020)
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
      ifelse(i == 2009, MAP <- map, MAP <- rbind(MAP, map))
    }
    
      
      parmap <- st_transform(parmap, st_crs(map)) #projects STL map to match CENSUS maps
    
      temp <- st_intersects(map, parmap)
      
      for(i in seq(1, length(temp))){
        parcel[,k] = replace(parcel[,k], temp[[i]], map$GEOID[i])
      }
      k<-k+1

    }
    cen.map <- map
      rm(MAP)
    save(cen.map, file="./Build/Output/CenMap.RData")

#Create a MAP of Percent below 2X Poverty Level for 2009, 2010, and 2020 as test.#####
  MAP2 <- map %>%
    right_join(., ACS, by=c("GEOID", "mapyear")) %>%
    select(GEOID, mapyear, B2, geometry)
  
  ggplot(MAP2) +
    geom_sf(aes(fill = B2)) +
    scale_fill_stepsn(colors = cols,breaks = breaks, name = "Percent Below 2 times Poverty Level") +
    facet_wrap( ~mapyear)

