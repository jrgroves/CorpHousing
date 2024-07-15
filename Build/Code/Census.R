#Pulls ACS data
#This file needs the Owner Filter.R file to run first.


#Jeremy R. Groves
#June 20, 2024

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Load Data Files####

load("./Build/Output/owners.RData")

#Set starting parameters####
y <- seq(2009,2020)

breaks <- seq(0, 1, by = 0.1)
cols <- RColorBrewer::brewer.pal(11, "Spectral")
var<-c("C17002_001","C17002_002","C17002_003","C17002_004","C17002_005","C17002_006","C17002_007",
       "C17002_008")

    #These correspond to the following Ratio of Income to Poverty Level over 12 months:
    #"Total", "under .50", ".5 to .99", "1 to 1.24", "1.25 to 1.49", "1.5 to 1.84", "1.85 to 1.99", "2 and over"


#Get acs data Tiger Files from Census####
for(i in y){
acs <- get_acs(geography = "tract",
                     variables = var,
                     year = i,
                     state = 29,
                     county = 189,
                     geometry = FALSE) #Geometry different for 2009, 2010-2019, 2020 + thus the three maps below
acs <- acs %>%
  pivot_wider(., id_cols = "GEOID", names_from = "variable", values_from = "estimate") %>%
  mutate(across(C17002_002:C17002_008, ~ .x / C17002_001),
         CENSUS_TRA = substr(GEOID, 6, 11),
         year = i) 

ifelse(i==2009, 
       ACS <- acs, 
       ACS <- rbind(ACS, acs))
}

ACS <- ACS %>%
  mutate(B2 = 1 - C17002_008, #This creates a total under 2 times income/poverty level ratio
         mapyear = case_when(year<2010 ~ "2009",
                             year>2009 & year<2020 ~ "2010",
                             year>2019 ~ "2020",
                             TRUE~"0"))
  

#Get ACS maps for the three different years

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

  ifelse(i==2009, 
         MAP <- map, 
         MAP <- rbind(MAP, map))
}

#Create Master ACS/Map file####
  MAP2 <- MAP %>%
    right_join(., ACS, by=c("GEOID", "mapyear")) %>%
    select(GEOID, mapyear, B2, geometry)

save(MAP2, file="./Build/Output/CenMap.RData")

plot<-ggplot(MAP2) +
  geom_sf(aes(fill = B2)) +
  scale_fill_stepsn(colors = cols,breaks = breaks, name = "Percent Below 2 times Poverty Level") +
  facet_wrap( ~year)

