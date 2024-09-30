#Creates maps using the parcel maps from GIS and Census Tracts for U.S. Census
#This file needs the Owner Filter.R file to run first.


#Jeremy R. Groves
#June 20, 2024
#REV: July 15: Account for difference Census maps and full ownership data .

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Functions####
agg <- function(y, z){
  
  j <- parcel$year[1]
  
  ifelse(j<2010,
         temp <- parcel %>% 
           group_by(GEOID2009) %>% 
           summarise_at(vars(y), list(Units = z)) %>%
           mutate(year = j) %>%
           rename(GEOID = GEOID2009),
         ifelse(j<2020,
                temp <- parcel %>% 
                  
                  group_by(GEOID2010) %>% 
                  summarise_at(vars(y), list(Units = z)) %>%
                  mutate(year = j)%>%
                  rename(GEOID = GEOID2010),
                
                temp <- parcel %>% 
                  group_by(GEOID2020) %>% 
                  summarise_at(vars(y), list(Units = z)) %>%
                  mutate(year = j) %>%
                  rename(GEOID = GEOID2020)))
  temp <- rename_with(temp, ~paste0(y), starts_with("Units"))
  
  return(temp)
}



#Load Data Files####

  loc<-st_read("./Build/Data/2021/Parcels.shp")
  #load("./Build/Output/owners.RData")
  load("./Build/Output/CenMap.RData")
  #load("./Build/Output/OWN.RData")


#Set starting parameters####
  y <- seq(2002,2020)
  
  cy <-c(2009, 2010, 2020)
  
  breaks <- seq(0, 0.5, by = 0.05)
  cols <- RColorBrewer::brewer.pal(11, "Spectral")
  var<-c("B25017_001")


#Clean up the PARDAT Shape file for merge with ACS maps

  parmap <- loc %>%
    select(PARENT_LOC, LOCATOR, PROP_ADD, PROP_ZIP) %>%
    st_centroid()
  
  parmap <- st_transform(parmap, st_crs(MAP2)) #projects STL map to match CENSUS maps

#Link parcels with census tracts for all three years as seperate sf objects
  
  cen.00 <- MAP2 %>%
    filter(mapyear == "2009") %>%
    select(B2) %>%
    st_intersection(., parmap)
  


#Merge the annual owner data with the 2021 parcel map with census tracks####
  
  for(i in y){
    #Calculate the total living units in each census tract
  
    parcel <- get(paste0("own_dat",i))
    parcel <- OWN %>%
      filter(LIVUNIT > 0) %>%
      select(PARENT_LOC, LOCATOR, TENURE, LIVUNIT,
             Corporate, Bank, Trustee, Hoa, Muni, Nonprof, private, year, key) 
    
    
    %>%
      right_join(parmap, ., by = c("PARENT_LOC", "LOCATOR")) %>%
      st_drop_geometry(.)


  
  liv  <- agg("LIVUNIT", sum)
  Corp <- agg("Corporate", mean)
  Trst <- agg("Trustee", mean)
  Priv <- agg("private", mean)
  Bank <- agg("Bank", mean)
  
  core <- liv %>%
    right_join(., Corp, by=c("GEOID", "year")) %>%
    right_join(., Priv, by=c("GEOID", "year")) %>%
    right_join(., Trst, by=c("GEOID", "year")) %>%
    right_join(., Bank, by=c("GEOID", "year")) 
  
  
  ifelse(i==2002, CORE <- core, CORE<-rbind(CORE, core))
}
  

CORE <- CORE %>%
  mutate(mapyear = case_when(year < 2010 ~ "2009",
                             year > 2019 ~ "2020",
                             TRUE ~ "2010")) 

Map<-full_join(MAP2, CORE, by=c("GEOID", "mapyear"))


#Generate Plot
plot1<-ggplot(Map) +
    geom_sf(aes(fill = Corporate)) +
    scale_fill_stepsn(colors = cols,breaks = breaks, name = "Percent Corporate") +
    facet_wrap( ~year)

plot2<-ggplot(Map) +
  geom_sf(aes(fill = Bank)) +
  facet_wrap( ~year)
  