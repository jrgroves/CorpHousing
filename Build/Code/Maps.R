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
load("./Build/Output/owners.RData")
load("./Build/Output/CenMap.RData")


#Set starting parameters####
y <- seq(2002,2020)

cy <-c(2009, 2010, 2020)

breaks <- seq(0, 0.5, by = 0.05)
cols <- RColorBrewer::brewer.pal(11, "Spectral")
var<-c("B25017_001")


#Clean up the PARDAT shapefile for merge with ACS maps

parmap <- loc %>%
  select(PARENT_LOC, LOCATOR, PROP_ADD, PROP_ZIP) %>%
  st_centroid()


#Get 2010 Tiger Files from Census####
for(i in cy){
  acs <- get_acs(geography = "tract",
                       variables = var,
                       year = i,
                       state = 29,
                       county = 189,
                       geometry = TRUE) 
  acs <- acs %>%
    select(GEOID, estimate, geometry) %>%
    rename_with(., ~ paste0("GEOID",i), starts_with("GEOID")) %>%
    rename_with(., ~ paste0("estimate",i), starts_with("estimate")) %>%
    st_make_valid(.)
 
  map <- st_transform(acs, st_crs(parmap)) #projects ACS map to match STL maps
  
  parmap<-st_intersection(parmap, map)
  
  
}


#Merge the annual owner data with the 2021 parcel map with census tracks####

for(i in y){
  #Calculate the total living units in each census tract

  parcel <- get(paste0("fown_dat",i))
  parcel <- parcel %>%
    filter(LIVUNIT > 0) %>%
    select(PARENT_LOC, LOCATOR, TENURE, LIVUNIT,
           Corporate, Partner, Trust, Bank, Instit, hoa, muni, private) %>%
    mutate(key = Corporate+Partner+Trust+Bank+Instit+hoa+muni+private,
           private = case_when(key == 0 ~ 1,
                               private == 1 ~ 1,
                               TRUE~0)) %>%
    mutate(year = i)  %>%
    right_join(parmap, ., by = c("PARENT_LOC", "LOCATOR")) %>%
    st_drop_geometry(.)


  
  liv  <- agg("LIVUNIT", sum)
  Corp <- agg("Corporate", mean)
  Priv <- agg("private", mean)
  
  core <- liv %>%
    right_join(., Corp, by=c("GEOID", "year")) %>%
    right_join(., Priv, by=c("GEOID", "year")) 
  
  
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
  geom_sf(aes(fill = private)) +
  scale_fill_stepsn(colors = cols,breaks = breaks, name = "Percent Private") +
  facet_wrap( ~year)
  