#Creates maps using the parcel maps from GIS and Census Tracts for U.S. Census
#This file needs the Owner Filter.R file to run first.


#Jeremy R. Groves
#June 20, 2024

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Load Data Files####

loc<-st_read("./Build/Data/Map/locators.shp")
load("./Build/Data/Input/owners.RData")


#Set starting parameters####
y<-str_pad(seq(2,17), 2, "left", pad="0")

breaks <- seq(0, 0.6, by = 0.05)
cols <- RColorBrewer::brewer.pal(11, "Spectral")

var<-c("H001001")

#Load Census Map and Convert CRS####

acs<-get_decennial(geography = "tract",
             variables = var,
             year = 2000,
             state = 29,
             county = 189,
             geometry = TRUE) 

acs <- st_transform(acs, st_crs(loc))


#Loop through owner data to find percentages####
t<-1
for(i in y){
  
  own_dat<-get(paste0("own_dat", i))

  par <- own_dat %>% 
    select(PARENT_LOC, LOCATOR, LUCODE, TENURE,
           Corporate, Partner, Trust, Bank, Instit, hoa, muni, private) %>%
    mutate(key = Corporate+Partner+Trust+Bank+Instit+hoa+muni+private,
           private = case_when(key == 0 ~ 1,
                               private == 1 ~ 1,
                               TRUE~0)) %>%
    filter(LUCODE == "Single Family" |
             LUCODE == "Multi-Family" |
             LUCODE == "Duplex/Townhome")
  
  par_sf <-  right_join(loc, par, by = c("PARENT_LOC", "LOCATOR"))
  
  par_sf2<-st_intersection(par_sf, acs)
  
  corp<-par_sf2 %>%
    group_by(GEOID) %>%
    summarise_at(vars(Corporate),
                 list(Mean_Frequency = mean,
                      Overall = sum)) %>%
    st_drop_geometry()
  
  corp <- right_join(acs, corp, by = "GEOID") %>%
    mutate(year = paste0("20", i),
           p_whole = Overall/value)
  
  ifelse(t==1, CORP <- corp, CORP <- rbind(CORP, corp))
  
  t<- t + 1
}

#Generate Plot
plot<-ggplot(CORP) +
    geom_sf(aes(fill = p_whole)) +
    scale_fill_stepsn(colors = cols,breaks = breaks, name = "Percent Corporate") +
    facet_wrap( ~year)
  



