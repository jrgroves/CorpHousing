#Creates maps using the parcel maps from GIS and Census Tracts for U.S. Census
#This file needs the Owner Filter.R file to run first.


#Jeremy R. Groves
#June 20, 2024

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Load Data Files####

loc<-st_read("./Build/Data/2021/Parcels.shp")
load("./Build/Output/owners.RData")


#Set starting parameters####
y <- seq(2002,2020)

breaks <- seq(0, 0.5, by = 0.05)
cols <- RColorBrewer::brewer.pal(11, "Spectral")
var<-c("H001001")


#Get 2010 Tiger Files from Census####

acs <- get_decennial(geography = "tract",
                     variables = var,
                     year = 2010,
                     state = 29,
                     county = 189,
                     geometry = TRUE) 
acs <- acs %>%
  mutate(CENSUS_TRA = substr(GEOID, 6, 11)) %>%
  select(GEOID, CENSUS_TRA, value, geometry)

acs<-st_make_valid(acs)

#Merge the annual owner data with the 2021 parcel map with census tracks####

loc <- loc %>%
  select(PARENT_LOC, LOCATOR, CENSUS_TRA)

for(i in y){
  #Calculate the total living units in each census tract
  ifelse(i<2018,
         parcel <- read.dbf(file=paste0("./Build/Data/", i, "/Pardata.dbf"), as.is = FALSE),
         parcel <- read.csv(file=paste0("./Build/Data/", i, "/primary_parcel.csv"),
                            header = TRUE,
                            sep = "|",
                            quote = "", 
                            row.names = NULL, 
                            stringsAsFactors = FALSE))
  
  ifelse(i<2018,
         parcel <- parcel,
         parcel <- parcel %>%
           mutate(PARENT_LOC = PARID,
                  LOCATOR = PARID)
  )
  
  liv_unit <- parcel %>%
    select(PARENT_LOC, LOCATOR, LIVUNIT) %>%
    filter(LIVUNIT > 0) %>%
    mutate(year = i) %>%
    right_join(loc, ., by = c("PARENT_LOC", "LOCATOR"))
  
  liv_unit<-st_drop_geometry(liv_unit)
  
  
  #Combine Ownership Data for Not Owner Occuplied
  own_dat<-get(paste0("own_dat", i))

  par <- own_dat %>% 
    select(PARENT_LOC, LOCATOR, TENURE, LIVUNIT,
           Corporate, Partner, Trust, Bank, Instit, hoa, muni, private) %>%
    mutate(key = Corporate+Partner+Trust+Bank+Instit+hoa+muni+private,
           private = case_when(key == 0 ~ 1,
                               private == 1 ~ 1,
                               TRUE~0)) %>%
    right_join(loc, ., by = c("PARENT_LOC", "LOCATOR")) %>%
    mutate(year = i)
  
  ifelse(i==2002, PAR <- par, PAR <- rbind(PAR, par))
  
  
  ifelse(i==2002, LIV <- liv_unit, LIV <- rbind(LIV, liv_unit))
}

  liv <- LIV %>%
    group_by(CENSUS_TRA, year) %>%
    mutate(VAR = LIVUNIT) %>%
    summarise_at(vars(VAR),
                 list(Overall = sum)) 

  corp<-PAR %>%
    st_drop_geometry()
  
  corp2 <- corp %>%
    group_by(CENSUS_TRA, year) %>%
    mutate(VAR = Corporate,
           Corp_LU = LIVUNIT * Corporate) %>%
    summarise_at(vars(VAR, Corp_LU),
                 list(Percent = mean,
                      Overall = sum)) %>%
    full_join(., liv, by=c("CENSUS_TRA", "year")) %>%
    right_join(acs, ., by = "CENSUS_TRA") %>%
    mutate(per_lu = Corp_LU_Overall/Overall) %>%
    select(-c(value, VAR_Overall))
  

  corp2<-filter(corp2, year>2008)

#Generate Plot
plot<-ggplot(corp2) +
    geom_sf(aes(fill = per_lu)) +
    scale_fill_stepsn(colors = cols,breaks = breaks, name = "Percent Corporate") +
    facet_wrap( ~year)
  