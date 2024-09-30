#Uses annual ownership files and sales data to determine how the tenure and ownership information changes
#after the sale of each parcel.

#Uses: Sales.RData file created by Sales.R
#      OWN.RData file created by Owner Filter Fixing Parallel.R        

#Saves: sal_own.RData - sales data with pre and post sale ownership.

#Jeremy R. Groves
#Created: September 16, 2024
#Updated: September 18, 2024

rm(list=ls())

library(tidyverse)

load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/Own.RData")

#Create wide data of ownership tenure####

  own.ten <- OWN %>%
    select(PARID, year, TENURE) %>%
    pivot_wider(id_cols = PARID, 
                names_from = year, 
                names_prefix = "ten.",
                values_from = TENURE)

#Create wide data of ownership corporate vs. private####

  own.pri <- OWN%>%
    select(PARID, year, private) %>%
    pivot_wider(id_cols = PARID, 
                names_from = year, 
                names_prefix = "priv.",
                values_from = private)

#Clean sales data####

  sold <- SALES %>%
    select(PARID, saleyear) %>%
    mutate(presale = saleyear - 1,
           postsale = saleyear + 1)

  work <- sold %>%
    mutate(year = presale) %>%
    left_join(., OWN, c("PARID", "year")) %>%
    mutate(PREOWN_CITY = as.character(OWN_CITY),
           PREOWN_STATE = as.character(OWN_STATE),
           PREOWN_ZIP = as.character(OWN_ZIP),
           PREOWN_TENURE = as.character(TENURE),
           PREOWN_Private = as.character(private)) %>%
    select(PARID, saleyear, presale, postsale, starts_with("PRE"))
  
  work <- work %>%
    mutate(year = postsale) %>%
    left_join(., OWN, c("PARID", "year")) %>%
    mutate(POSTOWN_CITY = as.character(OWN_CITY),
           POSTOWN_STATE = as.character(OWN_STATE),
           POSTOWN_ZIP = as.character(OWN_ZIP),
           POSTOWN_TENURE = as.character(TENURE),
           POSTOWN_Private = as.character(private)) %>%
    select(PARID, saleyear, presale, postsale, starts_with("PRE"), starts_with("POST"))
  
    #NOTE that there are about 32000 observations, especially with more recent sales, that do not show up in owner
    #data for some reason.

    core <- work %>%
      filter(!is.na(PREOWN_TENURE)) %>%
      filter(!is.na(POSTOWN_TENURE)) %>%
      mutate(ten = as.numeric(PREOWN_TENURE!=POSTOWN_TENURE),
             ten1 = case_when(PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "NOT OWNER" ~ 1,
                              PREOWN_TENURE == "OWNER" & POSTOWN_TENURE == "NOT OWNER" ~ 2,
                              PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "OWNER" ~ 3,
                              is.na(PREOWN_TENURE) & !is.na(POSTOWN_TENURE) ~ 5, #this is the case where a property shows up after sale (none exist)
                              !is.na(PREOWN_TENURE) & is.na(POSTOWN_TENURE) ~ 6,
                              TRUE ~ 4),  #this is owner sold to owner
             city = as.numeric(PREOWN_CITY != POSTOWN_CITY),
             state = as.numeric(PREOWN_STATE != POSTOWN_STATE),
             zip = as.numeric(PREOWN_ZIP != POSTOWN_ZIP),
             P2C = ifelse(PREOWN_Private == 1 & POSTOWN_Private == 0, 1, 0),
             P2P = ifelse(PREOWN_Private == 1 & POSTOWN_Private == 1, 1, 0),
             C2C = ifelse(PREOWN_Private == 0 & POSTOWN_Private == 0, 1, 0),
             C2P = ifelse(PREOWN_Private == 0 & POSTOWN_Private == 1, 1, 0),
             N2O = ifelse(PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "OWNER",1,0),
             N2N = ifelse(PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "NOT OWNER",1,0),
             O2O = ifelse(PREOWN_TENURE == "OWNER" & POSTOWN_TENURE == "OWNER",1,0),
             O2N = ifelse(PREOWN_TENURE == "OWNER" & POSTOWN_TENURE == "NOT OWNER",1,0))
    
    rm(work)

#Save Sales Data with ownership changes
  save(core, file="./Build/Output/sal_own.RData")
