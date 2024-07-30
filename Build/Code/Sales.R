#Processes Sales Data

#Jeremy R. Groves
#June 27, 2024

rm(list=ls())

library(tidyverse)
library(foreign)

#Load in Data#####
sales <- read.csv(file="./Build/Data/2020/sales.csv",
                  header = TRUE,
                  sep = "|",
                  quote = "", 
                  row.names = NULL, 
                  stringsAsFactors = FALSE)

load(file="./Build/Output/owners.RData")


#Set parameters#####

year<-seq(2001,2020)

#Clean Sales Data

SALES <- sales %>%
  mutate(SALEVAL = case_when(SALEVAL == ".X" ~ "X",
                             SALEVAL == "i" ~ "I",
                             SALEVAL == "x" ~ "X",
                             SALEVAL == " x" ~ "X",
                             SALEVAL == "t" ~ "T",
                             SALEVAL == "TT" ~ "T",
                             SALEVAL == " T" ~ "T",
                             TRUE ~ SALEVAL),
         SALEDT2 = as.Date(SALEDT, "%d-%b-%y"),
         year = year(SALEDT2)) %>%
  filter(SALEVAL == "4" |
           SALEVAL == "5" |
           SALEVAL == "F" |
           SALEVAL == "I" |
           SALEVAL == "P" |
           SALEVAL == "X" |
           SALEVAL == "Z" |
           SALEVAL == "T") %>%
  filter(!is.na(PRICE)) %>%
  select(PARID, SALEDT2, PRICE, SALETYPE, SALEVAL, year) %>%
  distinct() %>%
  mutate(taxyear = year + 1,
         presale = taxyear - 1,
         postsale = taxyear + 1) %>%
  filter(year>2001 & year < 2020) 

#Create main ownership data####

  for(i in year){
    own<-get(paste0("fown_dat",i))
    
    own <- own %>%
      mutate(PARID = LOCATOR,
             year = i) 
    ifelse(i==2001,
           OWN <- own,
           OWN <- rbind(OWN, own ))
  }
  
  OWN <- filter(OWN, !is.na(OWN_STATE))
  OWN <- filter(OWN, !is.na(OWN_ZIP))
  OWN <- filter(OWN, !is.na(PROP_ZIP))
  
  work <- SALES %>%
    mutate(year = presale) %>%
    left_join(., OWN, c("PARID", "year")) %>%
    filter(!is.na(LOCATOR)) %>%
    mutate(OWN_STATE = as.character(OWN_STATE),
           OWN_ZIP = as.character(OWN_ZIP))
  
  work <- work %>%
    mutate(year = postsale) %>%
    left_join(., OWN, c("PARID", "year"))%>%
    filter(!is.na(LOCATOR.y)) %>%
    mutate(OWN_STATE.y = as.character(OWN_STATE.y),
           OWN_ZIP.y = as.character(OWN_ZIP.y))
  
  core <- work %>%
    mutate(ten = as.numeric(TENURE.x!=TENURE.y),
           ten1 = case_when(TENURE.x == "NOT OWNER" & TENURE.y == "NOT OWNER" ~ 1,
                            TENURE.x == "OWNER" & TENURE.y == "NOT OWNER" ~ 2,
                            TENURE.x == "NOT OWNER" & TENURE.y == "OWNER" ~ 3,
                            TRUE ~ 4),
           own = as.numeric(OWNER_NAME.x != OWNER_NAME.y),
           state = as.numeric(OWN_STATE.x != OWN_STATE.y),
           zip = as.numeric(OWN_ZIP.x != OWN_ZIP.y))
  
  
