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
  filter(year>2001 & year < 2020) 

#Create main ownership data####

for(i in year){
  own<-get(paste0("fown_dat",i))
  
  own <- own %>%
    mutate(PARID = LOCATOR,
           year = i) %>%
    select(PARID, TENURE, year)
  
  ifelse(i==2001,
         OWN <- own,
         OWN <- rbind(OWN, own ))
}

comp <- SALES %>%
  mutate(coreyear = year,
         preyear = coreyear - 1,
         postyear = coreyear + 1,
         year = preyear) %>%
  left_join(., OWN, by=c("PARID","year")) %>%
  rename(., pre = TENURE) %>%
  mutate(year = postyear) %>%
  left_join(., OWN, by=c("PARID","year"))%>%
  rename(., post = TENURE) %>%
  mutate(switch = case_when(pre == post ~ 0,
                            pre != post ~ 1,
                            TRUE ~ 1),
         O2NO = case_when(pre == "OWNER" & post== "NOT OWNER" ~ 1,
                          TRUE ~ 0),
         NO2O = case_when(pre == "NOT OWNER" & post== "OWNER" ~ 1,
                          TRUE ~ 0),
         O2O = case_when(pre == "OWNER" & post== "OWNER" ~ 1,
                          TRUE ~ 0),
         NO2NO = case_when(pre == "NOT OWNER" & post== "NOT OWNER" ~ 1,
                          TRUE ~ 0))

  
