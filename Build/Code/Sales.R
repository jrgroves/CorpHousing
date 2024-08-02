#Processes Sales Data

#Jeremy R. Groves
#June 27, 2024

rm(list=ls())

library(tidyverse)
library(foreign)
library(rjson)
library(blsAPI)

#Load in Data#####
sales <- read.csv(file="./Build/Data/2020/sales.csv",
                  header = TRUE,
                  sep = "|",
                  quote = "", 
                  row.names = NULL, 
                  stringsAsFactors = FALSE)

load(file="./Build/Output/owners.RData")

#Get CPI data from BLS API#####

payload <- list(
  'seriesid' = c('CUSR0000SA0'),
  'startyear' = 2000,
  'endyear' = 2019,
  'registrationKey' = '8213e4e9bef14041a5f00491b6c123d6')

response <- blsAPI(payload, 2, TRUE)

cpi <- response %>%
  mutate(period = gsub("M", "", period),
         date = paste(period, "01", year, sep="-"),
         date2 = format(as.Date(date, format = "%m-%d-%Y"), "%m-%Y"),
         value = as.numeric(value)) %>%
  select(date2, value)

payload <- list(
  'seriesid' = c('CUSR0000SA0'),
  'startyear' = 2020,
  'endyear' = 2023,
  'registrationKey' = '8213e4e9bef14041a5f00491b6c123d6')

response <- blsAPI(payload, 2, TRUE)

cpi2 <- response %>%
  mutate(period = gsub("M", "", period),
         date = paste(period, "01", year, sep="-"),
         date2 = format(as.Date(date, format = "%m-%d-%Y"), "%m-%Y"),
         value = as.numeric(value)) %>%
  select(date2, value)

cpi <- rbind(cpi, cpi2)

rm(payload, response, cpi2)

#Set parameters#####

year<-seq(2001,2020)

#Clean Sales Data#####

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
         date2 = format(SALEDT2, "%m-%Y"),   #for CPI merge
         year = year(SALEDT2),
         PRICE = as.numeric(PRICE)) %>%
  filter(SALEVAL == "4" |
           SALEVAL == "5" |
           SALEVAL == "F" |
           SALEVAL == "I" |
           SALEVAL == "P" |
           SALEVAL == "X" |
           SALEVAL == "Z" |
           SALEVAL == "T") %>%
  filter(!is.na(PRICE)) %>%
  select(PARID, SALEDT2, date2, PRICE, SALETYPE, SALEVAL, year) %>%
  distinct() %>%
  mutate(taxyear = year + 1,
         presale = taxyear - 1,
         postsale = taxyear + 1) %>%
  filter(year>2001 & year < 2020) 

#Adjust sales prices in Sales data

cpi_max <- cpi$value[which(cpi$date2==max(cpi$date2))]

SALES <- SALES %>%
  right_join(., cpi, by="date2") %>%
  filter(PRICE > 0) %>%                            #Limits to nominal price greater than 1K loss of 615 obs
  mutate(adj_price = PRICE * (cpi_max/value),
         lnadj_price = log(adj_price))

#Create main ownership data####

  for(i in year){
    own<-get(paste0("own_dat",i))
    
    own <- own %>%
      mutate(PARID = LOCATOR,
             year = i) 
    ifelse(i==2001,
           OWN <- own,
           OWN <- rbind(OWN, own ))
  }

 rm(own)
 rm(list = ls(pattern="^own_dat"))

OWN <- filter(OWN, !is.na(OWN_STATE))
OWN <- filter(OWN, !is.na(OWN_ZIP))
OWN <- filter(OWN, !is.na(PROP_ZIP))

#Pull in the ownership data for each sale####

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
           zip = as.numeric(OWN_ZIP.x != OWN_ZIP.y),
           Corp = Corporate.x - Corporate.y,
           Muni = Muni.x - Muni.y,
           Bank = Bank.x - Bank.y,
           Trus = Trustee.x - Trustee.y,
           Nonp = Nonprof.x - Nonprof.y,
           Hoa = Hoa.x - Hoa.y,
           Priv = private.x - private.y) %>%
    select(!matches("\\.[x]+")) %>%
    select(!matches("\\.[y]+" ))
  
  rm(work)
  
#Add Sales Count to Each PARID 
  c<-core %>%
    count(PARID) 
  
  core <- core %>%
    left_join(., c, by = "PARID") %>%
    arrange(SALESDT2, .keep_all = TRUE)
  
  rm(c)
