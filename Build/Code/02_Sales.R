#Processes the sales data file for all sales, uses BLS data to put prices into real terms

#Saves: Sales.RData - main sale data with adjusted prices

#Jeremy R. Groves
#June 27, 2024
#UPDATES:
    #August 5, 2024: Added BLS data for adjusted and cleaned up files.
    #April 8, 2025: Updated with 2024 EOY Sales file and read from external zip.
    #May 4, 2026: Updated with new BLS API library and methods; additionally I filtered through parcels that sold
                  #more than once in a year and removed all foreclosed units and kept the most recent if multiple
                  #valid sales were present.

rm(list=ls())

library(tidyverse)
library(foreign)
library(rjson)
library(blsR)

#Load in Data#####
#Set the year of the EOY file to extract from
i <- 2024

#Extract and Read
sales <- read.csv(unz(paste0("F:/Data/Saint Louis County Assessor Data/STLCOMO_REAL_ASMTROLL_EOY_",i,".zip"),
                     "sales.csv"), sep = "|", header = TRUE, stringsAsFactors = FALSE, quote = "")

#Get CPI data from BLS API#####

response <- get_series('series_id' = c('CUSR0000SA0'),
                       'start_year' = 2000,
                       'end_year' = 2019)

cpi <- data_as_table(response$data) %>%
  mutate(period = gsub("M", "", period),
         date = paste(period, "01", year, sep="-"),
         date2 = format(as.Date(date, format = "%m-%d-%Y"), "%m-%Y"),
         value = as.numeric(value)) %>%
  select(date2, value)

response <- get_series('series_id' = c('CUSR0000SA0'),
                       'start_year' = 2020,
                       'end_year' = 2024)

cpi2 <- data_as_table(response$data) %>%
  mutate(period = gsub("M", "", period),
         date = paste(period, "01", year, sep="-"),
         date2 = format(as.Date(date, format = "%m-%d-%Y"), "%m-%Y"),
         value = as.numeric(value)) %>%
  select(date2, value)

cpi <- rbind(cpi, cpi2)

rm(response, cpi2)

#Set parameters#####

  year<-seq(2001,2024)

#Clean Sales Data#####
  
  sales.tmp <- sales %>%
    mutate(SALEVAL = case_when(SALEVAL == ".X" ~ "X",
                               SALEVAL == "i" ~ "I",
                               SALEVAL == "x" ~ "X",
                               SALEVAL == " x" ~ "X",
                               SALEVAL == "t" ~ "T",
                               SALEVAL == "TT" ~ "T",
                               SALEVAL == " T" ~ "T",
                               TRUE ~ SALEVAL),
           saledate = as.Date(SALEDT, "%d-%b-%Y"),
           date2 = format(saledate, "%m-%Y"),   #for CPI merge
           saleyr = year(saledate),
           price = as.numeric(PRICE)) %>%
    filter(SALEVAL == "4" |
             SALEVAL == "5" |
             SALEVAL == "F" |
             SALEVAL == "I" |
             SALEVAL == "P" |
             SALEVAL == "X" |
             SALEVAL == "Z" |
             SALEVAL == "T") %>%
    filter(!is.na(price),
           price > 0,
           SOURCE == 1) %>%   #Keeps only buyer reported sales (removes about 146 sales)
    select(PARID, saledate, date2, price, saleyr, SALETYPE, SALEVAL, SOURCE) %>%
    distinct() %>%
    filter(saleyr > 2001 ) %>%
    filter(saleyr < 2025)  %>%
  #Remove foreclosed properties completely from set
      group_by(PARID, saleyr) %>%
          mutate(flag = any(SALEVAL == "5"),
                 count = n()) %>%
      ungroup() %>%
      filter(flag == "FALSE") %>%
      select(-flag) %>%
  #Keep only VALID sales in multi-year sales
    filter(case_when(
      count > 1 ~ SALEVAL == "X",
      TRUE ~ TRUE)) %>%
    group_by(PARID, saleyr) %>%
        mutate(count = n()) %>%
    ungroup() %>%
  slice_max(order_by = saledate, by = c(PARID, saleyr))%>%
  group_by(PARID, saleyr) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count == 1) %>%
  select(-count) %>%
    mutate(presale = saleyr - 1,
           postsale = saleyr + 1)

#Adjust sales prices in Sales data
  
  cpi_max <- cpi$value[which(cpi$date2==max(cpi$date2))]
  
  sales <- sales.tmp %>%
    right_join(., cpi, by="date2") %>%
    filter(price > 1000) %>%                            #Limits to nominal price greater than 1K loss of 615 obs
    mutate(adj_price = price * (cpi_max/value),
           lnadj_price = log(adj_price))

#Save main Sales Data

save(sales, file="./Build/Output/Sales.RData")

