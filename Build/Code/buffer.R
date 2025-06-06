#This scripts use the buffer_list dataframes from bufflistX_X.RData created by Buffer_Create.R 
#Pulls up each list, joins with relevant year file and create neighbor values

#Jeremy R. Groves
#May 12, 2025

rm(list=ls())

library(tidyverse)

load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/bufferlist1_4a.RData")

year <- seq(2009,2012,1)

for(yr in year){

  data.1 <- sales %>%
    select(PARID, saleyr) %>%
    distinct() %>%
    rename(base.parid = PARID) %>%
    left_join(., buffer.list, by="base.parid", relationship = "many-to-many") %>%
    filter(!is.na(PARID)) %>%
    filter(saleyr == yr)
  
  gc()
  
  load("./Build/Output/Own.RData")
  
  data.2 <- OWN %>%
    filter(year == yr) %>%
    right_join(., data.1, by = "PARID") %>%
    filter(!is.na(LIVUNIT)) %>%
    mutate(count = 1,
           owner = case_when(TENURE == "OWNER" ~ 1,
                             TRUE ~ 0),
           renter = 1 - owner,
           inzip = case_when(OWN_ZIP == PROP_ZIP ~ 0,
                               TRUE ~ 1),
           outzip = 1 - inzip) %>%
    group_by(base.parid) %>%
    aggregate(cbind(LIVUNIT, Corporate, Trustee, Bank, Muni, Nonprof, Hoa, private, 
                    owner, renter, inzip, outzip, count) ~ 
                base.parid, FUN = sum) %>%
    mutate(LIVUNIT = LIVUNIT / count,
           Corporate = Corporate / count,
           Trustee = Trustee / count,
           Bank = Bank / count,
           Muni = Muni / count,
           Nonprof = Nonprof / count,
           Hoa = Hoa / count,
           private = private / count,
           owner = owner / count,
           renter = renter / count,
           inzip = inzip / count, 
           outzip = outzip / count) %>%
    ungroup() %>%
    rename(PARID = base.parid) %>%
    mutate(year = yr)
  
  ifelse(yr == 2009,
         core.nb <- data.2,
         core.nb <- rbind(core.nb, data.2))
  
}



