#This scripts use the buffer_list dataframes from bufflistX_X.RData created by Buffer_Create.R 
#Pulls up each list, joins with relevant data from OWN file to determined the share of neighbors of each
#sale that are owned according to various metrics.

#Jeremy R. Groves
#May 12, 2025
#June 17, 2025: Updated with new variable names with redone OWN file

#OUTPUT ./Build/Output/core.nbX_X.RData

rm(list=ls())

library(tidyverse)

load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/bufferlist1_4a.RData")

year <- seq(2009,2024,1)

#year <- 2009

for(yr in year){

  data.1 <- sales %>%
    select(PARID, saleyr) %>%
    distinct() %>%
    rename(base.parid = PARID) %>%
    filter(saleyr == yr)  %>%
    left_join(., buffer.list, by="base.parid")%>%
    filter(!is.na(PARID)) %>%
    filter(saleyr == yr)
  
  gc()
  
  load("./Build/Output/Own.RData")

  data.2 <- data.1 %>%
    rename(year = saleyr,
           parid = PARID) %>%
    left_join(., OWN, by = c("parid", "year")) %>%
    filter(!is.na(key))  %>%
    mutate(count = 1,
           owner = case_when(tenure == "OWNER" ~ 1,
                             TRUE ~ 0),
           renter = 1 - owner,
           inzip = case_when(co_zip == po_zip ~ 0,
                               TRUE ~ 1),
           outzip = 1 - inzip) %>%
    group_by(base.parid) %>%
    aggregate(cbind(po_livunit, corporate, trustee, reown, muni, nonprofit, hoa, private, 
                    owner, renter, inzip, outzip, count) ~ 
                base.parid, FUN = sum) %>%
    mutate(n_livunit = po_livunit / count,
           n_corporate = corporate / count,
           n_trustee = trustee / count,
           n_reown = reown / count,
           n_muni = muni / count,
           n_nonprofit = nonprofit / count,
           n_hoa = hoa / count,
           n_private = private / count,
           n_owner = owner / count,
           n_renter = renter / count,
           n_inzip = inzip / count, 
           n_outzip = outzip / count) %>%
    ungroup() %>%
    rename(parid = base.parid) %>%
    mutate(year = yr)
  
  ifelse(yr == 2009,
         core.nb <- data.2,
         core.nb <- rbind(core.nb, data.2))
  
}

save(core.nb, file = "./Build/Output/core.nb1_4.RData")

