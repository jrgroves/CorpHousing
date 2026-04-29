#This create the main repeat sales data set with the 1/8 mile buffer

#Jeremy R. Groves
#Updated: May 9, 2025

rm(list = ls())

library(tidyverse)
library(gtsummary)

#Load Key Data and build core
load("./Build/Output/core_cen.RData")     #core_cen; contains sale owner and census data
load("./Build/Output/core_660.RData")    #core2 for 1/4 mile buffers

temp <- core2 %>%
  select(parid, saleyr, starts_with("nb"), starts_with("neig")) %>%
  distinct() 

core <- core_cen %>%
  left_join(., temp, by = c("parid", "saleyr")) %>%
  select(-cen_yr, -year) %>%
  filter(!is.na(nb_owner))  #removes 20 observations from main data

rm(core_cen, core2, temp)


#Repeat Sale Version
 
  rs_core <- core %>%
    filter(n == 2) 
  
#Difference between sales of properties.

    rs2_core1.8 <- rs_core %>%
      arrange(parid, saleyr) %>%
      mutate(saleyr1 = saleyr,
             price1 = adj_price,
             co1 = nb_corporate,
             leg1 = nb_legal,
             priv1 = nb_private,
             oth1 = nb_other,
             nzip1 = nb_nonzip,
             own1 = nb_owner,
             neigh1 = neighbors,
             neighlu1 = neighbor_lu,
             neighpc1 = neighbor_pclass,
             ten1 = case_when(pre_tenure == "OWNER" ~ 1,
                              TRUE ~ 0)) %>%
      group_by(parid) %>%
      mutate(saleyr0 = lag(saleyr),
             price0 = lag(adj_price),
             co0 = lag(nb_corporate),
             leg0 = lag(nb_legal),
             priv0 = lag(nb_private),
             oth0 = lag(nb_other),
             nzip0 = lag(nb_nonzip),
             own0 = lag(nb_owner),
             neigh0 = lag(neighbors),
             neighlu0 = lag(neighbor_lu),
             neighpc0 = lag(neighbor_pclass),
             ten0 = case_when(post_tenure == "OWNER" ~ 1,
                              TRUE ~ 0)) %>%
      ungroup() %>%
      select(ends_with("0"), ends_with("1"))  %>%
      filter(!is.na(price0))

  save (rs2_core1.8, file="./Analysis/Input/Core18.RData")
 