#This create the main repeat sales data set with the 1/2 mile buffer

#Jeremy R. Groves
#Updated: March 9, 2026

rm(list = ls())

library(tidyverse)

#Load Key Data and build core
load("./Build/Output/core_cen.RData")     #core_cen; contains sale owner and census data
load("./Build/Output/core_2640.RData")    #core2 for 1/2 mile buffers

temp <- core2 %>%
  select(parid, saleyr, starts_with("nb")) %>%
  distinct() 

core <- core_cen %>%
  left_join(., temp, by = c("parid", "saleyr")) %>%
  select(-cen_yr, -year) %>%
  filter(!is.na(nb_owner))  #removes 20 observations from main data

rm(core_cen, core2, temp)


#Repeat Sale Version
 
  rs_core <- core %>%
    filter(n > 1) 
  #Add Year Sales Dummy
    temp <- rs_core %>%
      mutate(d_year = as.character(saleyr))%>%
      select(d_year) 
        
    t <- model.matrix(~d_year - 1, temp)
        
    rs_core <- rs_core %>%
      bind_cols(t)
    rm(t, temp)  
    
#Difference between sales of properties.

    rs_core1.2 <- rs_core %>%
      arrange(parid, saleyr) %>%
      mutate(ln_price = log(adj_price),
             d_saleyr = saleyr) %>%
      filter(new_con == 0) %>%
      select(-c(starts_with("po_"), starts_with("pre_"), starts_with("post_"), new_con, ten, 
                P2NP, P2P, NP2NP, NP2P, N2O, N2N, O2O, O2N,n)) %>%
      relocate(adj_price, .after=ln_price) %>%
      arrange(parid, saleyr) %>%
      group_by(parid) %>%
      mutate(across(per_own:d_saleyr, ~.x - lag(.x))) %>%
      ungroup() %>%
      filter(!is.na(adj_price),
             !is.infinite(ln_price),
             adj_price > -200000 & adj_price < 200000,
             d_saleyr != 0,
             !is.na(ll_state))
       
  save (rs_core1.2, file="./Analysis/Input/Core12.RData")
 