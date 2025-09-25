#This create the main data core for the analysis.

#Jeremy R. Groves
#Updated: May 9, 2025

rm(list = ls())

library(tidyverse)
library(gtsummary)

#Load Key Data and build core
 load("./Build/Output/core_cen.RData")     #core_cen; contains sale owner and census data
 load("./Build/Output/core_1320.RData")    #core2
 
 temp <- core2 %>%
   select(ID, neighbors, starts_with("nb"))
 
 core <- core_cen %>%
   left_join(., temp, by = "ID") %>%
   select(-cen_yr, -year) %>%
   filter(!is.na(neighbors))  #removes 20 observations from main data
 
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
    rs_core2 <- rs_core %>%
      arrange(parid, saleyr) %>%
      mutate(ln_price = log(adj_price)) %>%
      group_by(parid) %>%
      mutate(
        d_saleyr = saleyr - lag(saleyr),
        d_adj_price = adj_price - lag(adj_price),
        d_ln_price = ln_price - lag(ln_price),
        d_nb_corporate = nb_corporate - lag(nb_corporate),
        d_nb_corporate2 = d_nb_corporate^2,
        from = lag(pre_tenure),
        to = post_tenure,
        across(per_own:d_year2023, ~.x - lag(.x))) %>%
      ungroup() %>%
      filter(!is.na(d_adj_price),
             !is.infinite(d_ln_price),
             from != "BUILDER",
             d_adj_price > -200000 & d_adj_price < 200000,
             d_nb_corporate > -.40,
             d_saleyr != 0) %>%
      select(d_ln_price, d_adj_price, d_nb_corporate, starts_with("per_"), N2N, N2O, O2N, O2O,
             P2P, P2C, C2P, C2C, ten1, trans.own, starts_with("d_"),) 
 
  save (rs_core, rs_core2, file="./Analysis/Input/Core.RData")
  