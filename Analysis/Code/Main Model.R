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
   select(parid, saleyr, starts_with("nb")) %>%
   distinct() 
 
 core <- core_cen %>%
   left_join(., temp, by = c("parid", "saleyr")) %>%
   select(-cen_yr, -year) %>%
   filter(!is.na(nb_owner))  #removes 20 observations from main data
 
 rm(core_cen, core2, temp)
 

#Repeat Sale Version
 
  dummy <- core %>%
    mutate(d_year = as.character(saleyr)) %>%
    select(d_year) 
        
    t <- model.matrix(~d_year - 1, dummy)
        
    rs_core <- core %>%
      bind_cols(t)
    rm(t, dummy)  
    
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
          d_nb_private = nb_private - lag(nb_private),
          d_nb_private2 = d_nb_private^2,
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
      select(d_ln_price, d_adj_price, d_nb_corporate,  starts_with("per_"), starts_with("nb_"), N2N, N2O, O2N, O2O,
             P2P, P2C, C2P, C2C, ten1, trans.own, starts_with("d_"),) 

  #Basic Models
    
    variables <- colnames(rs_core2)
    depVar <- 'd_ln_price'
    
    time.dum <- variables[grepl('d_year',variables)]
       time.drop <- c("d_year2011")
    time.dum <- paste(time.dum[!(time.dum %in% time.drop)], collapse = "+")
    
    types <- paste(variables[grepl('d_type', variables)], collapse = "+")
    tenure <- paste(variables[grepl('d_tenure', variables)], collapse = "+")
    
    census <- variables[grepl('per_', variables)] 
        cen.drop <- c("per_oth", "per_hs", "per_pov3", "per_own")
    census <- paste(census[!(census %in% cen.drop)], collapse = "+")
    
    #Wang One Model
    indepVars = paste("nb_nonowner", time.dum, census, sep = "+")
    myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
    mod.1 <-lm(myModel,data=rs_core2)
    
    #Wang Two Model
    indepVars = paste("nb_nonowner", "nb_po_livunit", time.dum, census, sep = "+")
    myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
    mod.2 <-lm(myModel,data=rs_core2)
    
    
    
    
    
    
    indepVars = paste("P2C", "C2P", "C2C", "d_nb_corporate", "d_nb_private", time.dum, census, sep = "+")
    myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
    mod.3 <-lm(myModel,data=rs_core2)
    
    indepVars = paste("P2C", "C2P", "C2C", "d_nb_corporate","d_nb_corporate2", "d_nb_private", "d_nb_private2", time.dum, census, sep = "+")
    myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
    mod.4 <-lm(myModel,data=rs_core2)
    
    indepVars = paste("d_nb_corporate","d_nb_corporate2", "d_nb_private", "d_nb_private2", time.dum, census, sep = "+")
    myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
    mod.5a <-lm(myModel,data=filter(rs_core2, P2P == 1))

    mod.5b <-lm(myModel,data=filter(rs_core2, P2C == 1))

    mod.5c <-lm(myModel,data=filter(rs_core2, C2C == 1))
    mod.5d <-lm(myModel,data=filter(rs_core2, C2P == 1))
    