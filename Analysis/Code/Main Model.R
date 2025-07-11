#This create the main data core for the analysis.


#Jeremy R. Groves
#Updated: May 9, 2025

rm(list = ls())

library(tidyverse)

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
    
  #Difference between sales
    rs_core2 <- rs_core %>%
      arrange(parid, saleyr) %>%
      group_by(parid) %>%
        mutate(
               ln_price = log(adj_price),
               adj_price = lead(adj_price) - adj_price,
               ln_price = lead(ln_price) - ln_price,
               across(nb_livunit:d_year2023, ~lead(.x) - .x)) %>%
      ungroup() %>%
      filter(!is.na(adj_price)) %>%
      select(adj_price, ln_price, starts_with("nb_"), new_con, N2O, N2N, O2O, O2N, P2C, P2P, C2C, C2P, trans.own, ten1,
             starts_with("d_")) %>%
      select(-nb_livunit)
    
  #Visualizations
    plot1 <- ggplot(rs_core2,  aes(factor(ten1), ln_price, fill=factor(trans.own))) +
      geom_boxplot()
  
  #Model 1  
      
    mod1 <- lm(ln_price ~ ., data = select(rs_core2, ln_price, new_con, O2N, N2N, N2O, starts_with("d_")))
    mod2 <- lm(ln_price ~ ., data = select(rs_core2, ln_price, new_con, P2C, C2P, C2C, starts_with("d_")))
    
    
