#Wang Model Replications (or close to it)

#Jeremy R. Groves
#September 23, 2025

rm(list=ls())
gc()

library(tidyverse)
library(gtsummary)
library(gt)

#Load Key Data and build core
  load("./Build/Output/core_cen.RData")     #core_cen; contains sale owner and census data
  load("./Build/Output/wang.RData")
  source("./Analysis/Code/Function/rep.sale.R")

#Build Repeat Sales Data
  #Merge the sales and census data with the Wang neighbor files
  working <- core_cen %>%
    left_join(., wang5, by = c("parid" = "name", "saleyr")) %>%
    left_join(., wang8, by = c("parid" = "name", "saleyr")) %>%
    filter(!is.na(sales.x)) %>%
    select(parid, yrblt, saleyr, adj_price, starts_with("pre_"), starts_with("post_"), 
                
  
  
  # Pull out the sales data to know how many times a property sold over the sample
  temp.sale <- core.cen %>%
    group_by()
    
  
  
  
    
  temp <- core_RP5 %>%
    select(ID, starts_with("nb"))
  
  core.5 <- core_cen %>%
    left_join(., temp, by = "ID") %>%
    select(-cen_yr, -year)  %>%
    filter(!is.na(nb_nonres))  #removes 623 observations from main data

  temp <- core_RP8 %>%
    select(ID, starts_with("nb"))
  
  core.8 <- core_cen %>%
    left_join(., temp, by = "ID") %>%
    select(-cen_yr, -year)  %>%
    filter(!is.na(nb_nonres))  #removes 653 observations from main data
  
  rm(core_cen, core_RP5, core_RP8, temp)
  
  
#Repeat Sale Version
    rs_temp <- core.5 %>%
      filter(n > 1) 
    
    #Add Year Sales Dummy
      temp <- rs_temp %>%
        mutate(d_year = as.character(saleyr))%>%
        select(d_year) 
      
      t <- model.matrix(~d_year - 1, temp)
      
      rs_temp <- rs_temp %>%
        bind_cols(t)
      rm(t, temp)  
  
  #Difference between sales of properties.
      rs_core.5 <- rs_temp %>%
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
               P2P, P2C, C2P, C2C, ten1, trans.own, starts_with("d_"), starts_with("nb_")) 

    
#Regressions  
    #Basic Indices
      #Assign Data to Core and set Index
      core <- rs_core.5
      index <- c(names(select(core, starts_with("d_year"), -d_year2011)))
      
      #Set up the levels and regression parameters
      set <- levels(factor(core$ten1))
      color.set <- c("Nonowner to Nonowner" = "blue", "Owner to Nonowner" = "lightblue", 
                     "Nonowner to Owner" = "red", "Owner to Owner" = "darkred")
      
      predictors <- c(index, "d_nb_corporate2", "per_blk", "per_asn", "per_oth", "per_u16", "per_o65")
      
      my_formula <- as.formula(paste("d_ln_price ~ -1 + ", paste(predictors, collapse = " + ")))
      
      #Run Regressions for each level
      for(i in set){
        
        d.temp <- core %>%
          filter(ten1 == i) %>%
          select(d_ln_price, d_saleyr, eval(predictors)) 
        
        mod.1 <- rep.sale(my_formula, d.temp)
        
        ci.temp <- as.data.frame(confint(mod.1)) %>%
          mutate(var = row.names(.)) %>%
          remove_rownames() %>%
          set_names(c("lci", "hci", "var"))
        
        c.temp <- as.data.frame(mod.1$coefficients) %>%
          mutate(var = row.names(.)) %>%
          remove_rownames() %>%
          set_names(c("est", "var")) %>%
          full_join(., ci.temp, by = "var") %>%
          filter(var %in% index)%>%
          mutate(mod = i,
                 year = as.numeric(substr(var,7,10)))
        
        ifelse(i == set[[1]],
               p.data <- c.temp,
               p.data <- bind_rows(p.data, c.temp))
        
        rm(ci.temp, c.temp, d.temp)
      }
      
      
      
      plot.1 <- ggplot(data = p.data) +
        geom_line(aes(x = year, y = est, color = mod), linewidth = 1) +
        geom_line(aes(x = year, y = lci, color = mod), linetype = "dashed") +
        geom_line(aes(x = year, y = hci, color = mod), linetype = "dashed") +
        geom_hline(yintercept = 0, color = "black", linewidth = 1.5) +
        scale_x_continuous(
          breaks = p.data$year, # Set breaks at each year in your data
          labels = as.character(p.data$year) # Ensure labels are character
        )+
        scale_color_manual(values = color.set) +
        labs(color = "Transaction Type",
             x = "Year",
             y = "Price Index") +
        theme_bw() +
        theme(axis.text.x=element_text(angle=60, hjust=1),
              legend.position="bottom") 
      
  
  
  