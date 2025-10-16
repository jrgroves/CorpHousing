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

  temp <- core %>%
    select(parid, saleyr, starts_with("RP5"), starts_with("RP8")) %>%
    distinct()
  
  #Merge the sales and census data with the Wang neighbor files
  working <- core_cen %>%
    left_join(., temp, by = c("parid", "saleyr"))  %>%
    select(parid, yrblt, saleyr, adj_price, starts_with("pre_"), starts_with("post_"), starts_with("per_"),
           starts_with("RP5_"), starts_with("RP8_")) %>%
  #Turn Text into Dummies
    mutate(pre_type = case_when(pre_corporate == 1 ~ "Corporate",
                                 pre_private == 1 ~ "Private",
                                 TRUE ~ "Other"),
           post_type = case_when(post_corporate == 1 ~ "Corproate",
                                  post_private == 1 ~ "Private",
                                  TRUE ~ "Other"),
           ln_adj_price = log(adj_price)) %>%
    select(-c(pre_trustee, pre_nonprofit, pre_reown, pre_partnership, pre_hoa, pre_muni,pre_sale,pre_corporate, pre_private,
              post_trustee, post_nonprofit, post_reown, post_partnership, post_hoa, post_muni, post_sale,
              post_corporate, post_private)) %>%
    relocate(adj_price, ln_adj_price, saleyr, .before = per_own) %>%
    filter(pre_tenure != "BUILDER") %>%
    arrange(parid, saleyr)
  

#Create Difference data
  
  #Add Year Sales Dummy
  temp <- working %>%
    mutate(d_year = as.character(saleyr))%>%
    select(d_year) 
  
  t <- model.matrix(~d_year - 1, temp)
  
  working <- working %>%
    bind_cols(t)
  rm(t, temp)  
  
  core <- working %>%
    arrange(parid, saleyr) %>% 
    relocate(pre_type, post_type, .before = adj_price) %>%
    group_by(parid) %>%
      mutate(across(adj_price:d_year2023, ~.x - lag(.x)),
             tenure = paste(pre_tenure, post_tenure, sep = " - "),
             type = paste(pre_type, post_type, sep = " - ")) %>%
    ungroup() %>%
    filter(!is.na(adj_price)) %>%
    #Clean up data 
    select(-c(yrblt, starts_with("pre_"), starts_with("post_"))) %>%
    filter(saleyr != 0)
  
#Create dummies for tenures and types
  temp <- core %>%
    mutate(d_type = as.character(type))%>%
    select(d_type) 
  
  t.1 <- model.matrix(~d_type - 1, temp)
  
  temp <- core %>%
    mutate(d_tenure = as.character(tenure))%>%
    select(d_tenure) 
  
  t.2 <- model.matrix(~d_tenure - 1, temp)
   
#Rejoin to the core data
  core <- core %>%
    bind_cols(., t.1, t.2) %>%
    select(-c(tenure, type, parid)) 
  
  colnames(core) <- gsub(" - ", "2", colnames(core))
  
  

  #Regression Models
  
  variables <- colnames(core)
  depVar <- 'ln_adj_price'

  time.dum <- variables[grepl('d_year',variables)]
    time.drop <- c("d_year2011")
  time.dum <- paste(time.dum[!(time.dum %in% time.drop)], collapse = "+")
  
  types <- paste(variables[grepl('d_type', variables)], collapse = "+")
  tenure <- paste(variables[grepl('d_tenure', variables)], collapse = "+")
  
  census <- variables[grepl('per_', variables)] 
    cen.drop <- c("per_oth", "per_hs", "per_pov3")
  census <- paste(census[!(census %in% cen.drop)], collapse = "+")
  
  RP5 <-variables[grepl('RP5', variables)]
    RP5.drop <- c("RP5_distance", "RP5_ten.own", "RP5_other")
  RP5 <- paste(RP5[!(RP5 %in% RP5.drop)], collapse = "+")
  
  RP52 <-variables[grepl('RP5', variables)]
    RP5.drop2 <- c("RP5_distance", "RP5_ten.own", "RP5_other", "RP5_corporate", "RP5_private")
  RP52 <- paste(RP52[!(RP52 %in% RP5.drop2)], collapse = "+")
  
  RP8 <-variables[grepl('RP8', variables)]
    RP8.drop <- c("RP8_distance", "RP8_ten.own", "RP8_other")
  RP8 <- paste(RP8[!(RP8 %in% RP8.drop)], collapse = "+")
  
  RP82 <-variables[grepl('RP8', variables)]
    RP8.drop2 <- c("RP8_distance", "RP8_ten.own", "RP8_other", "RP8_corporate", "RP8_private")
  RP82 <- paste(RP82[!(RP82 %in% RP8.drop2)], collapse = "+")
  

  
  indepVars = paste(time.dum, RP5, census, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.1 <-lm(myModel,data=core)
  
  indepVars = paste(time.dum, RP52, census, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.2 <-lm(myModel,data=core)
  
  indepVars = paste(tenure, RP52, census, time.dum, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.3 <-lm(myModel,data=core)
  
  indepVars = paste(tenure, RP52, RP82, census, time.dum, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.4 <-lm(myModel,data=core)
  
  