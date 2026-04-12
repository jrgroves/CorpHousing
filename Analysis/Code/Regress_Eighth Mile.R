# This file creates regressions

rm(list=ls())

library(tidyverse)
library(gtsummary)
library(gt)
library(estimatr)
library(flextable)

reg<- set_flextable_defaults(font.family = "Times")
   

#Load Data

  load("./Analysis/Input/Core18.RData")
  rs_core1.8 <- rs_core1.8 %>%
    mutate(income = income / 1000,
           post19 = case_when(saleyr > 2018 ~ 1,
                              TRUE ~ 0),
           trans.ten = case_when(trans.ten=="Not Owner to Not Owner" ~ "N2N",
                                 trans.ten=="Not Owner to Owner" ~  "N2O",
                                 trans.ten=="Owner to Not Owner" ~ "O2N",
                                 trans.ten=="Owner to Owner" ~ "O2O",
                                 TRUE ~ NA),
           trans.ten = factor(trans.ten, levels = c("O2O", "N2N", "N2O", "O2N")))
  
#Define Variables 
  
  variables <- colnames(rs_core1.8)
  depVar <- 'ln_price'
  
  time.dum <- variables[grepl('d_year',variables)]
    time.drop <- c("d_year2011")
  time.dum <- paste(time.dum[!(time.dum %in% time.drop)], collapse = "+")
  
  census <- variables[grepl('per_', variables)] 
    cen.drop <- c("per_oth", "per_hs", "per_pov3", "per_own", "per_pov2")
  census <- paste(census[!(census %in% cen.drop)], collapse = "+")
    census <- (paste("income", census, sep = "+"))
  
#Base Model  
  indepVars = paste(time.dum, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.1a <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8)
  
  tab1a <- tbl_regression(mod.1a,
                         intercept = TRUE,
                         estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation()%>%
    modify_header(label = "Variable", estimate = "Estimate",
                  std.error = "Std. Error")
  
  indepVars = paste(census,indepVars, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.1b <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8)
  
  tab1b <- tbl_regression(mod.1b,
                          intercept = TRUE ,
                          estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation()  %>%
    modify_header(label = "Variable", estimate = "Estimate",
                  std.error = "Std. Error")
  
  tab1 <- tbl_merge(list(tab1a, tab1b), tab_spanner = c("Column One", "Column Two"), quiet = TRUE) %>%
    modify_table_body(
      ~.x %>% 
        dplyr::arrange(
          row_type == "glance_statistic")
    ) %>%
    as_flex_table() %>%
    add_footer_lines("Standard Errors Clustered as Census Tract") %>%
    add_header_lines("Table One: Full Sample") %>%
    bold(part = "header", i = 1) %>%
    align(align = "center", part = "header") %>%
    fontsize(part = "header", size = 14) %>%
    fontsize(part = "body", size = 11) %>%
    fontsize(part = "footer", size = 9) %>%
    line_spacing(part = "header", space = 1.2) %>%
    line_spacing(part = "body", space = 0.75) %>%
    line_spacing(part = "footer", space = 0.5) 


#Main Model 
    
  indepVars = paste("nb_corporate", "nb_private", "nb_legal", "nb_other", time.dum,
                    census, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.2a <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8) 
  
  tab2a <- tbl_regression(mod.2a,
                 include = c(starts_with("nb_")),
                 intercept = TRUE,
                 label = list(nb_corporate = "Share Corporate Owned",
                              nb_private = "Share Private Owned",
                              nb_legal = "Share Legal Owned",
                              nb_other = "Share Other Owned",
                              nb_owner = "Share Owner Occupied"),
                 estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation()%>%
    modify_header(label = "Changes in:", estimate = "Estimate",
                  std.error = "Std. Error")
  
  indepVars = paste("nb_corporate", "nb_private", "nb_legal", "nb_other", "trans.ten",
                    time.dum, census, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.2b <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8)
  
  
  tab2b<- tbl_regression(mod.2b,
                 include = c(starts_with("nb_"), starts_with("trans.")),
                 intercept = TRUE,
                 label = list(trans.ten = "Tenure Change",
                              nb_corporate = "Share Corporate Owned",
                              nb_private = "Share Private Owned",
                              nb_legal = "Share Legal Owned",
                              nb_other = "Share Other Owned",
                              nb_owner = "Share Owner Occupied"),
                 estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation() %>%
    modify_header(label = "Changes in:", estimate = "Estimate",
                  std.error = "Std. Error")

  mod.2c <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.end.own == "Private"))
  
  tab2c<- tbl_regression(mod.2c,
                         include = c(starts_with("nb_"), starts_with("trans.")),
                         intercept = TRUE,
                         label = list(trans.ten = "Tenure Change",
                                      nb_corporate = "Share Corporate Owned",
                                      nb_private = "Share Private Owned",
                                      nb_legal = "Share Legal Owned",
                                      nb_other = "Share Other Owned",
                                      nb_owner = "Share Owner Occupied"),
                         estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation()%>%
    modify_header(label = "Changes in:", estimate = "Estimate",
                  std.error = "Std. Error")
  
  
  
  tab2 <- tbl_merge(list(tab2a, tab2b,tab2c), tab_spanner = c("**Model One**", "**Model Two**",
                                                              "**Model Three**")) %>%
    modify_table_body(
      ~.x %>% 
        dplyr::arrange(
          row_type == "glance_statistic")
    ) %>%
    as_flex_table() %>%
    add_footer_lines("Standard Errors Clustered as Census Tract") %>%
    add_header_lines("Table Three: One Eighth Mile Radius") %>%
    bold(part = "header", i = 1) %>%
    align(align = "center", part = "header") %>%
    fontsize(part = "header", size = 14) %>%
    fontsize(part = "body", size = 11) %>%
    fontsize(part = "footer", size = 9) %>%
    line_spacing(part = "header", space = 1.2) %>%
    line_spacing(part = "body", space = 0.75) %>%
    line_spacing(part = "footer", space = 0.5) 
  
  
  
  
  
  
  
  
  
  indepVars = paste("nb_corporate", "nb_private", "nb_legal", "nb_other", time.dum, census, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  
  mod.4a <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8)



  mod.4b <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.ten == "N2O"))
  
  tab4b<- tbl_regression(mod.4b,
                         include = c(starts_with("nb_")),
                         intercept = TRUE,
                         label = list(nb_corporate = "Share Corporate Owned",
                                      nb_private = "Share Private Owned",
                                      nb_legal = "Share Legal Owned",
                                      nb_other = "Share Other Owned",
                                      nb_owner = "Share Owner Occupied",
                                      nb_nonzip = "Share Owner not in Zip Area"),
                         estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation()%>%
    modify_header(label = "Changes in:", estimate = "Estimate",
                  std.error = "Std. Error")
  
  tab4c<- tbl_regression(mod.4b,
                         include = c(starts_with("nb_")),
                         intercept = TRUE,
                         label = list(nb_corporate = "Share Corporate Owned",
                                      nb_private = "Share Private Owned",
                                      nb_legal = "Share Legal Owned",
                                      nb_other = "Share Other Owned",
                                      nb_owner = "Share Owner Occupied",
                                      nb_nonzip = "Share Owner not in Zip Area"),
                         estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation()%>%
    modify_header(label = "Changes in:", estimate = "Estimate",
                  std.error = "Std. Error")
  
  
  tab4 <- tbl_merge(list(tab4a, tab4b,tab4c), tab_spanner = c("**Full Sample**", "**Owners**", "**Not Owners**")) %>%
    modify_table_body(
      ~.x %>% 
        dplyr::arrange(
          row_type == "glance_statistic")
    ) %>%
    as_flex_table() %>%
    add_footer_lines("Standard Errors Clustered as Census Tract") %>%
    add_header_lines("Table One: Full Sample") %>%
    bold(part = "header", i = 1) %>%
    align(align = "center", part = "header") %>%
    fontsize(part = "header", size = 14) %>%
    fontsize(part = "body", size = 11) %>%
    fontsize(part = "footer", size = 9) %>%
    line_spacing(part = "header", space = 1.2) %>%
    line_spacing(part = "body", space = 0.75) %>%
    line_spacing(part = "footer", space = 0.5) 
  
  save_as_docx(tab1, tab2, tab3, tab4, path = "./Analysis/Output/Eighth Tables.docx")
  
 