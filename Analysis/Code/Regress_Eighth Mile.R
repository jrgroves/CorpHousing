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
                              TRUE ~ 0))
  
#Define Variables 
  
  variables <- colnames(rs_core1.8)
  depVar <- 'ln_price'
  
  time.dum <- variables[grepl('d_year',variables)]
    time.drop <- c("d_year2011")
  time.dum <- paste(time.dum[!(time.dum %in% time.drop)], collapse = "+")
  
  census <- variables[grepl('per_', variables)] 
    cen.drop <- c("per_oth", "per_hs", "per_pov3", "per_own", "per_pov1")
  census <- paste(census[!(census %in% cen.drop)], collapse = "+")
    census <- (paste("income", census, sep = "+"))
  
  
  indepVars = time.dum
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.1 <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8)
  
  tab1a <- tbl_regression(mod.1,
                         intercept = TRUE,
                         estimate_fun = label_style_number(digits = 4))%>%
    add_significance_stars()%>%
    add_glance_table(include = c(r.squared, nobs))  %>%
    remove_abbreviation()%>%
    modify_header(label = "Variable", estimate = "Estimate",
                  std.error = "Std. Error")
  
  indepVars = paste(census,indepVars, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.2 <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8)
  
  tab1b <- tbl_regression(mod.2,
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
    
  indepVars = paste("nb_corporate", "nb_private", "nb_legal", "nb_other", indepVars, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.3 <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8) 
  tab2a <- tbl_regression(mod.3,
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
  
  mod.3a <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.end.ten == "Owner"))
  mod.3b <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.end.ten == "Not Owner"))
  
  tab2b<- tbl_regression(mod.3a,
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
  
  
  tab2c<- tbl_regression(mod.3b,
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
  
  tab2 <- tbl_merge(list(tab2a, tab2b,tab2c), tab_spanner = c("Full Sample","Owners", "Not Owners")) %>%
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
  
  mod.3c <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.end.own == "Private"))
  mod.3d <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.end.own == "Not Private"))
  
  tab2c<- tbl_regression(mod.3c,
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
  
  tab2d<- tbl_regression(mod.3d,
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
  
  tab3 <- tbl_merge(list(tab2a, tab2c,tab2d), tab_spanner = c("**Full Sample**", "**Private**", "**Not Private**")) %>%
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
  
  indepVars = paste("nb_nonzip","nb_owner",indepVars, sep = "+")
  myModel <- as.formula(paste(depVar,indepVars,sep = ' ~ '))
  mod.4 <-lm_robust(myModel, cluster = GEOID, data = rs_core1.8)
  
  tab4a<- tbl_regression(mod.4,
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
  
  mod.4a <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.end.ten == "Owner"))
  mod.4b <-lm_robust(myModel, cluster = GEOID, data = subset(rs_core1.8, trans.end.ten == "Not Owner"))
  
  tab4b<- tbl_regression(mod.4a,
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
  