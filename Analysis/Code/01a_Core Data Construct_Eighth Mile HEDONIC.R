#This create the main hedonic sales data set with the 1/8 mile buffer

#Jeremy R. Groves
#Updated: May 9, 2025

rm(list = ls())

library(tidyverse)
library(gtsummary)

#Load Key Data and build core
load("./Build/Output/core_cen.RData")     #core_cen; contains sale owner and census data
load("./Build/Output/core_660.RData")    #core2 for 1/4 mile buffers
load("./Build/Output/saledwell.RData")  #core data with dwelling attributes

temp <- core2 %>%
  select(parid, saleyr, starts_with("nb")) %>%
  distinct() 

core <- core_cen %>%
  left_join(., temp, by = c("parid", "saleyr")) %>%
  select(-cen_yr, -year) %>%
  filter(!is.na(nb_owner)) %>%  #removes 20 observations from main data
  left_join(., sale_dwell, by = c("parid" = "PARID", "saleyr" = "TAXYR")) %>%
  filter(!is.na(MGFA)) %>% #This removes 1888 observations with about 1300 being new construction and the remaining missing values.
  mutate(across(RMFAM:REMBATH, ~replace_na(.x, 0)),
         WBFP_O = ifelse(WBFP_O < 0, abs(WBFP_O), WBFP_O),
         WBFP_PF = ifelse(WBFP_PF < 0, abs(WBFP_PF), WBFP_PF),
         BSMTCAR = ifelse(is.na(BSMTCAR), 0, BSMTCAR))

  
rm(core_cen, core2, temp, sale_dwell)

