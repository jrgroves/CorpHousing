#This create the main repeat sales data set with the 1/8 mile buffer

#Jeremy R. Groves
    #Updated: May 9, 2025 - In this update I used the McSpatial processing for repeat sales observations with "0"
              #denoting the initial sale and "1" denoting the second.

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
             across(.cols = per_own:nb_prop_multi,
                    ~ .x,
                    .names = "{.col}1")) %>%
      group_by(parid) %>%
        mutate(price0 = lag(adj_price),
               saleyr0 = lag(saleyr),
               across(.cols = per_own:nb_prop_multi,
                      ~ lag(.x),
                      .names = "{.col}0")) %>%
      ungroup() %>%
      select(parid, ends_with("0"), ends_with("1"), dten, down) %>%
      filter(!is.na(price0))

  save (rs2_core1.8, file="./Analysis/Input/Core18.RData")
 