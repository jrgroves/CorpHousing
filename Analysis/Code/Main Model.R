#This create the main data core for the analysis.


#Jeremy R. Groves
#Updated: May 9, 2025

rm(list = ls())

library(tidyverse)

#Load Key Data
 load("./Build/Output/sal_own.RData")
 load("./Build/Output/par_cen.RData")
 load("./Build/Output/core.nb1_4.RData")

#Repeat Sale Version
 
  rs <- core %>%
    filter(saleyr > 2009) %>% #To accommodate census data limits
    #First we filter out to keep only multiple sale properties and limit to one sale per year.
    arrange(PARID, saleyr) %>%
    add_count(PARID, saleyr, sort = TRUE) %>%
    filter(n == 1) %>%
    select(-n) %>%
    add_count(PARID) %>%
    filter(n > 1)
  
  t <- rs %>%
    select(saleyr) %>%
    mutate(saleyr = as.character(saleyr)) %>%
    model.matrix(~. -1,.) %>%
    as.data.frame()
  
  rs2 <- rs %>%
    cbind(., t)%>%
    #Now we removed unneeded columns
    select(-c(presale, postsale, starts_with("PREOWN_"), starts_with("POSTOWN_"))) %>%
    #Calculate the change in price between sales
    group_by(PARID) %>%
    arrange(PARID, saleyr) %>%
    mutate(ln_adj_price = log(adj_price),
           delta_p = ln_adj_price - lag(ln_adj_price),
           delta_syr2010 = saleyr2010 - lag(saleyr2010),
           delta_syr2011 = saleyr2011 - lag(saleyr2011),
           delta_syr2012 = saleyr2012 - lag(saleyr2012),
           delta_syr2013 = saleyr2013 - lag(saleyr2013),
           delta_syr2014 = saleyr2014 - lag(saleyr2014),
           delta_syr2015 = saleyr2015 - lag(saleyr2015),
           delta_syr2016 = saleyr2016 - lag(saleyr2016),
           delta_syr2017 = saleyr2017 - lag(saleyr2017),
           delta_syr2018 = saleyr2018 - lag(saleyr2018),
           delta_syr2019 = saleyr2019 - lag(saleyr2019),
           delta_syr2020 = saleyr2020 - lag(saleyr2020),
           delta_syr2021 = saleyr2021 - lag(saleyr2021),
           delta_syr2022 = saleyr2022 - lag(saleyr2022),
           delta_syr2023 = saleyr2023 - lag(saleyr2023)) %>%
    ungroup() %>%
    left_join(., core.nb, by = c("PARID", "saleyr" = "year")) %>%
    filter(!is.na(delta_syr2010),
           !is.na(count))
  
  
  
  mod1 <- lm(delta_p ~ P2C + C2C + C2P + N2O + N2N + O2N +
              LIVUNIT + Corporate + Trustee + Bank + Muni + Nonprof +
               owner + outzip + delta_syr2010 + delta_syr2011 +
               delta_syr2012 + delta_syr2013 + delta_syr2014 +
               delta_syr2015 + delta_syr2016 + delta_syr2017 +
               delta_syr2018 + delta_syr2019 + delta_syr2020 +
               delta_syr2021 + delta_syr2022, data=rs2)
 
mod2 <- lm(delta_p ~ P2C + C2C + C2P + N2O + N2N + O2N +
             P2C*N2O + P2C*N2N + P2C*O2N + C2P*N2O + C2P*N2N + C2P*O2N + 
             C2C*N2O + C2C*N2N + C2C*O2N + 
             LIVUNIT + Corporate + Trustee + Bank + Muni + Nonprof +
             owner + outzip + delta_syr2010 + delta_syr2011 +
             delta_syr2012 + delta_syr2013 + delta_syr2014 +
             delta_syr2015 + delta_syr2016 + delta_syr2017 +
             delta_syr2018 + delta_syr2019 + delta_syr2020 +
             delta_syr2021 + delta_syr2022, data=rs2)


