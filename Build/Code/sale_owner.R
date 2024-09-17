#Uses annual ownership files to track the tenure of a sold property pre and post sale. 

#Uses: Sales.RData file created by Sales.R

#Saves: sal_own.RData - sales data with pre and post sale ownership.

#Jeremy R. Groves
#September 16, 2024

rm(list=ls())

library(tidyverse)
library(doParallel)


load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/Own.RData")

year<-seq(2001, 2020, 1)

#test<-  foreach(i=year) %dopar% {
#  library(foreign)
#  library(tidyverse)

#The following removes 679 observations
OWN <- filter(OWN, !is.na(OWN_STATE))
OWN <- filter(OWN, !is.na(OWN_ZIP))
OWN <- filter(OWN, !is.na(PROP_ZIP))

#Pull in the ownership data for each sale####

work <- SALES %>%
  mutate(year = presale) %>%
  left_join(., OWN, c("PARID", "year")) %>%
  mutate(OWN_STATE = as.character(OWN_STATE),
         OWN_ZIP = as.character(OWN_ZIP))

work <- work %>%
  mutate(year = postsale) %>%
  left_join(., OWN, c("PARID", "year")) %>%
  mutate(OWN_STATE.y = as.character(OWN_STATE.y),
         OWN_ZIP.y = as.character(OWN_ZIP.y))

#NOTE that there are about 32000 observations, especially with more recent sales, that do not show up in owner
#data for some reason.

core <- work %>%
  filter(!is.na(LOCATOR.x) & !is.na(LOCATOR.y)) %>%
  mutate(ten = as.numeric(TENURE.x!=TENURE.y),
         ten1 = case_when(TENURE.x == "NOT OWNER" & TENURE.y == "NOT OWNER" ~ 1,
                          TENURE.x == "OWNER" & TENURE.y == "NOT OWNER" ~ 2,
                          TENURE.x == "NOT OWNER" & TENURE.y == "OWNER" ~ 3,
                          is.na(TENURE.x) & !is.na(TENURE.y) ~ 5, #this is the case where a property shows up after sale (none exist)
                          !is.na(TENURE.x) & is.na(TENURE.y) ~ 6,
                          TRUE ~ 4),  #this is owner sold to owner
         own = as.numeric(OWNER_NAME.x != OWNER_NAME.y),
         state = as.numeric(OWN_STATE.x != OWN_STATE.y),
         zip = as.numeric(OWN_ZIP.x != OWN_ZIP.y),
         Corp = Corporate.x - Corporate.y,
         Muni = Muni.x - Muni.y,
         Bank = Bank.x - Bank.y,
         Trus = Trustee.x - Trustee.y,
         Nonp = Nonprof.x - Nonprof.y,
         Hoa = Hoa.x - Hoa.y,
         Priv = private.x - private.y) %>%
  select(!matches("\\.[x]+")) %>%
  select(!matches("\\.[y]+" ))

rm(work)

#Add Sales Count to Each PARID 
c<-core %>%
  count(PARID) 

core <- core %>%
  left_join(., c, by = "PARID") %>%
  arrange(SALEDT2, .keep_all = TRUE) %>%
  select(-c(year))

rm(c)

#Save Sales Data with ownership changes
save(core, file="./Build/Output/sal_own.RData")
