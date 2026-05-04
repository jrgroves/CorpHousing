#Uses annual ownership files and sales data to determine how the tenure and ownership information changes
#after the sale of each parcel. This file also creates some descriptive visualizations grouped by transaction
#type and changes in tenure.

#Uses: Sales.RData file created by Sales.R
#      OWN.RData file created by Owner Filter Fixing Parallel.R        

#Saves: sal_own.RData - sales data with pre and post sale ownership.

#Jeremy R. Groves
#Created: September 16, 2024
#Updated: September 18, 2024
#         April 17, 2025: updated data and added visualization code.
#         June 18, 2025: updated to use only post-2009 Owner Data due to census restrictions
#         July 8, 2025: using the post-2009 data I have filled in all owner data and completed clean
#         May 4, 2026: Removed much of the fixing of the ownership address, state, and zips due to errors and
#                      also put data into McSpaital repeat sales form in terms of pre and post


rm(list=ls())

library(tidyverse)

load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/Own10.RData") 
load(file="./Build/Output/dwell.RData")


#Clean sales data####
#Finding the Year Built for Housing in the sample to remove new construction from sales data

built <- dwelldat %>%
  filter(card == 1) %>%
  select(parid, yrblt)

sold <- sales %>%
  rename(parid = PARID) %>%
  left_join(., built, by="parid", relationship = "many-to-many") %>%
  relocate(parid, yrblt, saleyr) %>%
  filter(yrblt != saleyr |
           yrblt != postsale |
           yrblt != presale) %>%
  distinct() %>%
  #NOTE: The sales data contains 296,451 observations that the start of this process. Removing those homes with 
  #      sale dates equal to or within one year of the yearblt (new construction) removes 24,098 from this full
  #      sample.
  filter(saleyr > 2010) %>%
  #NOTE: Limiting the sample to post 2010 sales results in a loss of 83, 421 observations
  select(parid, yrblt, saleyr, saledate, adj_price, presale, postsale)



#Merge the cleaned sales data with the OWN data to find pre-sale and post-sale owner information####

working <- sold %>%
  left_join(., OWN, by=c("parid", "presale"="year")) %>%
  filter(!is.na(xcoord)) %>%
  relocate(starts_with("po_"), .after = adj_price) %>%
  select(-c(key, trustee, nonprofit, reown, partnership, hoa, muni, class, xcoord, ycoord)) %>% #Remove unneeded columns
  rename_with(~str_c(., "0"), co_stradr:other) 



#Next we pull in the post_sale information######
work1 <- working %>%
  left_join(., select(OWN, -starts_with("po_")), by=c("parid", "postsale"="year"))  %>%
  filter(postsale != 2025) %>%
  filter(!is.na(xcoord)) %>%
  select(-c(key, trustee, nonprofit, reown, partnership, hoa, muni, class, xcoord, ycoord)) %>% #Remove unneeded columns
  rename_with(~str_c(., "1"), co_stradr:other) %>%
  mutate(co_city1 = na_if(co_city1, ""),
         co_state1 = na_if(co_state1, "")) 

#Now we create the working variables
rm(working, dwelldat, built, sales, sold)

sales <- work1 %>%
  filter(!is.na(tenure0) | !is.na(tenure1))  %>%
  mutate(
    #First we create a variable tracking a change in tenure at the sale
    dten = as.numeric(tenure0 != tenure1),
    #Next we create a factor for the ownership type
    own0 = case_when(corporate0 == 1 ~ "Corporate",
                     private0 == 1 ~ "Private",
                     legal0 == 1 ~ "Legal",
                     TRUE ~ "Other"),
    own1 = case_when(corporate1 == 1 ~ "Corporate",
                     private1 == 1 ~ "Private",
                     legal1 == 1 ~ "Legal",
                     TRUE ~ "Other"),
    #Next we create an indicator for a change in ownership type
    down = as.numeric(own0 != own1)) %>%
  #NOTE: Remove outlyers on prices dropping 1142 with the lower bound and 433 with upper
  filter(adj_price > 9999 | adj_price < 2500000) %>% 
  add_count(parid)  #Add a count of the number of times a parcel sells in the data

#Save Sales Data with ownership changes
  save(sales, file="./Build/Output/sal_own.RData")


