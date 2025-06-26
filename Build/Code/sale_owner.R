#Uses annual ownership files and sales data to determine how the tenure and ownership information changes
#after the sale of each parcel. This file also creates some descriptive visualizations grouped by transaction
#type and changes in tenure.

#Uses: Sales.RData file created by Sales.R
#      OWN.RData file created by Owner Filter Fixing Parallel.R        

#Saves: sal_own.RData - sales data with pre and post sale ownership.

#Jeremy R. Groves
#Created: September 16, 2024
#Updated: September 18, 2024
#Updated: April 17, 2025: updated data and added visualization code.
#Updated: June 18, 2025: updated to use only post-2009 Owner Data due to census restrictions

rm(list=ls())

library(tidyverse)

load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/Own10.RData") 
load(file="./Build/Output/dwell.RData")

#Create wide data of ownership tenure####

  own.ten <- OWN %>%
    select(parid, year, tenure) %>%
    pivot_wider(id_cols = parid, 
                names_from = year, 
                names_prefix = "ten.",
                values_from = tenure)

#Create wide data of ownership corporate vs. private####

  own.pri <- OWN %>%
    select(parid, year, private) %>%
    pivot_wider(id_cols = parid, 
                names_from = year, 
                names_prefix = "priv.",
                values_from = private)

#Clean sales data####

  sold <- sales %>%
    select(PARID, saleyr, adj_price) %>%
    rename(parid = PARID) %>%
    filter(saleyr > 2010) %>%
    mutate(pre_sale = saleyr - 1,
           post_sale = saleyr + 1) %>%
  left_join(., dwelldat, by="parid", relationship = "many-to-many") %>%
  filter(card == 1) %>%
  relocate(parid, yrblt, saleyr) %>%
  mutate(seller = case_when((saleyr - yrblt) < 2 ~ "BUILDER",
                            TRUE ~ "OTHER")) %>%
  select(parid, saleyr, adj_price, pre_sale, post_sale, yrblt, seller)


  work <- sold %>%
    left_join(., OWN, by=c("parid", "pre_sale"="year")) %>%
    rename_with(~str_c("pre_", .), corporate:muni) %>%
    rename(pre_tenure = tenure) %>%
    mutate(pre_tenure = case_when(is.na(pre_tenure) & seller == "BUILDER" ~ "BUILDER",
                                  TRUE ~ pre_tenure),
           across(.cols = pre_corporate:pre_muni,
                         ~ifelse(pre_tenure == "BUILDER",0, .)),
           pre_trustee = case_when(pre_tenure == "BUILDER" ~ 1,
                                   TRUE ~ pre_trustee )) %>%
    select(parid, yrblt, saleyr, adj_price, post_sale, starts_with("pre_") ) %>%
    filter(!is.na(pre_tenure))
    
  
##There are about 331 cases with no initial address and the data does not exist on the website either. The tax data
  #does not go back that far for the properties, even those sold after 2014. I removed these above
  
 
  
  #Next we pull in the post_sale information
  work1 <- work %>%
    left_join(., OWN, by=c("parid", "post_sale"="year")) %>%
    filter(post_sale != 2025)
  
  temp1 <- work1 %>%
    filter(is.na(tenure)) %>%
    distinct(parid, .keep_all = T) %>%
    mutate(year = post_sale)
    
  temp2 <- OWN %>% 
    filter(parid %in% temp1$parid) %>%
    bind_rows(., temp1) %>%
    arrange(parid, year) %>%
    group_by(parid) %>%
    fill(, .direction = "up") %>%   #NEED TO FILL ACROSS COLUMNS
    ungroup()
    
    rename_with(~str_c("post_", .), corporate:muni) %>%
    rename(post_tenure = tenure) %>%
    select(parid, saleyr, adj_price, starts_with("pre_"), starts_with("post_")) %>%
    filter(post_sale != 2025) #This removes about 7200 cases where the sale occurs in 2024 and we do not know the buyer.
  

  
  
  
  
  
  %>%
    group_by(parid) %>%
    across(co_stradr:co_zip fill()
  
  
  head(temp2
       %>%
    filter(!is.na(pre_tenure),
           !is.na(post_tenure)) %>%
    mutate(tenure = paste(pre_tenure, post_tenure, sep="_"))
  

  
  
  
  
  
  
  

  work <- sold %>%
    mutate(year = presale) %>%
    left_join(., OWN, c("parid", "year")) %>%
    mutate(PREOWN_CITY = as.character(OWN_CITY),
           PREOWN_STATE = as.character(OWN_STATE),
           PREOWN_ZIP = as.character(OWN_ZIP),
           PREOWN_TENURE = as.character(TENURE),
           PREOWN_Private = as.character(private)) %>%
    select(PARID, saleyr, presale, postsale, starts_with("PRE"), adj_price)
  
  work <- work %>%
    mutate(year = postsale) %>%
    left_join(., OWN, c("PARID", "year")) %>%
    mutate(POSTOWN_CITY = as.character(OWN_CITY),
           POSTOWN_STATE = as.character(OWN_STATE),
           POSTOWN_ZIP = as.character(OWN_ZIP),
           POSTOWN_TENURE = as.character(TENURE),
           POSTOWN_Private = as.character(private)) %>%
    select(PARID, saleyr, presale, postsale, starts_with("PRE"), starts_with("POST"), adj_price)
  
    #NOTE that there are about 32000 observations, especially with more recent sales, that do not show up in owner
    #data for some reason.

    core <- work %>%
      filter(!is.na(PREOWN_TENURE)) %>%
      filter(!is.na(POSTOWN_TENURE)) %>%
      mutate(ten = as.numeric(PREOWN_TENURE!=POSTOWN_TENURE),
             ten1 = case_when(PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "NOT OWNER" ~ 1,
                              PREOWN_TENURE == "OWNER" & POSTOWN_TENURE == "NOT OWNER" ~ 2,
                              PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "OWNER" ~ 3,
                              is.na(PREOWN_TENURE) & !is.na(POSTOWN_TENURE) ~ 5, #this is the case where a property shows up after sale (none exist)
                              !is.na(PREOWN_TENURE) & is.na(POSTOWN_TENURE) ~ 6,
                              TRUE ~ 4),  #this is owner sold to owner
             city = as.numeric(PREOWN_CITY != POSTOWN_CITY),
             state = as.numeric(PREOWN_STATE != POSTOWN_STATE),
             zip = as.numeric(PREOWN_ZIP != POSTOWN_ZIP),
             P2C = ifelse(PREOWN_Private == 1 & POSTOWN_Private == 0, 1, 0),
             P2P = ifelse(PREOWN_Private == 1 & POSTOWN_Private == 1, 1, 0),
             C2C = ifelse(PREOWN_Private == 0 & POSTOWN_Private == 0, 1, 0),
             C2P = ifelse(PREOWN_Private == 0 & POSTOWN_Private == 1, 1, 0),
             N2O = ifelse(PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "OWNER",1,0),
             N2N = ifelse(PREOWN_TENURE == "NOT OWNER" & POSTOWN_TENURE == "NOT OWNER",1,0),
             O2O = ifelse(PREOWN_TENURE == "OWNER" & POSTOWN_TENURE == "OWNER",1,0),
             O2N = ifelse(PREOWN_TENURE == "OWNER" & POSTOWN_TENURE == "NOT OWNER",1,0),
             trans.own = case_when(P2C == 1 ~ "Private to Corporate",
                                   P2P == 1 ~ "Private to Private",
                                   C2C == 1 ~ "Corporate to Corporate",
                                   C2P == 1 ~ "Corporate to Private",
                                   TRUE ~ "Unknown"),
             trans.end.own = case_when(P2C == 1 ~ "Corporate",
                                       P2P == 1 ~ "Private",
                                       C2C == 1 ~ "Corporate",
                                       C2P == 1 ~ "Private",
                                       TRUE ~ "Unknown"),
             trans.ten = case_when(N2O == 1 ~ "Not Owner to Owner",
                                   N2N == 1 ~ "Not Owner to Not Owner",
                                   O2O == 1 ~ "Owner to Owner",
                                   O2N == 1 ~ "Owner to Not Owner",
                                   TRUE ~ "Unknown"),
             trans.end.ten = case_when(N2O == 1 ~ "Owner",
                                       N2N == 1 ~ "Not Owner",
                                       O2O == 1 ~ "Owner",
                                       O2N == 1 ~ "Not Owner",
                                       TRUE ~ "Unknown")) %>%
      filter(adj_price < 2500000)
    
    rm(work)

#Save Sales Data with ownership changes
  save(core, file="./Build/Output/sal_own.RData")
 
 
#Visualizations

  core.own <- core %>%
    select(adj_price, saleyr, trans.own, trans.ten) %>%
    group_by(trans.own, saleyr) %>%
    summarize(mean = mean(adj_price)/100000,
              n = n())%>%
    ungroup() %>%
    mutate(year = as.factor(saleyr))  %>%
    group_by(year) %>%
    mutate(n_per = n/sum(n)) %>%
    group_by(trans.own) %>%
    arrange(year) %>%
    mutate(diff.price = mean - lag(mean),
           diff.n = n - lag(n),
           diff.n_per = n_per - lag(n_per)) %>%
    ungroup()

  ggplot(core.own) +
    geom_point(aes(y = mean, x = year, color = trans.own)) +
    geom_line(aes(y = mean, x = year, color = trans.own, group = trans.own)) +
    labs(title = "Average Real Price by Transaction Type",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Real Dollars (in 100,000)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "Transaction Type"))
  
  ggplot(core.own) +
    geom_point(aes(y = n_per, x = year, color = trans.own)) +
    geom_line(aes(y = n_per, x = year, color = trans.own, group = trans.own)) +
    labs(title = "Share of Transactions by Transaction Type",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Percentage of Annual Transactions") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "Transaction Type"))
 
  core.own.end <- core %>%
    select(adj_price, saleyr, trans.end.own) %>%
    group_by(trans.end.own, saleyr) %>%
    summarize(mean = mean(adj_price)/100000,
              n = n())%>%
    ungroup() %>%
    mutate(year = as.factor(saleyr))  %>%
    group_by(year) %>%
    mutate(n_per = n/sum(n))
  
  ggplot(core.own.end) +
    geom_point(aes(y = mean, x = year, color = trans.end.own)) +
    geom_line(aes(y = mean, x = year, color = trans.end.own, group = trans.end.own)) +
    labs(title = "Average Real Price by Buyer Type",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Real Dollars (in 100,000)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "Buyer Type"))
  
  ggplot(core.own.end) +
    geom_point(aes(y = n_per, x = year, color = trans.end.own)) +
    geom_line(aes(y = n_per, x = year, color = trans.end.own, group = trans.end.own)) +
    labs(title = "Share of Transactions by Buyer Type",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Percentage of Annual Transactions") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") +
    guides(color = guide_legend(title = "Buyer Type"))

  core.ten <- core %>%
    select(adj_price, saleyr, trans.ten) %>%
    group_by(trans.ten, saleyr) %>%
    summarize(mean = mean(adj_price) / 100000,
              n = n())%>%
    ungroup() %>%
    mutate(year = as.factor(saleyr))%>%
    group_by(year) %>%
    mutate(n_per = n/sum(n))
  
  ggplot(core.ten) +
    geom_point(aes(y = mean, x = year, color = trans.ten))+
    geom_line(aes(y = mean, x = year, color = trans.ten, group = trans.ten)) +
    labs(title = "Average Real Price by Change in Tenure",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Real Dollars (in 100,000)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") 
  
  ggplot(core.ten) +
    geom_point(aes(y = n_per, x = year, color = trans.ten))+
    geom_line(aes(y = n_per, x = year, color = trans.ten, group = trans.ten)) +
    labs(title = "Share of Transactions by Change in Tenure",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Percentage of Annual Transactions") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") 
  
  core.end.ten <- core %>%
    select(adj_price, saleyr, trans.end.ten) %>%
    group_by(trans.end.ten, saleyr) %>%
    summarize(mean = mean(adj_price) / 100000,
              n = n())%>%
    ungroup() %>%
    mutate(year = as.factor(saleyr))%>%
    group_by(year) %>%
    mutate(n_per = n/sum(n))
  
  ggplot(core.end.ten) +
    geom_point(aes(y = mean, x = year, color = trans.end.ten))+
    geom_line(aes(y = mean, x = year, color = trans.end.ten, group = trans.end.ten)) +
    labs(title = "Average Real Price by Change in Buyer's Tenure",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Real Dollars (in 100,000)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") 
  
  ggplot(core.end.ten) +
    geom_point(aes(y = n_per, x = year, color = trans.end.ten))+
    geom_line(aes(y = n_per, x = year, color = trans.end.ten, group = trans.end.ten)) +
    labs(title = "Share of Transactions by Buyer's Tenure",
         caption = "Data from St. Louis County Assessor Records") +
    xlab("Year") +
    ylab("Percentage of Annual Transactions") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position="bottom") 
 

  