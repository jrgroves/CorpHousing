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

#Merge the cleaned sales data with the OWN data to find pre-sale owner information

  work <- sold %>%
    left_join(., OWN, by=c("parid", "pre_sale"="year")) %>%
    select(-c(key, fxc_stradr)) %>% #Remove unneeded columns
    rename_with(~str_c("pre_", .), corporate:muni) %>%
    rename(pre_tenure = tenure) %>%
    mutate(pre_tenure = case_when(is.na(pre_tenure) & seller == "BUILDER" ~ "BUILDER",
                                  TRUE ~ pre_tenure),
           across(.cols = pre_corporate:pre_muni,
                         ~ifelse(pre_tenure == "BUILDER",0, .)),
                                 pre_trustee = case_when(pre_tenure == "BUILDER" ~ 1,
                                 TRUE ~ pre_trustee ))
  
  #Need to pull in property city, zip, living unit, and coordinates from other installments in OWN data for new construction
  
  temp1 <- work %>%
    filter(is.na(po_zip)) %>%
    distinct(parid)
  
  temp2 <- OWN %>%
    filter(parid %in% temp1$parid) %>%
    filter(!is.na(po_zip)) %>%
    select(parid, starts_with("po_"), class, xcoord, ycoord) %>%
    distinct(parid, .keep_all = TRUE) %>%
    rename_with(~str_c("fix_", .), po_stradr:ycoord)
  
  #Put back the elements with the missing values
  
  work <- work %>%
    relocate(xcoord, ycoord, .after = class) %>%
    left_join(., temp2, by="parid") %>%
    mutate(across(po_stradr:ycoord, ~ coalesce(.x, get(paste('fix',cur_column(),sep = '_'))) )) %>%
    select(-starts_with("fix_")) %>%
    filter(!is.na(po_zip)) %>% #removes three observations with no data on property
    filter(!is.na(pre_tenure)) %>%
    rename_with(~str_c("pre_", .), co_city:co_zip)
  
  #There are 72 observations where the owner zip and address are missing, but name is present. 
      #This code need not be run more than once and it fixes the missing ownership data with external file.
      #temp.pre.own <- work %>%
      #  filter(is.na(pre_co_zip)) %>%
      #  filter(seller != "BUILDER")
      
      #write.csv(temp.pre.own, file="./Build/Input/temp_pre_own.csv")
      temp.pre.own <- read.csv(file="./Build/Input/temp_pre_own.csv", header = TRUE, as.is = TRUE)
      
      work <- work %>%
        left_join(., temp.pre.own, by = c("parid", "saleyr"), relationship = "many-to-many") %>%
        mutate(pre_co_city = na_if(pre_co_city, ""),
               pre_co_state = na_if(pre_co_state, ""),
               across(pre_co_city:pre_co_zip, ~coalesce(.x, get(paste('fix',cur_column(),sep = '_'))))) %>%
        select(-starts_with("fix_")) %>%
        distinct() %>%
        select(parid, starts_with("po_"), yrblt, saleyr, adj_price, post_sale, starts_with("pre_"))

  ##There are about 322 cases with no initial address and the data does not exist on the website either. The tax data
  #does not go back that far for the properties, even those sold after 2014. I removed these above. Additionally,
  #properties with a BUILDER have no co_zip at this point.
  

  #Next we pull in the post_sale information
  work1 <- work %>%
    left_join(., OWN, by=c("parid", "post_sale"="year")) %>%
    filter(post_sale != 2025) %>%
    select(-fxc_stradr)
  
    #There are 907 cases of missing data
  
        temp1 <- work1 %>%
          filter(is.na(tenure)) %>%
          distinct(parid, post_sale, .keep_all = T) %>%
          mutate(year = post_sale) %>%
          arrange(parid, year)
  
    #I am able to fill the missing owner data by using the next in the sequence
        
        temp2 <- OWN %>% 
          filter(parid %in% temp1$parid)  %>%
          select(-fxc_stradr) %>%
          bind_rows(., temp1) %>%
          arrange(parid, year) %>%
          group_by(parid) %>%
              fill(., c(co_stradr:key), .direction = "up") %>%   #NEED TO FILL ACROSS COLUMNS
          ungroup() %>%
          filter(!is.na(key)) %>% #One observations still missing information
          filter(post_sale != 2025) #This removes about 7200 cases where the sale occurs in 2024 and we do not know the buyer.
        
    #Rejoin the filled data with the original work after removing the elements in temp1
  
        work1 <- work1 %>%
          filter(!is.na(tenure)) %>%
          bind_rows(., temp2) %>%
          distinct() %>%
          mutate(ID = row_number())
     
        
        ##THis is where you stopped see notes for what to do.
        
           
    #There are 214 observations where the owner zip and address are missing, but name is present. 
    #This code need not be run more than once and it fixes the missing ownership data with external file.
        #temp.post.own <- work1 %>%
        #  relocate(ID) %>%
        #  mutate(co_city = na_if(co_city, ""),
        #         co_state = na_if(co_state, "")) %>%
        #  filter(is.na(co_city)) 
        
        #write.csv(temp.post.own, file="./Build/Input/temp_post_own.csv")
        temp.post.own <- read.csv(file="./Build/Input/temp_post_own.csv", header = TRUE, as.is = TRUE)
        
        work <- work1 %>%
          left_join(., temp.post.own, by = c("parid", "ID", "post_sale")) %>%
          mutate(co_city = na_if(co_city, ""),
                 co_state = na_if(co_state, ""))
        
        
        
                 across(co_city:co_zip, ~coalesce(.x, get(paste('fix',cur_column(),sep = '_'))))) %>%
          select(-starts_with("fix_")) %>%
          distinct() %>%
          select(-starts_with("po_"), -class, -xcoord, -ycoord) %>%
          rename_with(~str_c("post_", .), co_city:muni) %>%
          select(parid, starts_with("po_"), yrblt, saleyr, adj_price, starts_with("pre_"), starts_with("post_")) %>%
          filter(!is.na(post_co_zip))   #Drops 5 observations
        
#Now we create the working variables

    core <- work %>%
      filter(!is.na(pre_tenure)) %>%
      filter(!is.na(post_tenure)) %>%
      mutate(ten = as.numeric(pre_tenure != post_tenure),
             ten1 = case_when(pre_tenure == "BUILDER" & post_tenure == "NONOWNER" ~ 1,
                              pre_tenure == "NONOWNER" & post_tenure == "NONOWNER" ~ 1,
                              pre_tenure == "OWNER" & post_tenure == "NONOWNER" ~ 2,
                              pre_tenure == "NONOWNER" & post_tenure == "OWNER" ~ 3,
                              pre_tenure == "BUILDER" & post_tenure == "OWNER" ~ 3,
                              TRUE ~ 4), #This is owner to owner transactions
             new_con = case_when(pre_tenure == "BUILDER" ~ 1,
                                 TRUE ~ 0),
             city = as.numeric(pre_co_city != post_co_city),
             state = as.numeric(pre_co_state != post_co_state),
             zip = as.numeric(pre_co_zip != post_co_zip))
    
    
    
    ,
             P2C = ifelse(PREOWN_Private == 1 & POSTOWN_Private == 0, 1, 0),
             P2P = ifelse(PREOWN_Private == 1 & POSTOWN_Private == 1, 1, 0),
             C2C = ifelse(PREOWN_Private == 0 & POSTOWN_Private == 0, 1, 0),
             C2P = ifelse(PREOWN_Private == 0 & POSTOWN_Private == 1, 1, 0),
             N2O = ifelse(pre_tenure == "NONOWNER" & post_tenure == "OWNER",1,0),
             N2N = ifelse(pre_tenure == "NONOWNER" & post_tenure == "NONOWNER",1,0),
             O2O = ifelse(pre_tenure == "OWNER" & post_tenure == "OWNER",1,0),
             O2N = ifelse(pre_tenure == "OWNER" & post_tenure == "NONOWNER",1,0),
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
             trans.ten = case_when(N2O == 1 ~ "NONOWNER to Owner",
                                   N2N == 1 ~ "NONOWNER to NONOWNER",
                                   O2O == 1 ~ "Owner to Owner",
                                   O2N == 1 ~ "Owner to NONOWNER",
                                   TRUE ~ "Unknown"),
             trans.end.ten = case_when(N2O == 1 ~ "Owner",
                                       N2N == 1 ~ "NONOWNER",
                                       O2O == 1 ~ "Owner",
                                       O2N == 1 ~ "NONOWNER",
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
 

  