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
#Updated: July 8, 2025: using the post-2009 data I have filled in all owner data and completed clean

rm(list=ls())

library(tidyverse)

load(file="./Build/Output/Sales.RData")
load(file="./Build/Output/Own10.RData") 
load(file="./Build/Output/dwell.RData")


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
                                 TRUE ~ pre_trustee ),
           co_city = na_if(co_city, ""),
           co_state = na_if(co_state, ""))
  
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
      
  #There are 251 observations where the owner zip and address are missing, but name is present. 
      #This code need not be run more than once and it fixes the missing ownership data with external file.
      
    #Create a list of unique owner names with city, state, and zip from the core OWN file
      own <- OWN %>%
        select(starts_with("co_")) %>%
        distinct(co_name, .keep_all = TRUE) %>%
        mutate(co_city = na_if(co_city, ""),
               co_state = na_if(co_state, "")) %>%
        filter(!is.na(co_name),
               !is.na(co_zip),
               !is.na(co_state),
               !is.na(co_city)) %>%
        select(-co_stradr)
      
    #Join this list to the main and then coalesce to fill in NAs
      work1 <- work %>%
        left_join(., own, by="co_name") %>%
        mutate(pre_co_city = coalesce(pre_co_city, co_city),
               pre_co_state = coalesce(pre_co_state, co_state),
               pre_co_zip = coalesce(pre_co_zip, co_zip),
               ID = row_number()) %>%
        select(-co_state, -co_city, -co_zip) 
        
      
    #We have reduced it down to 85 that have to be manually looked up and added back.
      #temp.pre.own <- work1 %>%
      #  filter(is.na(pre_co_city) | is.na(pre_co_zip) | is.na(pre_co_state)) %>%
      #  filter(pre_tenure != "BUILDER") %>%
      #  select(ID, parid, pre_sale, co_name, starts_with("pre_co_"))
     
      #write.csv(temp.pre.own, file="./Build/Input/temp_pre_own.csv")
      
      temp.pre.own <- read.csv(file="./Build/Input/temp_pre_own.csv", header = TRUE, as.is = TRUE)
      
          #We make a sub list of the manually added information to tack onto bottom of the own dataframe
          temp <- temp.pre.own %>%
            mutate(co_city = fix_pre_co_city,
                   co_state = fix_pre_co_state,
                   co_zip = fix_pre_co_zip) %>%
            select(starts_with("co_")) %>%
            distinct()
          
          own <- own %>%
            bind_rows(., temp)
          
          rm(temp)
      
      #Read in the text file and use to filling mising values
      temp.pre.own <- select(temp.pre.own, -co_name)
      
      work <- work1 %>%
        left_join(., temp.pre.own, by = c("parid", "ID")) %>%
        mutate(across(pre_co_city:pre_co_zip, ~coalesce(.x, get(paste('fix',cur_column(),sep = '_'))))) %>%
        select(-starts_with("fix_"))  %>%
        select(parid, starts_with("po_"), yrblt, saleyr, adj_price, post_sale, starts_with("pre_"))

#Next we pull in the post_sale information
  work1 <- work %>%
    left_join(., select(OWN, -starts_with("po_")), by=c("parid", "post_sale"="year"))  %>%
    filter(post_sale != 2025) %>%
    select(-fxc_stradr) %>%
    select(-co_stradr) %>%
    mutate(co_city = na_if(co_city, ""),
           co_state = na_if(co_state, ""),
           ID = row_number())
  
    #There are 1123 cases of missing data
  
        temp1 <- work1 %>%
          filter(is.na(tenure) | is.na(co_city) | is.na(co_state) | is.na(co_zip)) %>%
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
              fill(., c(co_name:key), .direction = "up") %>%   #NEED TO FILL ACROSS COLUMNS
          ungroup() %>%
          filter(!is.na(key)) %>% #One observations still missing information
          filter(post_sale != 2025) #This removes about 7200 cases where the sale occurs in 2024 and we do not know the buyer.
        
    #Rejoin the filled data with the original work after removing the elements in temp1
  
        work2 <- work1 %>%
          filter(! ID %in% temp2$ID) %>%
          bind_rows(., temp2) %>%
          distinct() %>%
          select(-co_stradr, -year, -class) %>%
          mutate(co_city = na_if(co_city, ""),
                 co_state = na_if(co_state, "")) %>%
          rename_with(~str_c("post_", .), co_city:co_zip) %>%
          left_join(., own, by="co_name") %>%
          mutate(post_co_city = coalesce(post_co_city, co_city),
                 post_co_state = coalesce(post_co_state, co_state),
                 post_co_zip = coalesce(post_co_zip, co_zip)) %>%
          select(-c(co_city, co_state, co_zip))
        
    #There are 87 observations where the owner zip and address are missing, but name is present. 
    #This code need not be run more than once and it fixes the missing ownership data with external file.
        #temp.post.own <- work2 %>%
        #  filter(is.na(post_co_city) | is.na(post_co_state) | is.na(post_co_zip)) 
        
        #write.csv(temp.post.own, file="./Build/Input/temp_post_own.csv")
        temp.post.own <- read.csv(file="./Build/Input/temp_post_own.csv", header = TRUE, as.is = TRUE)
        
        temp.post.own <- select(temp.post.own, -co_name)
        
        work1 <- work2 %>%
          left_join(., temp.post.own, by = c("parid", "ID")) %>%
          mutate(across(post_co_city:post_co_zip, ~coalesce(.x, get(paste('fix',cur_column(),sep = '_'))))) %>%
          select(-starts_with("fix_")) %>%
          filter(!is.na(corporate)) %>%  #this removes 27 items where the post tenure type is missing
          select(-xcoord, -ycoord, -key) %>%
          rename_with(~str_c("post_", .), tenure:muni) %>%
          select(ID, parid, starts_with("po_"), yrblt, saleyr, adj_price, starts_with("pre_"), starts_with("post_")) %>%
          filter(!is.na(post_co_zip))   #Drops 4 observations
        
#Now we create the working variables

    core <- work1 %>%
      filter(!is.na(pre_tenure)) %>%
      filter(!is.na(post_tenure)) %>%
      mutate(
        #First we create a variable tracking a change in tenure at the sale
        ten = as.numeric(pre_tenure != post_tenure),
        
        #Next we define the change type
             ten1 = case_when(pre_tenure == "BUILDER" & post_tenure == "NONOWNER" ~ 1,
                              pre_tenure == "NONOWNER" & post_tenure == "NONOWNER" ~ 1,
                              pre_tenure == "OWNER" & post_tenure == "NONOWNER" ~ 2,
                              pre_tenure == "NONOWNER" & post_tenure == "OWNER" ~ 3,
                              pre_tenure == "BUILDER" & post_tenure == "OWNER" ~ 3,
                              TRUE ~ 4), #This is owner to owner transactions
        #This creates an indicator for the homes of new construction
             new_con = case_when(pre_tenure == "BUILDER" ~ 1,
                                 TRUE ~ 0),
        #This creates a set of indicators for is the owner is in the same city, state, or zip after the purchase
        #The NA values denote cases of original sales from builder
             city = as.numeric(pre_co_city != post_co_city),
             state = as.numeric(pre_co_state != post_co_state),
             zip = as.numeric(pre_co_zip != post_co_zip),
        #This creates a set of indicators to identify moves from private to non-private and combinations thereof
             P2C = ifelse(pre_private == 1 & post_private == 0, 1, 0),
             P2P = ifelse(pre_private == 1 & post_private == 1, 1, 0),
             C2C = ifelse(pre_private == 0 & post_private == 0, 1, 0),
             C2P = ifelse(pre_private == 0 & post_private == 1, 1, 0),
        #This creates a text label for the different owner type transactions
            trans.own = case_when(P2C == 1 ~ "Private to Not Private",
                                  P2P == 1 ~ "Private to Private",
                                  C2C == 1 ~ "Not Private to Not Private",
                                  C2P == 1 ~ "Not Private to Private",
                                  TRUE ~ "Unknown"),
        #This creates a set of indicators to identify moves from owner to non-owner and combinations thereof.
        #Builders are considered non-owners
             N2O = ifelse(pre_tenure == "NONOWNER" | pre_tenure=="BUILDER" & post_tenure == "OWNER",1,0),
             N2N = ifelse(pre_tenure == "NONOWNER" | pre_tenure=="BUILDER" & post_tenure == "NONOWNER",1,0),
             O2O = ifelse(pre_tenure == "OWNER" & post_tenure == "OWNER",1,0),
             O2N = ifelse(pre_tenure == "OWNER" & post_tenure == "NONOWNER",1,0),
        #This creates a text label for the transactions between owner and non-owner
        trans.ten = case_when(N2O == 1 ~ "Not Owner to Owner",
                              N2N == 1 ~ "Not Owner to Not Owner",
                              O2O == 1 ~ "Owner to Owner",
                              O2N == 1 ~ "Owner to Not Owner",
                              TRUE ~ "Unknown"),
        #This creates a text label for where the property ends in either private or non-private
             trans.end.own = case_when(P2C == 1 ~ "Not Private",
                                       P2P == 1 ~ "Private",
                                       C2C == 1 ~ "Not Private",
                                       C2P == 1 ~ "Private",
                                       TRUE ~ "Unknown"),
        #This creates a text label for where the property ends in either owner or not owner
             trans.end.ten = case_when(N2O == 1 ~ "Owner",
                                       N2N == 1 ~ "Not Owner",
                                       O2O == 1 ~ "Owner",
                                       O2N == 1 ~ "Not Owner",
                                       TRUE ~ "Unknown")) %>%
      filter(adj_price < 2500000) %>%
      add_count(parid)  #Add a count of the number of times a parcel sells in the data
    
    rm(dwelldat, own, OWN, sales, sold, temp.post.own, temp.pre.own, temp1, temp2, work, work2, work1)
    gc()

#Save Sales Data with ownership changes
  save(core, file="./Build/Output/sal_own.RData")
 

  