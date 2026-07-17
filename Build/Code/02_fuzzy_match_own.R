#This script relies on several functions and engages in a fuzzy match for the owner names
#based on the core ownership data. It matches across all years in the dataset and ends by rematching
#to the ownership data created in step 01. 

#By: Jeremy R. Groves
#Created: July 17, 2026


rm(list = ls())


library(RapidFuzz)
library(tidyverse)
library(text2vec)
library(Matrix)
library(igraph)

gc()

#Functions
source("./Build/Code/functions/clean.R")
source("./Build/Code/functions/mat.reit.R")
source("./Build/Code/functions/fuz.join.R")
source("./Build/Code/functions/fuz.search.R")

#Data load and Process
load("./Build/Output/Own10.RData")
  
sub_own <- OWN %>%
  #filter(year < 2011) %>%
  arrange(co_name) %>%
  mutate(ID = paste(year,seq(1:n()), sep = "-"),
         name = as.character(co_name),
         name = case_when(name == "1 70 investments limited liability corporation" ~ 
                            "I 70 investments limited liability corporation",
                          TRUE ~ name)) %>%
  select(parid, ID, name) 

  rm(OWN)
  gc()

blocks <- tapply(sub_own,toupper(substr(sub_own$name,1,1)),identity)
  num_blocks <- length(blocks)
  rm(sub_own)
  gc()

save(blocks, file = "./Build/Input/blocks.RData")
  rm(blocks)
  
#Main Processing Body  

  for(i in seq(1,num_blocks)){
    gc()
    
    load(file = "./Build/Input/blocks.RData")
    
    temp <- as.data.frame(blocks[i])
    
    rm(blocks)
    gc()
    
    temp <- data.frame(setNames(temp, c("parid", "ID", "name")))%>%
         mutate(og.name = name,
                name = gsub(" limited liability corporation", "", name),
                name = gsub(" incorporated", "", name))
                
    test <- mat.reit(temp, .88)
  
    if (!exists("own_block")) {
      # Establish the initial array structure on the first iteration
      own_block <- test
    } else {
      # Append subsequent layers along the 3rd dimension
      own_block <- bind_rows(own_block, test)
    }
  rm(test)
  gc()
  }
rm(temp)

#Attached to main OWN data

  own_block <- own_block %>%
    mutate(year = as.numeric(substr(ID, 1, 4))) %>%
    select(-old_name)
  
  save(own_block, file="./Build/Output/Matched_Own.RData")
  
  load("./Build/Output/Own10.RData")
  
  OWN <- OWN %>%
    left_join(., own_block, by = c("parid", "year"))
  
  save(OWN, file = "./Build/Output/Own10.RData")