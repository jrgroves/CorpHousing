rm(list = ls())


library(RapidFuzz)
library(tidyverse)
library(text2vec)
library(Matrix)
library(igraph)

gc()

load("./Build/Output/Own10.RData")
                                       

source("./clean.R")

sub_own <- OWN %>%
  filter(year == 2010) %>%
  arrange(co_name) %>%
  #filter(tenure == "NONOWNER") %>%
  mutate(ID = seq(1:n()),
         name = as.character(co_name),
         name = case_when(name == "1 70 investments limited liability corporation" ~ 
                            "I 70 investments limited liability corporation",
                          TRUE ~ name)) %>%
  select(ID, name) 

rm(OWN)
gc()

blocks <- tapply(sub_own,toupper(substr(sub_own$name,1,1)),identity)
  num_blocks <- length(blocks)
  
for(i in seq(1,nrow(blocks))){
  temp <- as.data.frame(blocks[i])
     temp <- data.frame(setNames(temp, c("ID", "name")))
  hands <- clean(temp, .87, .999999)
  
  if(nrow(hands) != 0){
      replace <- hands %>%
        select(string1, string2, rapidfuzz_score) %>%
        distinct()
      
      temp2 <- temp %>%
        left_join(., replace, by = c("name" = "string1"), relationship = "many-to-many") %>%
        mutate(new_name = coalesce(string2, name),
               old_name = name,
               name = new_name,
               rapidfuzz_score = replace_na(rapidfuzz_score, 100))%>%
        filter(rapidfuzz_score == max(rapidfuzz_score), .by = ID) %>%
        select(ID, old_name, name) %>%
        distinct(ID, .keep_all = TRUE)
  } else {
    temp2 <- temp %>%
      mutate(old_name = name) %>%
      select(ID, old_name, name) %>%
      distinct(ID, .keep_all = TRUE)
  }
  
  if (!exists("own_block")) {
    # Establish the initial array structure on the first iteration
    own_block <- temp2
  } else {
    # Append subsequent layers along the 3rd dimension
    own_block <- bind_rows(own_block, temp2)
  }
}

    test<-own_block %>%
      mutate(old_count = n(), .by = old_name) %>%
      mutate(new_count = n(), .by = name)
