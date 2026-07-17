rm(list = ls())


library(RapidFuzz)
library(tidyverse)
library(text2vec)
library(Matrix)
library(igraph)

gc()

source("./Build/Code/clean.R")
source("./Build/Code/mat.reit.R")

fuz.join <- function(x, y){
  output <- left_join(x, y, by = c("name" = "string2"), relationship = "many-to-many") %>%
    mutate(new_name = coalesce(string1, name),
           name = new_name) %>%
    select(parid, ID, og.name, old_name, name)
  
  return(output)
}

fuz.search <- function(x, y){
  list <- x %>%
    distinct(name, .keep_all = TRUE) 
  hands <- clean(list, y, .999999) %>%
    distinct(string1, .keep_all = TRUE) %>%
    filter(rapidfuzz_score > 88.00)
  
  gc()
  return(hands)
}


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

  save(own_block, file="./Build/Output/Matched_Own.RData")