#Processes Sales Data

#Jeremy R. Groves
#June 27, 2024

rm(list=ls())

library(tidyverse)
library(foreign)

sales <- read.csv(file="./Build/Data/2020/sales.csv",
                  header = TRUE,
                  sep = "|",
                  quote = "", 
                  row.names = NULL, 
                  stringsAsFactors = FALSE)

SALES <- sales %>%
  mutate(SALEVAL = case_when(SALEVAL == ".X" ~ "X",
                             SALEVAL == "i" ~ "I",
                             SALEVAL == "x" ~ "X",
                             SALEVAL == " x" ~ "X",
                             SALEVAL == "t" ~ "T",
                             SALEVAL == "TT" ~ "T",
                             SALEVAL == " T" ~ "T",
                             TRUE ~ SALEVAL),
         SALEDT2 = as.Date(SALEDT, "%d-%b-%y"),
         year = year(SALEDT2)) %>%
  filter(SALEVAL == "4" |
           SALEVAL == "5" |
           SALEVAL == "F" |
           SALEVAL == "I" |
           SALEVAL == "P" |
           SALEVAL == "X" |
           SALEVAL == "Z" |
           SALEVAL == "T") %>%
  filter(!is.na(PRICE)) %>%
  select(PARID, SALEDT2, PRICE, SALETYPE, SALEVAL, year) 
