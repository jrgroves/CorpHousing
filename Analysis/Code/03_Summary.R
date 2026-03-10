# This file creates the summary statistics needed for the paper.

rm(list=ls())

library(tidyverse)
library(gtsummary)

load(file="./Build/Output/Own10.RData")

sum.dat <- OWN %>%
  select(tenure, class, corporate:muni) %>%
  mutate(CLASS = case_when(class == "A" ~ "Agriculture",
                           class == "R" ~ "Residential",
                           class == "C" ~ "Commerical",
                           class == "W" ~ "Multi",
                           class == "X" ~ "Multi",
                           class == "Y" ~ "Multi",
                           class == "Z" ~ "Multi",
                           TRUE ~ NA),
         CLASS = factor(CLASS, levels = c("Residential", "Commerical", "Agriculture", "Multi")),
         legal = case_when(trustee == 1 ~ 1,
                           partnership == 1 ~ 1,
                           TRUE ~ 0),
         other = case_when(nonprofit == 1 ~ 1,
                           reown == 1 ~ 1,
                           hoa == 1 ~ 1,
                           muni == 1 ~ 1,
                           TRUE ~ 0)) %>%
  select(-class) %>%
  tbl_summary(by = CLASS,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%")) %>%
  add_overall()




load(file = "./Analysis/Input/core.RData")

sum.dat2 <- rs_core1.4 %>%
  select(d_ln_price, d_adj_price, P2P, P2C, C2P, C2C, ten1) %>%
  tbl_summary(by = ten1,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%"),
              label = list(P2P ~ "Private To Private",
                           P2C ~ "Private to Corporate",
                           C2P ~ "Corporate to Private",
                           C2C ~ "Corporate to Corporate")) %>%
  add_overall()

sum.dat3 <- rs_core1.4 %>%
  select(d_ln_price, d_adj_price, N2N, N2O, O2N, O2O, trans.own) %>%
  tbl_summary(by = trans.own,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%"),
              label = list(N2N ~ "Nonowner to Nonowner",
                           N2O ~ "Nonowner to Owner",
                           O2N ~ "Owner to Nonowner",
                           O2O ~ "Owner to Owner")) %>%
  add_overall()
