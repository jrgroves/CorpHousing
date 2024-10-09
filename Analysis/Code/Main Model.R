rm(list = ls())

library(tidyverse)

for(i in seq(0,18,1)){
  
  load(paste0("./Build/Output/buffer",i,".RData"))
  temp <- bind_rows(core)
  
  ifelse(i == 0,
         CORE <- temp,
         CORE <- bind_rows(CORE, temp))
}

rm(core, temp, i)

df <- CORE %>%
  arrange(PARID, SALEDT2) %>%
  distinct(PARID, SALEDT2, PRICE, .keep_all = TRUE) %>%
  filter(!is.na(Corporate)) %>%
  add_count(PARID) %>%
  filter(n > 1) %>%
  select(-c(SALETYPE, SALEVAL, presale, postsale, value)) %>%
  mutate(saleyear = as.factor(saleyear)) %>%
  with(., data.frame(model.matrix(~saleyear+0), .)) %>%
  relocate(c("PARID", "date2", "saleyear"))

df2 <- df %>%
  group_by(PARID) %>%
  mutate(across(saleyear2002:n, ~ .x - lag(.x), .names = "D_{.col}")) %>%
  select(starts_with("D"), PARID, saleyear) %>%
  ungroup() %>%
  filter(!is.na(D_PRICE)) %>%
  mutate(dom = as.numeric(D_SALEDT2)) %>%
  select(-c(date2, D_SALEDT2, D_PRICE, D_adj_price, D_key, D_private, D_n)) 


mod1 <- lm(D_lnadj_price ~ ., data = select(df2, -c("PARID", "saleyear")))
