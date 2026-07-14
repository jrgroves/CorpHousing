gc()

load("./Build/Output/Own10.RData")

library(RapidFuzz)
library(tidyverse)

temp <- OWN %>%
  filter(year == 2010) %>%
  arrange(co_name) %>%
  filter(tenure == "NONOWNER") %>%
  mutate(ID = seq(1:n())) %>%
  select(ID, co_name) %>%
  filter(ID < 50000,
         ID > 45000)

pairs <- combn(nrow(temp), 2)

scores <- sapply(1:ncol(pairs), function(i) {
  idx1 <- pairs[1, i]
  idx2 <- pairs[2, i]
  
  fuzz_ratio(temp$name[idx1], temp$name[idx2])
})

results <- data.frame(
  row_A_id    = temp$ID[pairs[1, ]],
  string_A    = temp$name[pairs[1, ]],
  row_B_id    = temp$ID[pairs[2, ]],
  string_B    = temp$name[pairs[2, ]],
  match_score = scores
)

duplicates <- results %>%
  filter(match_score >= 80) %>%
  filter(match_score != 100)
view(duplicates)
