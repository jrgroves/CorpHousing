rm(list = ls())

library(tidyverse)

####Functions########


comp_df = function(x, y){
  n1 <- as.data.frame(sapply(x, typeof))
  n2 <- as.data.frame(sapply(y, typeof))
  
  c <- n1 == n2  
  
  l1 <- length(c)
  
  for(i in seq(1, l1)){
    ifelse(c[i,1] == "TRUE",
           next,
           ifelse(n1[i,1] == "character",
                  y[,i] <- as.character(y[,i]),
                  ifelse(n1[i,1] == "integer",
                         y[,i] <- as.integer(y[,i]),
                         y[,i] <- as.numeric(y[i,1]))))
  }
  return(y)
}

####Code#####


#Create Owner Data Files####
for(i in seq(2009,2024,by = 1)){
  
  temp <- read.csv(unz(paste0("F:/Data/Saint Louis County Assessor Data/STLCOMO_REAL_ASMTROLL_EOY_",i,".zip"),
                         filename = "dwelling.csv"), sep = "|", header = TRUE, stringsAsFactors = FALSE,
                     quote = "", row.names = NULL)
  
 ifelse(length(temp) == 37,
        temp <- temp,
        temp <- temp %>% select(-c(V1, V2)))
 

 if(i == 2009){
      dwelldat <- temp
   }else{
     temp <- comp_df(dwelldat, temp)
     dwelldat <- dwelldat %>% bind_rows(temp)
   }
 
}

working <- dwelldat %>%
  filter(CARD == 1) %>%    #This limits to the primary structure on the parcel.
  relocate(YRBLT:YRREMOD, .after = TAXYR) %>%
  mutate(across(EXTWALL:REMBATH, ~replace(., .<=0, NA))) %>%  #This removes all negative and zero values and replaces with NA
  mutate(across(MGFA:SFLA, ~replace(., . <= 100, NA))) %>%
  mutate(across(YRBLT:YRREMOD, ~replace(., . <= 1700, NA))) %>%
  arrange(PARID, TAXYR, .keep = TRUE) %>%
  group_by(PARID) %>%
    fill(everything(), .direction = "downup") %>%
  ungroup()

sale_dwell <- working

save(sale_dwell, file = "./Build/OUtput/saledwell.RData")
