#This code pulls the dwelling data from the 2024 assessor's report and saves it for use in other codes.
#note that in the assessor downloads, the header coloumn is not complete for the dwelling data text file so the 
#column names are posted seperately via the data dictionary.
  
#Saves: dwelldat.RData - dwelling data from 2024

#Jeremy R. Groves
#Created: June 18, 2025

rm(list=ls())

library(tidyverse)

i<-2024

dwell <- read.csv(unz(paste0("F:/Data/Saint Louis County Assessor Data/STLCOMO_REAL_ASMTROLL_EOY_",i,".zip"),
                       filename = "dwelling.csv"), sep = "|", skip = 1, header = FALSE, stringsAsFactors = FALSE)
names(dwell) <- c("parid", "card", "taxyr", "stories", "extwall", "style", "yrblt", "effyr", "yrremod", "rmtot",
                  "rmbed", "rmfam", "fixbath", "fixhalf", "fixaddl", "fixtot", "remkit", "rembath", "bsmt",
                  "heat", "fuel", "heatsys", "attic", "unfinarea", "recromarea", "finbsmtarea", "wbfp_o",
                  "wbfp_s", "wbfp_pf","bsmtcar", "condolvl", "condotyp", "condovw", "unitno", "cndbasevale", "mgfa",
                  "sfla", "grade", "cdu")

dwelldat <- dwell %>%
  filter(sfla > 0,
         mgfa > 0,
         rmtot > 0,
         !is.na(rmtot)) %>%
  mutate(rmfam = case_when(is.na(rmfam) ~ 0,
                            TRUE ~ rmfam),
         rmbed = case_when(rmbed < 0 ~ 3,
                           TRUE ~ rmbed)) %>%
  select(parid, card, yrblt, stories, extwall, style, rmtot, rmbed, rmfam, fixbath,
         fixhalf, fixaddl, fixtot, bsmt, heat, fuel, heatsys, attic, wbfp_o, wbfp_s,
         wbfp_pf, bsmtcar, mgfa, sfla, grade, cdu) %>%
  distinct(parid, card, .keep_all = T)

save(dwelldat, file="./Build/Output/dwell.RData")
