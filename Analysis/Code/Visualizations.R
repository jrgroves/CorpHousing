# This file creates the summary statistics tables and visualizations needed for the paper for the data descriptions

rm(list=ls())

library(tidyverse)
library(gtsummary)


load(file="./Build/Output/Own10.RData") 
load("./Analysis/Input/Core14.RData")

#Data construction and Compile

    #Ownership Data
    
      own <- OWN %>%
        select(tenure, class, year, corporate:other) %>%
        mutate(CLASS = case_when(class == "A" ~ "Agriculture",
                                 class == "R" ~ "Residential",
                                 class == "C" ~ "Commerical",
                                 class == "W" ~ "Multi",
                                 class == "X" ~ "Multi",
                                 class == "Y" ~ "Multi",
                                 class == "Z" ~ "Multi",
                                 TRUE ~ NA),
               CLASS = factor(CLASS, levels = c("Residential", "Commerical", "Agriculture", "Multi")),
               OWN_TYPE = case_when(corporate == 1 ~ "Corporate",
                                    private == 1 ~ "Private",
                                    legal == 1 ~ "Legal",
                                    other == 1 ~ "Other",
                                    TRUE ~ NA)) %>%
        select(-class) %>%
        filter(!is.na(CLASS))
    
#Ownership Data Section

  sum.dat <- own %>%
    select(-year, -key, -OWN_TYPE) %>%
    tbl_summary(by = CLASS,
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_categorical() ~ "{p}%")) %>%
  add_overall()
  
  temp <- own %>%
    select(year, private, corporate, legal, other) %>%
    summarize(across(private:other, mean), .by = year) %>%
    select(-private)   %>%  #Can break here to get the percentage values for each type.
    pivot_longer(cols = c("corporate", "legal", "other"),
                 names_to = "OWN_TYPE", values_to = "Share") %>%
    mutate(OWN_TYPE = str_to_title(OWN_TYPE))
    
 
  ggplot(temp, aes(fill = OWN_TYPE, x = year, y = Share)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_bw()

#Sales Data Section
  
  temp <- rs_core1.4 %>%
    select(saleyr, d_saleyr, adj_price, ln_price) %>%
    tbl_summary( by = saleyr,
                include = c(d_saleyr, adj_price, ln_price),
                        type = list(d_saleyr ~ "continuous"),
                        statistic = list(all_continuous() ~ "{mean} ({sd})"),
                        label = list(d_saleyr = "Time Between Sales",
                                     adj_price = "Difference of Real Price",
                                     ln_price = "Difference of LN of Real Price")) %>%
  
      add_overall()
  as_hux_table(temp) %>% huxtable::quick_xlsx("my_table.xlsx") #This allows for the transposing of the table.
  
  temp <- rs_core1.4 %>%
    tbl_summary(by = ten1,
                include = c(type2, ln_price, adj_price, d_saleyr),
                label = list(type2 ~ "Transaction between",
                             adj_price = "Difference of Real Price",
                             ln_price = "Difference of LN of Real Price",
                             d_saleyr = "Time Between Sales")) %>%
    add_overall()
                
#Neighbor Section
  #1/4 Mile Buffers
 temp <- rs_core1.4 %>%
    tbl_summary(by = ten1,
                include = c(nb_private, nb_corporate, nb_legal, nb_other,
                            nb_owner, nb_nonzip, nb_prop_agg, nb_prop_com,
                            nb_prop_multi, nb_prop_res),
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                label = list(nb_private ~ "Private",
                             nb_corporate ~ "Corporate",
                             nb_legal ~ "Legal",
                             nb_other ~ "Other",
                             nb_owner ~ "Owner Occupied",
                             nb_nonzip ~ "Owner in Diff. Zip",
                             nb_prop_res ~ "Residential",
                             nb_prop_com ~ "Commerical",
                             nb_prop_multi ~ "Multiuse",
                             nb_prop_agg ~ "Aggricultural")) %>%
   add_overall()
  
 temp <- rs_core1.4 %>%
   tbl_summary(by = ten1,
               include = c(starts_with("per_"), income, own_sz, age),
               statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
   add_overall()
 
 #1/8 Mile Buffers
     load("./Analysis/Input/Core18.RData")  
    
     temp <- rs_core1.8 %>%
       tbl_summary(by = ten1,
                   include = c(nb_private, nb_corporate, nb_legal, nb_other,
                               nb_owner, nb_nonzip, nb_prop_agg, nb_prop_com,
                               nb_prop_multi, nb_prop_res),
                   statistic = list(all_continuous() ~ "{mean} ({sd})"),
                   label = list(nb_private ~ "Private",
                                nb_corporate ~ "Corporate",
                                nb_legal ~ "Legal",
                                nb_other ~ "Other",
                                nb_owner ~ "Owner Occupied",
                                nb_nonzip ~ "Owner in Diff. Zip",
                                nb_prop_res ~ "Residential",
                                nb_prop_com ~ "Commerical",
                                nb_prop_multi ~ "Multiuse",
                                nb_prop_agg ~ "Aggricultural")) %>%
       add_overall()
     
     temp <- rs_core1.8 %>%
       tbl_summary(by = ten1,
                   include = c(starts_with("per_"), income, own_sz, age),
                   statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
       add_overall() 
     
     
#Visualizations
     
    load(file="./Build/Output/sal_own.RData") 


  core.own <- core %>%
    filter(parid %in% rs_core1.4$parid) %>%
    select(adj_price, saleyr, trans.own, trans.ten, type2) %>%
    group_by(trans.ten, saleyr) %>%
    summarize(mean = mean(adj_price)/100000,
              n = n())%>%
    ungroup() %>%
    mutate(year = as.factor(saleyr))  %>%
    group_by(year) %>%
    mutate(n_per = n/sum(n)) %>%
    group_by(trans.ten) %>%
    arrange(year) %>%
    mutate(diff.price = mean - lag(mean),
           diff.n = n - lag(n),
           diff.n_per = n_per - lag(n_per)) %>%
    ungroup()

ggplot(core.own) +
  geom_point(aes(y = mean, x = year, color = trans.ten)) +
  geom_line(aes(y = mean, x = year, color = trans.ten, group = trans.ten)) +
  labs(title = "Average Real Price by Transaction Tenure Change",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Real Dollars (in 100,000)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Transaction Type"))

core.own <- core %>%
  filter(parid %in% rs_core1.4$parid) %>%
  select(adj_price, saleyr, trans.own, trans.ten, type2) %>%
  mutate(trans.end = case_when(str_split_i(type2, "2", 2) == "P" ~ "Private",
                               str_split_i(type2, "2", 2) == "C" ~ "Corporate",
                               str_split_i(type2, "2", 2) == "L" ~ "Legal",
                               str_split_i(type2, "2", 2) == "O" ~ "Other",
                               TRUE ~ NA)) %>%
  filter(!is.na(trans.end)) %>%
  group_by(trans.end, saleyr) %>%
  summarize(mean = mean(adj_price)/100000,
            n = n())%>%
  ungroup() %>%
  mutate(year = as.factor(saleyr))  %>%
  group_by(year) %>%
  mutate(n_per = n/sum(n)) %>%
  group_by(trans.end) %>%
  arrange(year) %>%
  mutate(diff.price = mean - lag(mean),
         diff.n = n - lag(n),
         diff.n_per = n_per - lag(n_per)) %>%
  ungroup()

ggplot(core.own) +
  geom_point(aes(y = mean, x = year, color = trans.end)) +
  geom_line(aes(y = mean, x = year, color = trans.end, group = trans.end)) +
  labs(title = "Average Real Price by Buyer Type",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Real Dollars (in 100,000)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Buyer Type"))

ggplot(core.own) +
  geom_point(aes(y = n_per, x = year, color = trans.end)) +
  geom_line(aes(y = n_per, x = year, color = trans.end, group = trans.end)) +
  labs(title = "Share of Transactions by Buyer Type",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Percentage of Annual Transactions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Buyer Type"))





core.ten <- core %>%
  filter(parid %in% rs_core1.4$parid) %>%
  select(adj_price, saleyr, trans.ten) %>%
  group_by(trans.ten, saleyr) %>%
  summarize(mean = mean(adj_price) / 100000,
            n = n())%>%
  ungroup() %>%
  mutate(year = as.factor(saleyr))%>%
  group_by(year) %>%
  mutate(n_per = n/sum(n))

ggplot(core.ten) +
  geom_point(aes(y = mean, x = year, color = trans.ten))+
  geom_line(aes(y = mean, x = year, color = trans.ten, group = trans.ten)) +
  labs(title = "Average Real Price by Change in Tenure",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Real Dollars (in 100,000)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") 

ggplot(core.ten) +
  geom_point(aes(y = n_per, x = year, color = trans.ten))+
  geom_line(aes(y = n_per, x = year, color = trans.ten, group = trans.ten)) +
  labs(title = "Share of Transactions by Change in Tenure",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Percentage of Annual Transactions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") 

ggplot(filter(core.ten, trans.ten != "Owner to Owner")) +
  geom_point(aes(y = n_per, x = year, color = trans.ten))+
  geom_line(aes(y = n_per, x = year, color = trans.ten, group = trans.ten)) +
  labs(title = "Share of Transactions by Change in Tenure (without Owner to Owner)",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Percentage of Annual Transactions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") 

core.end.ten <- core %>%
  select(adj_price, saleyr, trans.end.ten) %>%
  group_by(trans.end.ten, saleyr) %>%
  summarize(mean = mean(adj_price) / 100000,
            n = n())%>%
  ungroup() %>%
  mutate(year = as.factor(saleyr))%>%
  group_by(year) %>%
  mutate(n_per = n/sum(n)) %>%
  ungroup()

ggplot(core.end.ten) +
  geom_point(aes(y = mean, x = year, color = trans.end.ten))+
  geom_line(aes(y = mean, x = year, color = trans.end.ten, group = trans.end.ten)) +
  labs(title = "Average Real Price by Change in Buyer's Tenure",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Real Dollars (in 100,000)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") 

ggplot(core.end.ten) +
  geom_point(aes(y = n_per, x = year, color = trans.end.ten))+
  geom_line(aes(y = n_per, x = year, color = trans.end.ten, group = trans.end.ten)) +
  labs(title = "Share of Transactions by Buyer's Tenure",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Percentage of Annual Transactions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") 

