rm(list=ls())

library(tidyverse)

#load(file="./Build/Output/Sales.RData")
#load(file="./Build/Output/Own10.RData") 
load(file="./Build/Output/sal_own.RData")


#Replace builder tenure with nonowner

#Visualizations

core.own <- core %>%
  select(adj_price, saleyr, trans.own, trans.ten) %>%
  group_by(trans.own, saleyr) %>%
  summarize(mean = mean(adj_price)/100000,
            n = n())%>%
  ungroup() %>%
  mutate(year = as.factor(saleyr))  %>%
  group_by(year) %>%
  mutate(n_per = n/sum(n)) %>%
  group_by(trans.own) %>%
  arrange(year) %>%
  mutate(diff.price = mean - lag(mean),
         diff.n = n - lag(n),
         diff.n_per = n_per - lag(n_per)) %>%
  ungroup()

ggplot(core.own) +
  geom_point(aes(y = mean, x = year, color = trans.own)) +
  geom_line(aes(y = mean, x = year, color = trans.own, group = trans.own)) +
  labs(title = "Average Real Price by Transaction Type",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Real Dollars (in 100,000)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Transaction Type"))

ggplot(core.own) +
  geom_point(aes(y = n_per, x = year, color = trans.own)) +
  geom_line(aes(y = n_per, x = year, color = trans.own, group = trans.own)) +
  labs(title = "Share of Transactions by Transaction Type",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Percentage of Annual Transactions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Transaction Type"))

core.own.end <- core %>%
  select(adj_price, saleyr, trans.end.own) %>%
  group_by(trans.end.own, saleyr) %>%
  summarize(mean = mean(adj_price)/100000,
            n = n())%>%
  ungroup() %>%
  mutate(year = as.factor(saleyr))  %>%
  group_by(year) %>%
  mutate(n_per = n/sum(n))

ggplot(core.own.end) +
  geom_point(aes(y = mean, x = year, color = trans.end.own)) +
  geom_line(aes(y = mean, x = year, color = trans.end.own, group = trans.end.own)) +
  labs(title = "Average Real Price by Buyer Type",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Real Dollars (in 100,000)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Buyer Type"))

ggplot(core.own.end) +
  geom_point(aes(y = n_per, x = year, color = trans.end.own)) +
  geom_line(aes(y = n_per, x = year, color = trans.end.own, group = trans.end.own)) +
  labs(title = "Share of Transactions by Buyer Type",
       caption = "Data from St. Louis County Assessor Records") +
  xlab("Year") +
  ylab("Percentage of Annual Transactions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Buyer Type"))

core.ten <- core %>%
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

