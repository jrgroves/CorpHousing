#Breaks the data into subsamples and analizes


#Jeremy R. Groves
#Updated: Sept. 22, 2025

rm(list = ls())

library(tidyverse)
library(gtsummary)

#Load Key Data and build core
 load(file="./Analysis/Input/Core.RData")
 source("./Analysis/Code/Function/rep.sale.R")

 
#Basic Indices
 #Assign Data to Core and set Index
    core <- rs_core2
    index <- c(names(select(core, starts_with("d_year"), -d_year2011)))
    
 #Set up the levels and regression parameters
    set <- levels(factor(core$ten1))
    color.set <- c("Nonowner to Nonowner" = "blue", "Owner to Nonowner" = "lightblue", 
                   "Nonowner to Owner" = "red", "Owner to Owner" = "darkred")
    
    predictors <- c(index, "d_nb_corporate2", "per_blk", "per_asn", "per_oth", "per_u16", "per_o65")
    
    my_formula <- as.formula(paste("d_ln_price ~ -1 + ", paste(predictors, collapse = " + ")))

  #Run Regressions for each level
    for(i in set){
      
      d.temp <- core %>%
        filter(ten1 == i) %>%
        select(d_ln_price, d_saleyr, eval(predictors)) 
      
      mod.1 <- rep.sale(my_formula, d.temp)
      
      ci.temp <- as.data.frame(confint(mod.1)) %>%
        mutate(var = row.names(.)) %>%
        remove_rownames() %>%
        set_names(c("lci", "hci", "var"))
      
      c.temp <- as.data.frame(mod.1$coefficients) %>%
        mutate(var = row.names(.)) %>%
        remove_rownames() %>%
        set_names(c("est", "var")) %>%
        full_join(., ci.temp, by = "var") %>%
        filter(var %in% index)%>%
        mutate(mod = i,
               year = as.numeric(substr(var,7,10)))
      
      ifelse(i == set[[1]],
             p.data <- c.temp,
             p.data <- bind_rows(p.data, c.temp))
      
      rm(ci.temp, c.temp, d.temp)
    }
    
    
    
    plot.1 <- ggplot(data = p.data) +
      geom_line(aes(x = year, y = est, color = mod), linewidth = 1) +
      geom_line(aes(x = year, y = lci, color = mod), linetype = "dashed") +
      geom_line(aes(x = year, y = hci, color = mod), linetype = "dashed") +
      geom_hline(yintercept = 0, color = "black", linewidth = 1.5) +
      scale_x_continuous(
        breaks = p.data$year, # Set breaks at each year in your data
        labels = as.character(p.data$year) # Ensure labels are character
      )+
      scale_color_manual(values = color.set) +
      labs(color = "Transaction Type",
           x = "Year",
           y = "Price Index") +
      theme_bw() +
      theme(axis.text.x=element_text(angle=60, hjust=1),
            legend.position="bottom") 
    
    
    
    
    
#With Transaction Type

    d.temp <- rs_core2 %>%
      select(d_ln_price, contains("d_year"), -d_year2011, d_nb_corporate2) 
    
    mod.1<-lm(d_ln_price ~ . - 1, data = d.temp)
    
      temp.r <- as.data.frame((mod.1$residuals)^2)%>%
        rename("Residuals" = "(mod.1$residuals)^2" ) %>%
        bind_cols(rs_core2$d_saleyr)
      mod.temp <- lm(Residuals ~ . - 1, data = temp.r)
      ws <- mod.temp$fitted.values
      
    mod.1w <- lm(d_ln_price ~ . - 1, data = d.temp, weights = ws)
          
    
