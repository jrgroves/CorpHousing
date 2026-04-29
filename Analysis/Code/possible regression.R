#S&P 500/Case-Shiller Model
rm(list=ls())

library(Matrix)
library(tidyverse)
library(flextable)

load("./Analysis/Input/Core18.RData")

core <- rs2_core1.8

#From McSpatial Documentation

#C

dy1 <- log(core$price1 / core$price0)
dy2 <- core$price1 - core$price0

dy <- dy2

timevar <- levels(factor(c(core$saleyr0, core$saleyr1)))
  nt = length(timevar)
  n = length(dy)
  
xmat <- array(0, dim=c(n, nt-1))

for(j in seq(2,nt)){
  xmat[,j-1] <- ifelse(core$saleyr1 == timevar[j], 1, xmat[,j-1])
  xmat[,j-1] <- ifelse(core$saleyr0 == timevar[j], -1, xmat[,j-1])
}


dcorp <- core$co1 - core$co0
  dcorp2 <- case_when(dcorp > 0 ~ 1,
                      TRUE ~ 0)
cxmat <- dcorp2 * xmat

dpriv <- core$priv1 - core$priv0
doth <- core$oth1 - core$oth0
dleg <- core$leg1 - core$leg0
  
  
colnames(xmat) <- paste0("Time", seq(2,nt))

fit0 <- lm(dy ~ xmat + dcorp)
  b0 <- c(array(0,1), fit0$coefficients)

e <- residuals(fit0)
  xvar <- core$saleyr1 - core$saleyr0 #calculates the gap
  
  fit <- lm(e^2 ~ xvar)
  wgt <- fitted(fit)
  samp <- wgt>0
  wgt <- ifelse(samp == TRUE, 1/wgt, 0)
  
  fit <- lm(dy ~ xmat + dcorp , weights = wgt)
  
  #names(fit$coefficients) <- colnames(xmat)
  `%notin%` <- Negate(`%in%`)
    pindex <- c(array(0, 1), fit$coefficients)
    lo <- c(array(0,1), confint(fit, level = 0.95)[,1])
    hi <- c(array(0,1), confint(fit, level = 0.95)[,2])
 
    gr.temp1 <- data.frame(pindex, lo, hi, b0) %>%
      mutate(Time = factor(seq(1:nrow(.))),
             name = rownames(.)) %>%
      filter(name != "(Intercept)",
             name != "dcorp",
             str_detect(name, "cxmat")) %>%
      bind_cols(., timevar[-1]) %>%
      rename("Year" = "...7")
    
    gr.temp2 <- data.frame(pindex, lo, hi, b0) %>%
      mutate(Time = factor(seq(1:nrow(.))),
             name = rownames(.)) %>%
      filter(name != "(Intercept)",
             name != "dcorp",
             name %notin% gr.temp1$name) %>%
      bind_cols(., timevar) %>%
      rename("Year" = "...7")
    
      
    ggplot(gr.temp2, aes(x = Year)) +
      geom_line(aes(y = pindex, group = 1)) +
      geom_line(aes(y = lo, group = 1), linetype = "dashed") +
      geom_line(aes(y = hi, group = 1), linetype = "dashed") +
      geom_point(aes(y = b0), color = "red") +
      labs(x = "Year")
    
    ggplot(gr.temp1, aes(x = Year)) +
      geom_line(aes(y = pindex, group = 1)) +
      geom_line(aes(y = lo, group = 1), linetype = "dashed") +
      geom_line(aes(y = hi, group = 1), linetype = "dashed") +
      geom_point(aes(y = b0), color = "red") +
      labs(x = "Year")
    
    ggplot() +
      geom_line(aes(x = Year, y = pindex, group = 1), data = gr.temp2) +
      geom_line(aes(x = Year, y = pindex, group = 1), color = "blue", data = gr.temp1)

    models <- list(fit0, fit)
    
    
  temp <- dy1 %>%
    bind_cols(., dy2, dcorp)   %>%
    rename("lnP" = "...1",
           "Dp" = "...2",
           "Dcorp" = "...3")
  
  ggplot(temp) + 
    geom_point(aes(x = Dcorp, y = Dp), color = "red") 
  
  +
    geom_point(aes(x = Dcorp, y = Dp), color = "blue")
  