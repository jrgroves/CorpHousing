#S&P 500/Case-Shiller Model
rm(list=ls())

library(Matrix)
library(tidyverse)
library(flextable)

load("./Analysis/Input/Core18.RData")

core <- rs2_core1.8 %>%
  filter(down==0)

#From McSpatial Documentation


dy1 <- log(core$price1 / core$price0)
dy2 <- core$price1 - core$price0

dy <- dy1

timevar <- levels(factor(c(core$saleyr0, core$saleyr1)))
  nt = length(timevar)
  n = length(dy1)
  
xmat <- array(0, dim=c(n, nt-1))

for(j in seq(2,nt)){
  xmat[,j-1] <- ifelse(core$saleyr1 == timevar[j], 1, xmat[,j-1])
  xmat[,j-1] <- ifelse(core$saleyr0 == timevar[j], -1, xmat[,j-1])
}


dcorp <- core %>%
  mutate(dcorp = nb_sfh_corporate1 - nb_sfh_corporate0) %>%
  pull(dcorp)
dcorp2 = dcorp^2

dcorp2 <- case_when(dcorp > 0 ~ 1,
                      TRUE ~ 0)
cxmat <- dcorp2 * xmat


dpriv <- core$nb_sfh_private1 - core$nb_sfh_private0

# Model One

  colnames(xmat) <- paste0("Time", seq(2,nt))

    fit0 <- lm(dy ~ xmat + dcorp + dpriv - 1)
      b0 <- c(array(0,1), fit0$coefficients)

      e <- residuals(fit0)
        xvar <- core$saleyr1 - core$saleyr0 #calculates the gap
        
        fit <- lm(e^2 ~ xvar)
        wgt <- fitted(fit)
        samp <- wgt>0
        wgt <- ifelse(samp == TRUE, 1/wgt, 0)
        
    fit <- lm(dy ~ xmat + dcorp+ dpriv - 1 , weights = wgt)
        
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
             name != "dpriv",) %>%
      bind_cols(., timevar) %>%
      rename("Year" = "...7")

    ggplot(gr.temp1, aes(x = Year)) +
      geom_line(aes(y = pindex, group = 1)) +
      geom_line(aes(y = lo, group = 1), linetype = "dashed") +
      geom_line(aes(y = hi, group = 1), linetype = "dashed") +
      geom_point(aes(y = b0), color = "red") +
      labs(x = "Year")
    
  