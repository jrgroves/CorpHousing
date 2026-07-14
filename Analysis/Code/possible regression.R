#S&P 500/Case-Shiller Model
rm(list=ls())

library(Matrix)
library(tidyverse)
library(flextable)

load("./Analysis/Input/Core18.RData")

core <- rs2_core1.8 %>%
  filter(price1 != price0) %>%
  mutate(dy1 = log(price1/price0),
         dy2 = price1 - price0,
         dcorp = ifelse((nb_sfh_corporate1 - nb_sfh_corporate0) < -.15, 1, 0),
         dpriv = (nb_sfh_private1 - nb_sfh_private0),
         dleg = nb_sfh_legal1 - nb_sfh_legal0,
         doth = nb_sfh_other1 - nb_sfh_other0) %>%
  filter(dy2 > -150000,
         dy2 < 250000)

##Model Definition
  mymodel = formula(dy1 ~ xmat + dcorp -1)

#From McSpatial Documentation#####


timevar <- levels(factor(c(core$saleyr0, core$saleyr1)))
  nt = length(timevar)
  n = length(core$dy1)
  
xmat <- array(0, dim=c(n, nt-1))

for(j in seq(2,nt)){
  xmat[,j-1] <- ifelse(core$saleyr1 == timevar[j], 1, xmat[,j-1])
  xmat[,j-1] <- ifelse(core$saleyr0 == timevar[j], -1, xmat[,j-1])
}


# Model One

  colnames(xmat) <- paste0("Time", seq(2,nt))
  

    fit0 <- lm(mymodel, data = core)
      b0 <- c(array(0,1), fit0$coefficients)

      e <- residuals(fit0)
        xvar <- core$saleyr1 - core$saleyr0 #calculates the gap
        
        fit <- lm(e^2 ~ xvar)
        wgt <- fitted(fit)
        samp <- wgt>0
        wgt <- ifelse(samp == TRUE, 1/wgt, 0)
        
    fit <- lm(mymodel , weights = wgt, data = core)

    
  #names(fit$coefficients) <- colnames(xmat)
    pindex <- c(array(0, 1), fit$coefficients)
    lo <- c(array(0,1), confint(fit, level = 0.95)[,1])
    hi <- c(array(0,1), confint(fit, level = 0.95)[,2])
 
    gr.temp1 <- data.frame(pindex, lo, hi, b0) %>%
      mutate(Time = factor(seq(1:nrow(.))),
             name = rownames(.)) %>%
      filter(str_detect(name, "xmatTime") | name == "")   %>%
      bind_cols(., timevar) %>%
      rename("Year" = "...7")
    
    ggplot(gr.temp1, aes(x = Year)) +
      geom_line(aes(y = pindex, group = 1)) +
      geom_line(aes(y = lo, group = 1), linetype = "dashed") +
      geom_line(aes(y = hi, group = 1), linetype = "dashed") +
      geom_point(aes(y = b0), color = "red") +
      labs(x = "Year")
  