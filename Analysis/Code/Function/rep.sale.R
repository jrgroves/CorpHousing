rep.sale <- function(my_formula, data){
 
        mod.1 <- lm(my_formula, data = data)
      
        temp.r <- as.data.frame((mod.1$residuals)^2)%>%
          rename("Residuals" = "(mod.1$residuals)^2" ) %>%
          bind_cols(data$d_saleyr)
        
        mod.temp <- lm(Residuals ~ . - 1, data = temp.r)
        
        my_weights <<- mod.temp$fitted.values
        
        mod.1w <- lm(my_formula, data = data, weights = my_weights)
        
 return(mod.1w)

}


