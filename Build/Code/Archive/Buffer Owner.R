

rm(list=ls())

library(tidyverse)
library(parallel)

load("./Build/Output/OWN.RData")
load("./Build/Output/Sales.RData")
load("./Build/Output/bufflist1_4.RData")


##Functions#######

owners <- function(i){

  get("SALES", envir = .GlobalEnv)
  
  nam <- names(buff.list)[[i]]
  
  temp1 <- SALES %>%
    filter(PARID == nam)
  temp2 <- unlist(buff.list[i])
  ifelse(nrow(temp1)==1,out<-one.sale(temp1, temp2),out<-multi.sale(temp1, temp2))
  
  out <- out %>%
    filter(!is.na(PARID))
  
  return(out)
  print(i)
}
    
one.sale <- function(x,y){   #x would be temp1 and y would be temp2
  get("OWN", envir = .GlobalEnv)

  temp3 <- OWN %>%
    filter(PARID %in% y) %>%
    filter(year == x$presale) %>%
    mutate(Owner = case_when(TENURE == "OWNER" ~ 1,
                             TRUE ~ 0),
           Own_MO = case_when(OWN_STATE == "MO" ~ 1,
                              TRUE ~ 0)) %>%
    select(LIVUNIT, Owner, Corporate, key, private, Own_MO) %>%
    colMeans()
 
  temp3<-as.data.frame(as.list(temp3))
  out <- cbind(x, temp3)
 return(out)
}

multi.sale <- function(x,y){   #x would be temp1 and y would be temp2
  get("OWN", envir = .GlobalEnv)

  for(j in seq(1:nrow(x))){
    yr<-x$presale[j]
    
    temp3 <- OWN %>%
      filter(PARID %in% y) %>%
      filter(year == yr) %>%
      mutate(Owner = case_when(TENURE == "OWNER" ~ 1,
                               TRUE ~ 0),
             Own_MO = case_when(OWN_STATE == "MO" ~ 1,
                                TRUE ~ 0)) %>%
      select(LIVUNIT, Owner, Corporate, key, private, Own_MO) %>%
      colMeans()
    
    temp3<-as.data.frame(as.list(temp3))
    temp4 <- cbind(x[j,], temp3)
    
    ifelse(j==1, out <- temp4, out <- rbind(out,temp4))
    
  }
  return(out)
}

#Code#####  

cl <- makeCluster(19)

#this will divide up the main list into smaller lists
n<-length(buff.list)
chunk_size <- n %/% 19
data_chunks <- split(buff.list, ceiling(seq_along(buff.list) / chunk_size))

clusterExport(cl[1], "data_chunks"[1])





u.lim <- seq(20000, length(buff.list), 10000)

l.lim <- 10001

for(i in seq(1,18)){

  set <- buff.list[l.lim:u.lim[i]]
  
  start.time <- Sys.time()
  core <- pblapply(seq_along(set),owners)
  end.time <- Sys.time()
  end.time - start.time
  
  assign(paste0("core",i), core)
  l.lim <- u.lim[i] + 1
  save(core, file=paste0("./Build/Output/buffer",i,".RData"))
  rm(set, core)
  rm(list = paste0("core", i))

}

rm(OWN, SALES, buff.list, set, u.lim, l.lim)
save.image(file="./Build/Output/buffer_owner1_4.RData")
  
load("./Build/Output/buffer8.RData")



