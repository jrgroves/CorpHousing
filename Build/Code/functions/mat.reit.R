mat.reit <- function(x, y){
  f.match <- fuz.search(x, y)
  f.cor <- x
  success <- nrow(f.match) == 0
  
  while(!success) {
    f.cor <- f.cor %>%
      mutate(old_name = name) %>%
      fuz.join(., f.match) %>%
      fuz.join(., f.match) %>%
      fuz.join(., f.match) %>%
      distinct(ID, .keep_all = TRUE)
    
    y <- y + .005
    f.match <- fuz.search(f.cor, y)
    success <- nrow(f.match) == 0
  }

  if("old_name" %in% colnames(f.cor)){
    return(f.cor)
  } else {
    f.cor <- f.cor %>%
      mutate(old_name = name) %>%
      select(parid, ID, og.name, old_name, name)
    return(f.cor)
  }
}