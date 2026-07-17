fuz.search <- function(x, y){
  list <- x %>%
    distinct(name, .keep_all = TRUE) 
  hands <- clean(list, y, .999999) %>%
    distinct(string1, .keep_all = TRUE) %>%
    filter(rapidfuzz_score > 88.00)
  
  gc()
  return(hands)
}