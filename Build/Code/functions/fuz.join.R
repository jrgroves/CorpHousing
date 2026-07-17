
fuz.join <- function(x, y){
  output <- left_join(x, y, by = c("name" = "string2"), relationship = "many-to-many") %>%
    mutate(new_name = coalesce(string1, name),
           name = new_name) %>%
    select(parid, ID, og.name, old_name, name)
  
  return(output)
}
