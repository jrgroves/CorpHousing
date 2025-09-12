#This will replicate the creation of the Wang 5 and 8 neighbors.

library(spdep)

buffer_1320 <- df %>%
  unnest(., value)  %>%
  left_join(., loc.cen, by=c("value" = "ID")) %>%
  st_drop_geometry() %>%
  select(-value, -geometry, -LUC)

loc_cen <- loc %>%
  select(LOCATOR, geometry) %>%
  st_centroid()

c<- as.data.frame(st_coordinates(loc_cen))

loc_cen2 <- st_drop_geometry(bind_cols(loc_cen, c))

temp <- buffer_1320 %>%
  rename("Neighbor" = "LOCATOR") %>%
  select(-c(PROPCLASS, LIVUNIT, n)) %>%
  left_join(., loc_cen2, by=c("name" = "LOCATOR")) %>%
  rename("base.x" = "X",
         "base.y" = "Y") %>%
  left_join(., loc_cen2, by=c("Neighbor" = "LOCATOR"), relationship = "many-to-many") %>%
  mutate(distance = sqrt((X - base.x)^2 + (Y - base.y)^2)) %>%
  filter(distance > 0) %>%
  group_by(name) %>%
    arrange(name, distance) %>%
    mutate(countt = 1:n()) %>%
  ungroup() %>%
  filter(countt<15)
  




d = sqrt((x2 - x1)^2 + (y2 - y1)^2)