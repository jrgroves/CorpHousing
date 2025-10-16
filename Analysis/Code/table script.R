tabs<-list(    
  table1 <- tbl_summary(rs_core2,
                        by  = ten1,
                        include = -c(trans.own, N2N, N2O, O2O, O2N, P2P, P2C, C2P, C2C, d_year2023,
                                     d_nb_corporate2),
                        digits = list(all_continuous() ~ c(4,4),
                                      d_adj_price ~ c(0,2),
                                      all_categorical() ~ c(2,0)),
                        statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_categorical() ~ "{p}% {n}")) %>%
    remove_row_type(type = "level", level_value = "0")%>%
    modify_header(all_stat_cols() ~ "**{level}**")
  
  
  
  table2 <- tbl_summary(rs_core2,
                        by  = trans.own,
                        include = -c(ten1, N2N, N2O, O2O, O2N, P2P, P2C, C2P, C2C, d_year2023,
                                     d_nb_corporate2),
                        digits = list(all_continuous() ~ c(4,4),
                                      d_adj_price ~ c(0,2),
                                      all_categorical() ~ c(2,0)),
                        statistic = list(all_continuous() ~ "{mean}({sd})",
                                         all_categorical() ~ "{p}% {n}")) %>%
    remove_row_type(type = "level", level_value = "0") %>%
    modify_header(all_stat_cols() ~ "**{level}**")
  
  
  
  #  lapply(tabs, as_tibble) %>%
  #   openxlsx::write.xlsx(file = "./tables.xlsx")
  