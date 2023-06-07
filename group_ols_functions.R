nest_prep <-
function(tbl){
  tbl %>%
    dplyr::relocate(Date, .after = "Category") %>% 
    group_by(Category) %>% 
    nest()
}
