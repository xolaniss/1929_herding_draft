csad <-
function (data, market_return) {
  rowSums(abs(data[, -1] - as.numeric(market_return)), na.rm = TRUE) / nrow(data)
}
cssd <-
function (data, market_return) {
  sqrt(rowSums((data[, -1] - as.numeric(market_return)) ^ 2, na.rm = TRUE) / nrow(data - 1))
}


cross_deviations_tbl <-
function(data){
  # Market return
  market_return_matrix <- 
    rowMeans(data[, -1], na.rm = TRUE )
  
  # CSAD and CSSD
  csad_matrix <- csad(data = data, 
                      market_return = market_return_matrix)
  # cssd_matrix <- cssd(data = data[, -1], 
  #                     market_return = market_return_matrix)
  
  # Combined_results
  results_tbl <- 
    bind_cols(
      data %>% dplyr::select(Date),
      market_return_matrix,
      csad_matrix,
      # cssd_matrix 
    ) %>% 
    rename("Mkt" = ...2,  
           "CSAD" = ...3,
           # "CSSD" = ...4
           ) 
  
  # Result
  results_tbl
}
results_gg <- function(cross_deviations_tbl){
  cross_deviations_tbl %>% 
  pivot() %>% 
  mutate(Series = dplyr::recode(Series,
                                  # "CSSD" = "CSSD[t]",
                                  "CSAD" = "CSAD[t]",
                                  "Mkt" = "R[mt]")) %>% 
  fx_recode_plot(variables_color = 2)
}
