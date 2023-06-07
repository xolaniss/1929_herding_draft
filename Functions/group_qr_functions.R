nest_prep <-
function(data){
  data %>%
    dplyr::relocate(Date, .after = "Category") %>% 
    group_by(Category) %>% 
    nest()
}
tau_mapper <-
function(tau = .1){
  as_mapper(~rq(formula, tau = tau, data = .))
}
qmodel_tidy_group_models <-
function(nested_data, formula){
  formula <-  formula
  nested_data %>% 
    mutate(models_1 = map(data, tau_mapper(tau = .1))) %>% 
    mutate(models_2 = map(data, tau_mapper(tau = .25))) %>% 
    mutate(models_3 = map(data, tau_mapper(tau = .5))) %>% 
    mutate(models_4 = map(data, tau_mapper(tau = .95))) %>% 
    mutate(models_5 = map(data, tau_mapper(.99))) %>% 
    mutate(models_coef_1 = map(models_1, ~tidy(.))) %>% 
    mutate(models_coef_2 = map(models_2, ~tidy(.))) %>% 
    mutate(models_coef_3 = map(models_3, ~tidy(.))) %>% 
    mutate(models_coef_4 = map(models_4, ~tidy(.))) %>% 
    mutate(models_coef_5 = map(models_5, ~tidy(.))) 
}
qmodels_pretty_results <-
function(data_fitted_models){
  data_fitted_models %>% 
    unnest(cols = c(
      models_coef_1,
      models_coef_2,
      models_coef_3,
      models_coef_4,
      models_coef_5
    ),
    names_repair = "universal") %>% 
    dplyr::select(Category, 
                  term...8, 
                  starts_with("estimate"),
                  starts_with("p.value")
    ) %>% 
    mutate(across(.col = 2:11, .fns = ~format(., digits = 4))) %>% 
    mutate(across(.col = 2:11, .fns = as.character)) %>% 
    mutate(across(.col = 2:6, .fns = ~strtrim(., 8))) %>% 
    mutate(across(.col = 7:11, .fns = ~strtrim(., 4))) %>% 
    mutate(comb_10 = paste0(estimate...9, " ", "[", p.value...12,"]")) %>% 
    mutate(comb_25 = paste0(estimate...15, " ", "[", p.value...18,"]")) %>% 
    mutate(comb_50 = paste0(estimate...21, " ", "[", p.value...24,"]")) %>% 
    mutate(comb_95 = paste0(estimate...27, " ", "[", p.value...30,"]")) %>% 
    mutate(comb_99 = paste0(estimate...33, " ", "[", p.value...36,"]")) %>% 
    dplyr::select(
      Category,
      term...8,
      comb_10,
      comb_25,
      comb_50,
      comb_95,
      comb_99
    ) %>%  
    pivot_longer(-c(Category, term...8)) %>% 
    spread(key = term...8, value = value) %>% 
    mutate(tau_number = ifelse(name == "comb_10", 0.1, 
                               ifelse(name == "comb_25", 0.25,
                                      ifelse(name == "comb_50", 0.5, 
                                             ifelse(name == "comb_95", 0.95,
                                                    ifelse(name == "comb_99", 0.99, name)))))) %>% 
    arrange(tau_number, .by_group = TRUE) %>% 
    mutate(tau = if_else(name == "comb_10", "τ = 10%",
                         if_else(name == "comb_25", "τ = 25%",
                                 if_else(name == "comb_50", "τ = 50%",
                                         if_else(name == "comb_95", "τ = 95%",
                                                 if_else(name == "comb_99", "τ = 99%", name)))))) %>% 
    
    relocate(tau, .after = name) %>% 
    dplyr::select(-name, -tau_number) %>% 
    mutate(across(.col = 2:4, ~str_replace_all(.x, "\\[0]", "[0.00]")))
  
}
qmodels_group_workflow <-
function(data){
  data %>%   
    nest_prep() %>% 
    qmodel_tidy_group_models(formula = formula) %>% 
    qmodels_pretty_results()
}
