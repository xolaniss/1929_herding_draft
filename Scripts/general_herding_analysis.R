# Description
# General herding analysis by Xolani Sibande - 23 Septermber 2022

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)
library(quantreg)

options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
unnest_rol_col <- function(data, rol_column) {
  data %>% 
    mutate(glance = map({{ rol_column }}, broom::tidy)) %>% 
    unnest(glance) %>% 
    dplyr::select(Date, term:estimate, statistic) %>% 
    drop_na() %>% 
    pivot_wider(names_from = term, values_from = c(estimate, statistic)) %>% 
    dplyr::rename("a0" = `estimate_(Intercept)`,
                  "a1" = `estimate_..2`,
                  "a2" = `estimate_..3`,
                  "t-statistic a0" = `statistic_(Intercept)`,
                  "t-statistic a1" = `statistic_..2`,
                  "t-statistic a2" = `statistic_..3` ) # may delete depending on number of variables or focus
}

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_resuls_tbl <- result_csad_cssd$combined_results_tbl

# Static regressions ------------------------------------------------------
# # OLS -------------------------------------------------------------------
fitted_models <- combined_resuls_tbl %>% 
  relocate(Date, .after = "Category") %>% 
  group_by(Category) %>% 
  nest() %>% 
  mutate(models = map(data, ~lm(CSAD ~ abs(`Market Return`) + I(`Market Return` ^ 2), data = .))) %>% 
  mutate(models_coef = map(models, ~tidy(.))) %>% 
  mutate(models_glance = map(models, ~glance(.))) 

results_models <- 
  fitted_models %>% 
  unnest(cols = c(models_coef, models_glance), names_repair = "universal") %>% 
  dplyr::select(Category, term, estimate, p.value...8)  %>% 
  mutate(across(2:3, as.character)) %>% 
  mutate(across(2:3, ~strtrim(., 6))) %>% 
  mutate(comb = paste0(estimate, " ", "[", p.value...8, "]")) %>% 
  dplyr::select(Category, term, comb) %>% 
  pivot_longer(-c(Category, term)) %>% 
  spread(key = term, value = value) %>% 
  mutate(across(2:4, ~str_replace_all(., "\\[0]", "[0.0000]"))) %>% 
  dplyr::select(-name)

# # # QR --------------------------------------------------------------------

formular <-  as.formula(CSAD ~ abs(`Market Return`) + I(`Market Return` ^ 2))
tau_mapper <- function(tau = .1){
  as_mapper(~rq(formular, tau = tau, data = .))
}

fitted_qmodels_tbl <- 
  combined_resuls_tbl %>% 
  relocate(Date, .after = "Category") %>% 
  group_by(Category) %>% 
  nest() %>% 
  mutate(models_1 = map(data, tau_mapper(tau = .1))) %>% 
  mutate(models_2 = map(data, tau_mapper(tau = .25))) %>% 
  mutate(models_3 = map(data, tau_mapper(tau = .5))) %>% 
  mutate(models_4 = map(data, tau_mapper(tau = .95))) %>% 
  mutate(models_5 = map(data, tau_mapper(.99))) 

fitted_qmodels_summaries_tbl <- 
  fitted_qmodels_tbl %>% 
  # tidy
  mutate(models_coef_1 = map(models_1, ~tidy(.))) %>% 
  mutate(models_coef_2 = map(models_2, ~tidy(.))) %>% 
  mutate(models_coef_3 = map(models_3, ~tidy(.))) %>% 
  mutate(models_coef_4 = map(models_4, ~tidy(.))) %>% 
  mutate(models_coef_5 = map(models_5, ~tidy(.))) 

results_qmodels_tbl <- 
fitted_qmodels_summaries_tbl %>% 
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
  mutate(across(.col = 2:11, .fns = ~strtrim(., 6))) %>% 
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
  mutate(across(.col = 2:4, ~str_replace_all(.x, "\\[0]", "[0.0000]")))
  
print(results_qmodels_tbl, n =100)

#  Rolling regressions ----------------------------------------------------
# # OLS -------------------------------------------------------------------

# rolling_reg_spec <-
#   slidify(
#     .f =  ~coeftest(lm(..1 ~ ..2 + ..3)),
#     .period = 250,
#     .align = "right",
#     .unlist = FALSE,
#     .partial = FALSE
#   )
# 
# models_rol <-
#   result_csad_cssd_tbl %>% 
#   mutate(CSAD_model_rolling  = rolling_reg_spec(CSAD, abs(Mkt), I(Mkt ^ 2))) %>% 
#   mutate(CSSD_model_rolling  = rolling_reg_spec(CSSD, abs(Mkt), I(Mkt ^ 2)))
# CSAD_model_rol_tbl <- 
#   unnest_rol_col(data = models_rol, rol_column = CSAD_model_rolling)
# CSSD_model_rol_tbl <- 
#   unnest_rol_col(data = models_rol, rol_column = CSSD_model_rolling)
# 
# # Graphing ---------------------------------------------------------------
# CSAD_model_rol_gg <- 
#   CSAD_model_rol_tbl %>% 
#   pivot() %>% 
#   mutate(Series = dplyr::recode(
#     Series,
#     "a0" = "gamma[0]",
#     "a1" = "gamma[1]",
#     "a2" = "gamma[2]",
#     "t-statistic a0" = "t-statistic:gamma[0]",
#     "t-statistic a1" = "t-statistic:gamma[1]",
#     "t-statistic a2" = "t-statistic:gamma[2]"
#   )) %>% 
#   fx_recode_plot(variables_color = 6)
# 
# CSSD_model_rol_gg <- 
#   CSSD_model_rol_tbl %>% 
#   pivot() %>% 
#   mutate(Series = dplyr::recode(
#     Series,
#     "a0" = "gamma[0]",
#     "a1" = "gamma[1]",
#     "a2" = "gamma[2]",
#     "t-statistic a0" = "t-statistic:gamma[0]",
#     "t-statistic a1" = "t-statistic:gamma[1]",
#     "t-statistic a2" = "t-statistic:gamma[2]"
#   )) %>% 
#   fx_recode_plot(variables_color = 6)


# Export ---------------------------------------------------------------
artifacts_general_herding <- list (
  models = list(
    results_models  = results_models,
    results_qmodels_tbl = results_qmodels_tbl
    # CSAD_model_rol_tbl = CSAD_model_rol_tbl,
    # CSSD_model_rol_tbl = CSSD_model_rol_tbl
  ),
  graphs = list(
    # CSSD_model_rol_gg = CSSD_model_rol_gg,
    # CSAD_model_rol_gg = CSAD_model_rol_gg
  )
)

write_rds(artifacts_general_herding, file = here("Outputs", "artifacts_general_herding.rds"))


