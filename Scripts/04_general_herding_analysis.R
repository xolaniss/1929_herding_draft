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
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_results_tbl <- result_csad_cssd$combined_results_tbl

# OLS ------------------------------------------------------
## OLS -------------------------------------------------------------------
formula <-  as.formula(CSAD ~ abs(`Market Return`) + I(`Market Return` ^ 2))

ols_full_tbl <- 
  combined_results_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow() %>% 
  ungroup() %>% 
  mutate(Crisis = "Full Sample")

ols_crisis_tbl <- 
  combined_results_tbl %>% 
  filter(!Crisis == "No Crisis") %>% 
  ols_group_crisis_workflow() %>% 
  ungroup()

ols_tbl <- rbind(ols_full_tbl, ols_crisis_tbl) %>%
  arrange(Category) %>% 
  relocate(Crisis, .before = Category)
  
##  Rolling regressions ----------------------------------------------------
models_rol <-
  combined_results_tbl %>%
  mutate(Date = as.POSIXct(Date)) %>% 
  ols_slidify_models_standard() %>% 
  unnest_rol_col_standard(rol_column = models) 

## Graphing ---------------------------------------------------------------
rol_gg <-
  models_rol %>%
  dplyr::select(-Crisis) %>% 
  # mutate(Category = str_replace_all(Category, "All industries", "'All industries'"),
  #        Category = str_replace_all(Category, "Business services group", "'Business services group'"),
  #        Category = str_replace_all(Category, "Consumables group", "'Consumables group'"),
  #        Category = str_replace_all(Category, "Health group", "'Health group'"),
  #        Category = str_replace_all(Category, "Manufacturing group", "'Manufacturing group'"),
  #        Category = str_replace_all(Category, "Mines group", "'Mines group'")
         # ) %>% 
  slidyfy_gg_workflow_standard()


# Export ---------------------------------------------------------------
artifacts_general_herding <- list (
  models = list(
    ols_tbl  = ols_tbl
    # qr_tbl = qr_tbl
  ),
  graphs = list(
    rol_gg = rol_gg
  )
)

write_rds(artifacts_general_herding, file = here("Outputs", "artifacts_general_herding.rds"))


