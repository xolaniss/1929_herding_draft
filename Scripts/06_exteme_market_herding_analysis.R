# Description
# Extreme market analysis by Xolani Sibande - 2 October 2023

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
library(quantmod)

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

options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_results_tbl <- result_csad_cssd$combined_results_tbl

# OLS ---------------------------------------------------------------------

## Formula ---------------------------------------------------------------
formula <-  as.formula(CSAD ~ abs(`Market Return`) + I(`Market Return` ^ 2))


## Top market ------------------------------------------------------------
combined_top_results_tbl <- 
  combined_results_tbl %>% 
  group_by(Category) %>% 
  slice_max(order_by = `Market Return`, prop = 0.05) 

ols_max_tbl <- 
  combined_top_results_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow() %>% 
  mutate(Extreme = "Top market (5% market returns)") %>% 
  ungroup()

## Bottom market --------------------------------------------------------
combined_bottom_retults_tbl <- 
  combined_results_tbl %>% 
  group_by(Category) %>% 
  slice_min(order_by = `Market Return`, prop = 0.05)

ols_min_tbl <- 
  combined_bottom_retults_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow() %>%
  mutate(Extreme = "Bottom market (5% market returns)") %>% 
  ungroup()

## Combined --------------------------------------------------------------
ols_extreme_market_tbl <- rbind(ols_max_tbl, ols_min_tbl)

# Rolling regressions -----------------------------------------------------

## Top market ------------------------------------------------------------
models_max_rol <-
  combined_top_results_tbl %>%
  dplyr::select(-Crisis) %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  ols_slidify_models_standard() %>% 
  unnest_rol_col_standard(rol_column = models) 

rol_max_coeff_gg <-
  models_max_rol %>%
  dplyr::select(- starts_with("t")) %>% 
  slidyfy_gg_workflow_standard() +
  theme(
    legend.position = "none")

rol_max_tstats_gg <-
  models_max_rol %>%
  dplyr::select(- starts_with("a")) %>% 
  slidyfy_gg_workflow_standard() +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 

rol_max_gg <- rol_max_coeff_gg / rol_max_tstats_gg

## Bottom market ---------------------------------------------------------
models_min_rol <-
  combined_top_results_tbl %>%
  dplyr::select(-Crisis) %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  ols_slidify_models_standard() %>% 
  unnest_rol_col_standard(rol_column = models) 

rol_min_coeff_gg <-
  models_min_rol %>%
  dplyr::select(- starts_with("t")) %>% 
  slidyfy_gg_workflow_standard() +
  theme(
    legend.position = "none")

rol_min_tstats_gg <-
  models_min_rol %>%
  dplyr::select(- starts_with("a")) %>% 
  slidyfy_gg_workflow_standard() +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 

rol_min_gg <- rol_min_coeff_gg / rol_min_tstats_gg

# Export ---------------------------------------------------------------
artifacts_extreme_market_herding <- list (
    ols = list(
      ols_extreme_market_tbl = ols_extreme_market_tbl
    ),
    rol = list(
      rol_max_gg = rol_max_gg,
      rol_min_gg = rol_min_gg
    )
)

write_rds(artifacts_extreme_market_herding, file = here("Outputs", "artifacts_extreme_market_herding.rds"))



