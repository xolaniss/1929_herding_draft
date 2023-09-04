# Description

# Analysis on crisis periods - Xolani Sibande 9 June 2023

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
library(quantreg)

options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_qr_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))
dummy_crisis <- function(data_date, start_date, end_date){
  ifelse(data_date > start_date & data_date < end_date, 1,0)
}

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_results_tbl <- result_csad_cssd$combined_results_tbl

# Dummies --------------------------------------------------------

## Covid Crisis --------------------------------------------------------
start <- as.Date("2020-3-09")
end <- as.Date("2020-12-31")

dummy_tbl <- tibble(
  Date  = seq(
    from = as.Date("1926-07-01"),
    to = as.Date("2022-07-29"),
    by = "day"
  ),
  dummy = dummy_crisis(
    data_date = Date,
    start_date = start,
    end_date = end
  ),
  anti_dummy = 1 - dummy
)

dummy_tbl %>% fx_plot()

# Combining data sets -----------------------------------------------------
combined_results_dummy_tbl <-
  combined_results_tbl %>% 
  left_join(dummy_tbl, by = c("Date" = "Date")) %>% 
  mutate(squared_market_returns = `Market Return`^2, 
         absolute_market_returns = abs(`Market Return`),
         dummy_abs = dummy*absolute_market_returns,
         anti_dummy_abs = anti_dummy*absolute_market_returns,
         dummy_squared = dummy*squared_market_returns,
         anti_dummy_squared = anti_dummy*squared_market_returns)

combined_dummy_gg <- 
  combined_results_dummy_tbl %>% 
  ggplot(aes(x = Date, y = dummy, group = Category)) +
  geom_line() +
  facet_wrap( . ~ Category)

# Regressions ---------------------------------------------------------------
formula <- as.formula(
  CSAD ~
    dummy_abs +
    anti_dummy_abs +
    dummy_squared +
    anti_dummy_squared
)

## OLS ---------------------------------------------------------------------
ols_tbl <-
  combined_results_dummy_tbl %>% 
  ols_group_workflow() %>% 
  relocate(dummy_abs, .after = `(Intercept)`) %>%
  relocate(dummy_squared, .before = anti_dummy_squared)

## QR ----------------------------------------------------------------------
qr_tbl <- 
  combined_results_dummy_tbl %>% 
  qmodels_group_workflow() %>% 
  relocate(dummy_abs, .after = `(Intercept)`) %>%
  relocate(dummy_squared, .before = anti_dummy_squared)

## Rolling -----------------------------------------------------------------
models_rol <- 
  combined_results_dummy_tbl %>% 
  ols_slidify_models_crisis() 

# Graphing ---------------------------------------------------------------
rol_gg <-
  models_rol %>%
  fx_recode_prep_crisis() %>% 
  fx_recode_plot(variables_color = 6, ncol = 2, nrow = 5)

# Export ---------------------------------------------------------------
artifacts_herding_covid_crisis <- list (
  models = list(
    ols_tbl  = ols_tbl,
    qr_tbl = qr_tbl
  ),
  graphs = list(
    rol_gg = rol_gg
  )
)

write_rds(artifacts_herding_covid_crisis, file = here("Outputs", "artifacts_herding_covid_crisis.rds"))


