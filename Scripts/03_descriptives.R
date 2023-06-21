# Description
# Descriptives for 1929 paper by Xolani Sibande 2nd November 2022

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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
results <- read_rds(here("Outputs", "artifacts_csad_cssd.rds"))

combined_results_list <- list(
  "All industries" = results$data$results_all_industries_tbl,
  "Consumables group" = results$data$results_consumables_group_tbl,
  # "Durables group" = results$data$results_durables_group_tbl,
  "Health group"  = results$data$results_health_group_tbl,
  "Manufacturing group"  = results$data$results_manuf_group_tbl,
  "Mines group" = results$data$results_mines_group_tbl,
  "Business services group" = results$data$results_bus_group_tbl
)

# Cleaning -----------------------------------------------------------------
combined_results_tbl <- 
  combined_results_list %>% 
  map(~rename(., "Market Return" = "Mkt")) %>% 
  bind_rows(.id = "Category") %>% 
  relocate(Date, .before = "Category")
  # dplyr::select(-CSSD)

# Descriptives -------------------------------------------------------------

descriptives_tbl <- 
  combined_results_tbl %>% 
  drop_na() %>% 
  pivot_longer(cols = -c(Date, Category), names_to = "Variables", values_to = "Value") %>% 
  group_by(Category, Variables) %>% 
  summarise(across(.cols = -c(Date),
                   .fns = list(Median = median, 
                               SD = sd,
                               Min = min,
                               Max = max,
                               IQR = IQR,
                               Obs = ~ n()), 
                   .names = "{.fn}"))
  

# Export ---------------------------------------------------------------
artifacts_descriptives <- list (
  combined_results_list = combined_results_list,
  combined_results_tbl = combined_results_tbl,
  descriptives_tbl = descriptives_tbl
)


write_rds(artifacts_descriptives, file = here("Outputs", "artifacts_descriptives.rds"))


