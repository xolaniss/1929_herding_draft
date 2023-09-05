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
source(here("Functions", "group_qr_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_results_tbl <- result_csad_cssd$combined_results_tbl

# Regressions ------------------------------------------------------
## OLS -------------------------------------------------------------------
formula <-  as.formula(CSAD ~ abs(`Market Return`) + I(`Market Return` ^ 2))
ols_tbl <- 
  combined_results_tbl %>% 
  ols_group_workflow()
  
##  Rolling regressions ----------------------------------------------------
models_rol <-
  combined_results_tbl %>%
  ols_slidify_models_standard() %>% 
  unnest_rol_col_standard(rol_column = models) 


## Graphing ---------------------------------------------------------------
rol_gg <-
  models_rol %>%
  slidyfy_gg_workflow_standard()



# ## QR --------------------------------------------------------------------
# qr_tbl <- 
#   combined_results_tbl %>% 
#   qmodels_group_workflow()
# 


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


