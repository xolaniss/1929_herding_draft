# Description

# Analysis on crisis periods - Xolani Sibande 7 June 2023

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

## Financial Crisis --------------------------------------------------------
start_gfc <- as.Date("2006-12-31")
end_gfc <- as.Date("2008-12-31")

dummy_tbl <- tibble(
  Date  = seq(
    from = as.Date("1926-07-01"),
    to = as.Date("2022-07-29"),
    by = "day"
  ),
  dummy_gfc = dummy_crisis(
    data_date = Date,
    start_date = start_gfc,
    end_date = end_gfc
  ),
  anti_dummy_gfc = 1 - dummy_gfc
)

dummy_tbl %>% fx_plot()

# Combining data sets -----------------------------------------------------
combined_results_dummy_tbl <-  
  combined_results_tbl %>% 
  left_join(dummy_tbl, by = c("Date" = "Date"))


# Regressions ---------------------------------------------------------------
formula <- as.formula(
  CSAD ~
    dummy_gfc:abs(`Market Return`) +
    anti_dummy_gfc:abs(`Market Return`) +
    dummy_gfc:I(`Market Return` ^ 2) +
    anti_dummy_gfc:I(`Market Return` ^ 2)
)

## OLS ---------------------------------------------------------------------
ols_tbl <- 
  combined_results_dummy_tbl %>% 
  ols_group_workflow() %>% 
  relocate(`dummy_gfc:abs(\`Market Return\`)`, .before = `abs(\`Market Return\`):anti_dummy_gfc`) %>% 
  relocate(`dummy_gfc:I(\`Market Return\`^2)` , .before = `anti_dummy_gfc:I(\`Market Return\`^2)`)

## QR ----------------------------------------------------------------------
qr_tbl <- 
  combined_results_dummy_tbl %>% 
  qmodels_group_workflow() %>% 
  relocate(`dummy_gfc:abs(\`Market Return\`)`, .before = `abs(\`Market Return\`):anti_dummy_gfc`) %>% 
  relocate(`dummy_gfc:I(\`Market Return\`^2)` , .before = `anti_dummy_gfc:I(\`Market Return\`^2)`)


## Rolling -----------------------------------------------------------------



# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_herding_crisis <- list (
  models = list(
    ols_tbl  = ols_tbl,
    qr_tbl = qr_tbl
  ),
  graphs = list(
  )
)

write_rds(artifacts_herding_crisis, file = here("Outputs", "artifacts_herding_crisis.rds"))


