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

# Functions ---------------------------------------------------------------
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_qr_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R")

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_resuls_tbl <- result_csad_cssd$combined_results_tbl


# Transformations --------------------------------------------------------


# Regressions ---------------------------------------------------------------


## OLS ---------------------------------------------------------------------



## QR ----------------------------------------------------------------------



## Rolling -----------------------------------------------------------------



# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


