# Description
# Fundemental versus non fundemental herding Xolani Sibande - 13 Ocotber 2023
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
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
cross_sectionals <- read_rds(here("Outputs", "artifacts_csad_cssd.rds"))
CSAD_crisis_tbl <- cross_sectionals$csad_crisis$results_all_industries_crisis_csad_tbl

fama_french <- read_rds(here("Outputs", "artifacts_fama_french.rds"))
fama_french_tbl <- fama_french$data$fama_french_tbl

# Combining -----------------------------------------------------------------
combined_tbl <- 
  CSAD_crisis_tbl %>% 
  left_join(
    fama_french_tbl,
    by = c("Date" = "Date")
  ) %>% 
  relocate(Crisis, .after = Date)


# EDA ---------------------------------------------------------------
combined_tbl %>% skim()

# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


