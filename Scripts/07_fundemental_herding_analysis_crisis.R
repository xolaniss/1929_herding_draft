# Description
# Fundamental and non fundamental herding in time of crisis Xolani Sibande - 17 October 2023
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
combined_fundamental <- read_rds(here("Outputs", "artifacts_fundamental_herding.rds"))
combined_fundamental_tbl <- combined_fundamental$ols$combined_fundamental_tbl

# EDA ---------------------------------------------------------------
combined_fundamental_tbl %>% skim()



# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


