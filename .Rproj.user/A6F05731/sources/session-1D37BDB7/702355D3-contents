# Description
# Calculation of CSAD using equal returns by Xolani Sibande 21 September 2022

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
library(fDMA)
library(vars)
library(urca)
library(mFilter)
library(car)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
csad <- function (data, market_return) {
  rowSums(abs((data - as.numeric(market_return))), na.rm = TRUE) / nrow(data)
} # market return taken from fama-french factors

cssd <- function (data, market_return) {
  sqrt(rowSums((data - as.numeric(market_return)) ^ 2, na.rm = TRUE) / nrow(data - 1))
} # market return taken from fama-french factors


# Import -------------------------------------------------------------
returns <- read_rds(here("Outputs", "artifacts_returns_data.rds"))
equal_returns_tbl <- returns$equal_returns_tbl 
fama_french <-  read_rds(here("Outputs", "artifacts_fama_french.rds"))
fama_french_tbl <- fama_french$data$fama_french_tbl

# Market return  ---------------------------------------------------------------
fama_french_tbl <- 
  fama_french_tbl %>% 
  mutate(Mkt = `Mkt-RF` + RF)

# CSAD --------------------------------------------------------------------
csad_matrix <- csad(data = equal_returns_tbl[, -1], 
     market_return = as.matrix(fama_french_tbl[, 6]))

cssd_matrix <- cssd(data = equal_returns_tbl[, -1], 
                    market_return = as.matrix(fama_french_tbl[, 6]))

results_tbl <- 
  bind_cols(
    fama_french_tbl %>% dplyr::select(Date, Mkt) %>% drop_na(),
    csad_matrix,
    cssd_matrix
  ) %>%  
  rename("CSAD" = ...3, "CSSD" = ...4) 

# Graphing ---------------------------------------------------------------
results_gg <- 
  results_tbl %>% 
  pivot() %>% 
  mutate(Series = dplyr::recode(Series,
                       "CSSD" = "CSSD[t]",
                       "CSAD" = "CSAD[t]",
                       "Mkt" = "R[mt]")) %>% 
  fx_recode_plot(variables_color = 3)

# Export ---------------------------------------------------------------
artifacts_csad_cssd <- list (
  data = list(
    results_tbl = results_tbl
  ),
  graphs = list(
    results_gg = results_gg 
)
)

write_rds(artifacts_csad_cssd, file = here("Outputs", "artifacts_csad_cssd.rds"))


