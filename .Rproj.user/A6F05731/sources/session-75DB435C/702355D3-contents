# Description
# Calculation of CSAD using weighted returns by Xolani Sibande 20 October 2022

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

# 0.0 Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "results_functions.R"))

# 1.0 Import -------------------------------------------------------------
returns <- read_rds(here("Outputs", "artifacts_returns_data.rds"))
equal_returns_tbl <- returns$full_data$equal_returns_tbl 
fama_french <-  read_rds(here("Outputs", "artifacts_fama_french.rds"))
fama_french_tbl <- fama_french$data$fama_french_tbl

# 2.0 All industries ----------------------------------------------------------

# 2.1 Market return  ---------------------------------------------------------------
fama_french_tbl <- 
  fama_french_tbl %>% 
  mutate(Mkt = `Mkt-RF` + RF)

# 2.2 CSAD and CSSD --------------------------------------------------------------------
csad_all_matrix <- csad(data = equal_returns_tbl[, -1], 
     market_return = as.matrix(fama_french_tbl[, 6]))  # market return taken from fama-french factors

cssd_all_matrix <- cssd(data = equal_returns_tbl[, -1], 
                    market_return = as.matrix(fama_french_tbl[, 6])) # market return taken from fama-french factors

results_all_industries_tbl <- 
  bind_cols(
    fama_french_tbl %>% dplyr::select(Date, Mkt) %>% drop_na(),
    csad_all_matrix,
    cssd_all_matrix
  ) %>%  
  rename("CSAD" = ...3, "CSSD" = ...4) 

# 2.3 Graphing ---------------------------------------------------------------
results_all_industries_gg <- 
  results_all_industries_tbl %>% 
  pivot() %>% 
  mutate(Series = dplyr::recode(Series,
                       "CSSD" = "CSSD[t]",
                       "CSAD" = "CSAD[t]",
                       "Mkt" = "R[mt]")) %>% 
  fx_recode_plot(variables_color = 3)


# 3.0 Consumable_group ----------------------------------------------------

# 3.1 Data ----------------------------------------------------------------

returns_consumables_group_tbl <- returns$groups$Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_tbl # including some NAs in some categories
returns_consumables_group_tbl %>% skim()

# 3.2 CSAD and CSSD -------------------------------------------------------
results_consumables_group_tbl <- results_tbl(data = returns_consumables_group_tbl)


# 3.3 Graphing ------------------------------------------------------------
results_consumables_group_gg <- 
  results_consumables_group_tbl %>% 
  results_gg()


# 4.0 Durables_group -----------------------------------------------------

# 4.1 Data ----------------------------------------------------------------
returns_durables_group_tbl <- 
returns$groups$Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_tbl

# 4.2 CSAD and CSSD -------------------------------------------------------
results_durables_group_tbl <- results_tbl(data = returns_durables_group_tbl)


# 4.3 Graphing ------------------------------------------------------------
results_durables_group_gg <- 
  results_durables_group_tbl %>% 
  results_gg()


# 5.0 Manuf_group ---------------------------------------------------------

# 5.1 Data ----------------------------------------------------------------
returns_manuf_group_tbl <- 
returns$groups$manufacturing_energy_utilities_group_equal_tbl

# 5.2 CSAD and CSSD -------------------------------------------------------
results_manuf_group_tbl <- results_tbl(data = returns_manuf_group_tbl)

# 5.3 Graphing ------------------------------------------------------------
results_manuf_group_gg <- 
  results_manuf_group_tbl %>% 
  results_gg()


# 6.0 Bus_group -----------------------------------------------------------

# 6.1 Data ----------------------------------------------------------------
returns_bus_group_tbl <- 
returns$groups$business_equipment_telephone_and_television_transmission_group_equal_tbl

# 6.2 CSAD and CSSD -------------------------------------------------------
results_bus_group_tbl <- results_tbl(data = returns_bus_group_tbl)

# 6.3 Graphing ------------------------------------------------------------
results_bus_group_gg <- 
  results_bus_group_tbl %>% 
  results_gg()


# 7.0 Health_group --------------------------------------------------------

# 7.1 Data ----------------------------------------------------------------
returns_health_group_tbl <- 
returns$groups$healthcare_medical_equipment_and_drugs_group_equal_tbl

# 7.2 CSAD and CSSD -------------------------------------------------------
results_health_group_tbl <- results_tbl(data = returns_health_group_tbl)

# 7.3 Graphing ------------------------------------------------------------
results_health_group_gg <- 
  results_health_group_tbl %>% 
  results_gg()

# 8.0 Mines_group  --------------------------------------------------------------

# 8.1 Data ----------------------------------------------------------------

returns_mines_group_tbl <- 
returns$groups$mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_tbl

# 8.2 CSAD and CSSD -------------------------------------------------------
results_mines_group_tbl <- results_tbl(data = returns_mines_group_tbl)

# 8.3 Graphing ------------------------------------------------------------
results_mines_group_gg <- 
  results_mines_group_tbl %>% 
  results_gg()


# Export ---------------------------------------------------------------
artifacts_csad_cssd <- list (
  data = list(
    results_all_industries_tbl = results_tbl,
    results_consumables_group_tbl = results_consumables_group_tbl,
    results_durables_group_tbl = results_durables_group_tbl,
    results_manuf_group_tbl = results_manuf_group_tbl,
    results_bus_group_tbl = results_bus_group_tbl,
    results_health_group_tbl = results_health_group_tbl,
    results_mines_group_tbl = results_mines_group_tbl
    
  ),
  graphs = list(
    results_all_industries_gg = results_all_industries_gg,
    results_consumables_group_gg = results_consumables_group_gg,
    results_durables_group_gg  = results_durables_group_gg,
    results_manuf_group_gg = results_manuf_group_gg,
    results_bus_group_gg = results_bus_group_gg,
    results_health_group_gg = results_health_group_gg,
    results_mines_group_gg = results_mines_group_gg 
  )
)

write_rds(artifacts_csad_cssd, file = here("Outputs", "artifacts_csad_cssd.rds"))


