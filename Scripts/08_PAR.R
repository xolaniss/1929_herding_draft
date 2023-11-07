# Description
# Looking at PAR data options 6 November 2023 - Xolani Sibande

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
par <- read_excel(here("Data", "PAR.xlsx"))
pear <-  read_excel(here("Data", "PEAR (5).xlsx"), sheet = 2)

# Cleaning -----------------------------------------------------------------
par_tbl <- 
  par %>% 
  rename(Date = ...1) %>% 
  mutate(Date = str_replace_all(Date, "M", "-")) %>% 
  mutate(Date = parse_date_time(Date, orders = "Ym"))

pear_tbl <- 
  pear %>% 
  rename(Date = yearmonth) %>% 
  mutate(Date = parse_date_time(Date, orders = "Ym"))

# EDA ---------------------------------------------------------------
par_tbl %>% skim()
pear_tbl %>% skim()

# Graphing ---------------------------------------------------------------
combined_tbl <- 
  par_tbl %>% 
  left_join(pear_tbl, by = c("Date" = "Date"))

combined_gg <- 
  combined_tbl %>% fx_plot(variables_color = 2)

combined_gg

# Export ---------------------------------------------------------------
artifacts_presidential_ratings <- list (
  data = list(
    par_tbl = par_tbl,
    pear_tbl = pear_tbl
  ),
  graph = list(
    combined_gg = combined_gg
  )
)

write_rds(artifacts_presidential_ratings, file = here("Outputs", "artifacts_presidential_ratings.rds"))


