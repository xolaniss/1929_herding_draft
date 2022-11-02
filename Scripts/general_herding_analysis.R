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
library(fDMA)
library(vars)
library(urca)
library(mFilter)
library(car)
library(quantreg)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
unnest_rol_col <- function(data, rol_column) {
  data %>% 
    mutate(glance = map({{ rol_column }}, broom::tidy)) %>% 
    unnest(glance) %>% 
    dplyr::select(Date, term:estimate, statistic) %>% 
    drop_na() %>% 
    pivot_wider(names_from = term, values_from = c(estimate, statistic)) %>% 
    dplyr::rename("a0" = `estimate_(Intercept)`,
                  "a1" = `estimate_..2`,
                  "a2" = `estimate_..3`,
                  "t-statistic a0" = `statistic_(Intercept)`,
                  "t-statistic a1" = `statistic_..2`,
                  "t-statistic a2" = `statistic_..3` ) # may delete depending on number of variables or focus
}

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_resuls_tbl <- result_csad_cssd$combined_results_tbl

# Static regressions ------------------------------------------------------
# # OLS -------------------------------------------------------------------
fitted_models <- combined_resuls_tbl %>% 
  rename("Mkt" = "Market Return") %>% 
  group_by(Category) %>% 
  do(model = lm(CSAD ~ abs(Mkt) + I(Mkt ^ 2), data = .))

fitted_csad_models <- 
  fitted_models$model %>% 
  set_names(unique(combined_resuls_tbl$Category))

# # # QR --------------------------------------------------------------------
# quants <- c(.01, .25, .5, .75, .99)
# 
# CSSD_mode_qr_list <-
#   map(quants,
#   ~rq(CSSD ~ abs(Mkt) + I(Mkt ^ 2), 
#      tau= .x,
#      data = result_csad_cssd_tbl)
#   ) 
# 
# CSAD_model_qr_list <-
#   map(quants,
#       ~rq(CSAD ~ abs(Mkt) + I(Mkt ^ 2), 
#           tau= .x,
#           data = result_csad_cssd_tbl)
#   ) 
# 
# CSSD_mode_qr_gg <- rq(CSSD ~ abs(Mkt) + I(Mkt ^ 2), 
#                    tau= quants,
#                    data = result_csad_cssd_tbl) %>% 
#   summary() %>% 
#   plot()
# 
# CSAD_mode_qr_gg <- rq(CSAD ~ abs(Mkt) + I(Mkt ^ 2), 
#                       tau= quants,
#                       data = result_csad_cssd_tbl) %>% 
#   summary() %>% 
#   plot()


#  Rolling regressions ----------------------------------------------------
# # OLS -------------------------------------------------------------------

# rolling_reg_spec <-
#   slidify(
#     .f =  ~coeftest(lm(..1 ~ ..2 + ..3)),
#     .period = 250,
#     .align = "right",
#     .unlist = FALSE,
#     .partial = FALSE
#   )
# 
# models_rol <-
#   result_csad_cssd_tbl %>% 
#   mutate(CSAD_model_rolling  = rolling_reg_spec(CSAD, abs(Mkt), I(Mkt ^ 2))) %>% 
#   mutate(CSSD_model_rolling  = rolling_reg_spec(CSSD, abs(Mkt), I(Mkt ^ 2)))
# CSAD_model_rol_tbl <- 
#   unnest_rol_col(data = models_rol, rol_column = CSAD_model_rolling)
# CSSD_model_rol_tbl <- 
#   unnest_rol_col(data = models_rol, rol_column = CSSD_model_rolling)
# 
# # Graphing ---------------------------------------------------------------
# CSAD_model_rol_gg <- 
#   CSAD_model_rol_tbl %>% 
#   pivot() %>% 
#   mutate(Series = dplyr::recode(
#     Series,
#     "a0" = "gamma[0]",
#     "a1" = "gamma[1]",
#     "a2" = "gamma[2]",
#     "t-statistic a0" = "t-statistic:gamma[0]",
#     "t-statistic a1" = "t-statistic:gamma[1]",
#     "t-statistic a2" = "t-statistic:gamma[2]"
#   )) %>% 
#   fx_recode_plot(variables_color = 6)
# 
# CSSD_model_rol_gg <- 
#   CSSD_model_rol_tbl %>% 
#   pivot() %>% 
#   mutate(Series = dplyr::recode(
#     Series,
#     "a0" = "gamma[0]",
#     "a1" = "gamma[1]",
#     "a2" = "gamma[2]",
#     "t-statistic a0" = "t-statistic:gamma[0]",
#     "t-statistic a1" = "t-statistic:gamma[1]",
#     "t-statistic a2" = "t-statistic:gamma[2]"
#   )) %>% 
#   fx_recode_plot(variables_color = 6)


# Export ---------------------------------------------------------------
artifacts_general_herding <- list (
  models = list(
    fitted_csad_models = fitted_csad_models
    # CSAD_model_rol_tbl = CSAD_model_rol_tbl,
    # CSSD_model_rol_tbl = CSSD_model_rol_tbl
  ),
  graphs = list(
    # CSSD_model_rol_gg = CSSD_model_rol_gg,
    # CSAD_model_rol_gg = CSAD_model_rol_gg
  )
)

write_rds(artifacts_general_herding, file = here("Outputs", "artifacts_general_herding.rds"))


