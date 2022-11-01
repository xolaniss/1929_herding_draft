# Description
# Industry returns data cleaning by Xolani Sibande 17 September 2022

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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
equal_returns <-
  read_csv(
    here("Data", "49_Industry_Portfolios_Daily_equal.csv"),
    skip = 1,
    na = c("-99.99", "999")
  ) %>%
  rename("Date" = ...1)
weighted_returns <-
  read_csv(
    here("Data", "49_Industry_Portfolios_Daily_weighted.csv"),
    skip = 9,
    na = c("-99.99", "999")
  ) %>%
  rename("Date" = ...1)


# Cleaning -----------------------------------------------------------------
names_vec <- as_vector(colnames(weighted_returns))
replace_vec <- c(
  "Agriculture" = "Agric",
  "Food Products" = "Food",
  "Candy and Soda" = "Soda",
  "Beer and Liquor" = "Beer",
  "Tobacco Products" = "Smoke",
  "Recreation" = "Toys",
  "Entertainment" = "Fun",
  "Printing and Publishing" = "Books",
  "Consumer Goods" = "Hshld",
  "Apparel" = "Clths",
  "Health Care" = "Hlth",
  "Medical Equipment" = "MedEq",
  "Chemicals" = "Chems",
  "Rubber and Plastic Products" = "Rubbr",
  "Textiles" = "Txtls",
  "Construction Materials" = "BldMt",
  "Construction" = "Cnstr",
  "Steel Works" = "Steel",
  "Fabricated Products" = "FabPr",
  "Machinery" = "Mach",
  "Electrical Equipment"  = "ElcEq",
  "Automobiles and Trucks" = "Autos",
  "Aircraft" = "Aero",
  "Shipbuilding and Railroad Equipment" = "Ships",
  "Defense" = "Guns",
  "Precious Metals" = "Gold",
  "Non-Metallic and Industrial Metal Mining" = "Mines",
  "Petroleum and Natural Gas" = "Oil",
  "Utilities" = "Util",
  "Communication" = "Telcm",
  "Personal Services" = "PerSv",
  "Business Services" = "BusSv",
  "Computers" = "Hardw",
  "Software" = "Softw",
  "Electronic Equipment" = "Chips",
  "Measuring and Control Equipment" = "LabEq",
  "Business Supplies" = "Paper",
  "Shipping Containers" = "Boxes",
  "Transportation" = "Trans",
  "Wholesale" = "Whlsl",
  "Retail" = "Rtail",
  "Restaurants, Hotels, Motels" = "Meals",
  "Banking" = "Banks",
  "Insurance" = "Insur",
  "Real Estate" = "RlEst",
  "Trading" = "Fin"
)

equal_returns_tbl <-
  equal_returns %>%
  rename(replace_vec) %>%
  mutate(Date = parse_date_time(Date, orders = "Ymd"))

colnames(equal_returns_tbl)

weighted_returns_tbl <-
  weighted_returns %>%
  rename(replace_vec) %>%
  mutate(Date = parse_date_time(Date, orders = "Ymd"))

# EDA --------------------------------------------------------
equal_returns_tbl %>%  skim()
weighted_returns_tbl %>% skim()


# Grouping ----------------------------------------------------------------
Consumer_durables_nondurables_wholesale_retail_some_services_group_vec <-
  c(
    "Agriculture",
    "Food Products",
    "Candy and Soda",
    "Beer and Liquor",
    "Tobacco Products",
    "Recreation",
    "Printing and Publishing",
    "Consumer Goods",
    "Apparel",
    "Personal Services",
    # overlaps
    "Wholesale",
    "Retail",
    "Restaurants, Hotels, Motels" #overlaps
  )

manufacturing_energy_utilities_group_vec <-
  c(
    "Chemicals",
    "Rubber and Plastic Products",
    "Machinery",
    # overlaps,
    "Aircraft",
    "Shipbuilding and Railroad Equipment",
    "Defense",
    "Coal",
    "Utilities",
    "Business Supplies" # overlaps
  )

business_equipment_telephone_and_television_transmission_group_vec <-
  c(
    "Electronic Equipment",
    "Automobiles and Trucks",
    "Communication",
    "Computers",
    "Software",
    "Electronic Equipment",
    "Measuring and Control Equipment"
  )


healthcare_medical_equipment_and_drugs_group_vec <-
  c("Health Care",
    "Medical Equipment",
    "Drugs")


mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_vec <-
  c(
    "Entertainment" ,
    "Textiles" ,
    "Construction Materials" ,
    "Construction" ,
    "Steel Works" ,
    "Fabricated Products" ,
    "Non-Metallic and Industrial Metal Mining" ,
    "Precious Metals" ,
    "Petroleum and Natural Gas" ,
    "Business Services" ,
    "Shipping Containers" ,
    "Transportation" ,
    "Banking",
    "Insurance",
    "Real Estate",
    "Trading" ,
    "Other",
    "Automobiles and Trucks"
  )




# Group 1 -----------------------------------------------------------------

Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_tbl <-
  equal_returns_tbl %>%
  dplyr::select(Date,
                Consumer_durables_nondurables_wholesale_retail_some_services_group_vec)

Consumer_durables_nondurables_wholesale_retail_some_services_group_weighted_tbl <-
  weighted_returns_tbl  %>%
  dplyr::select(Date,
                Consumer_durables_nondurables_wholesale_retail_some_services_group_vec)

# Group 2 -----------------------------------------------------------------
manufacturing_energy_utilities_group_equal_tbl <-
  equal_returns_tbl %>%
  dplyr::select(Date,
                manufacturing_energy_utilities_group_vec)

manufacturing_energy_utilities_group_weighted_tbl <-
  weighted_returns_tbl  %>%
  dplyr::select(Date,
                manufacturing_energy_utilities_group_vec)

# Group 3 -----------------------------------------------------------------

business_equipment_telephone_and_television_transmission_group_equal_tbl <-
  equal_returns_tbl %>%
  dplyr::select(Date,
                business_equipment_telephone_and_television_transmission_group_vec)

business_equipment_telephone_and_television_transmission_group_weighted_tbl <-
  weighted_returns_tbl  %>%
  dplyr::select(Date,
                business_equipment_telephone_and_television_transmission_group_vec)


# Group 4 -----------------------------------------------------------------
healthcare_medical_equipment_and_drugs_group_equal_tbl <-
  equal_returns_tbl %>%
  dplyr::select(Date,
                healthcare_medical_equipment_and_drugs_group_vec)

healthcare_medical_equipment_and_drugs_group_weighted_tbl <-
  weighted_returns_tbl  %>%
  dplyr::select(Date,
                healthcare_medical_equipment_and_drugs_group_vec)


# Group 5 -----------------------------------------------------------------

mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_tbl <-
  equal_returns_tbl %>%
  dplyr::select(Date,
                mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_vec)

mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_weighted_tbl <-
  weighted_returns_tbl  %>%
  dplyr::select(Date,
                mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_vec)


# Graphing ---------------------------------------------------------------
equal_returns_part_1_gg <-
  equal_returns_tbl %>%
  dplyr::select(Date, 2:25) %>%
  fx_plot(plotname = "%", variables_color = 24)
ggsave(here("Outputs", "equal_returns_part_1_gg.png"), dpi = 500)

equal_returns_part_2_gg <-
  equal_returns_tbl %>%
  dplyr::select(Date, 26:49) %>%
  fx_plot(plotname = "%", variables_color = 25)
ggsave(here("Outputs", "equal_returns_part_2_gg.png"), dpi = 500)

weighted_returns_part_1_gg <-
  weighted_returns_tbl %>%
  dplyr::select(Date, 2:25) %>%
  fx_plot(plotname = "%", variables_color = 24)
ggsave(here("Outputs", "weighted_returns_part_1_gg.png"), dpi = 500)

weighted_returns_part_2_gg <-
  weighted_returns_tbl %>%
  dplyr::select(Date, 26:49) %>%
  fx_plot(plotname = "%", variables_color = 25)
ggsave(here("Outputs", "weighted_returns_part_2_gg.png"), dpi = 500)

Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_gg <- 
Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_tbl %>% 
  fx_plot(variables_color = 13)
Consumer_durables_nondurables_wholesale_retail_some_services_group_weighted_gg <- 
Consumer_durables_nondurables_wholesale_retail_some_services_group_weighted_tbl %>% 
  fx_plot(variables_color = 13)

manufacturing_energy_utilities_group_equal_gg <- 
manufacturing_energy_utilities_group_equal_tbl %>% 
  fx_plot(variables_color = 9)
manufacturing_energy_utilities_group_weighted_gg <- 
manufacturing_energy_utilities_group_weighted_tbl %>% 
  fx_plot(variables_color = 9)

business_equipment_telephone_and_television_transmission_group_equal_gg <- 
business_equipment_telephone_and_television_transmission_group_equal_tbl %>% 
  fx_plot(variables_color = 6)
business_equipment_telephone_and_television_transmission_group_gg <- 
business_equipment_telephone_and_television_transmission_group_weighted_tbl %>% 
  fx_plot(variables_color = 6)

healthcare_medical_equipment_and_drugs_group_equal_gg <- 
healthcare_medical_equipment_and_drugs_group_equal_tbl %>% 
  fx_plot(variables_color = 3)
healthcare_medical_equipment_and_drugs_group_weighted_gg <- 
healthcare_medical_equipment_and_drugs_group_weighted_tbl %>% 
  fx_plot(variables_color = 3)

mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_gg <- 
mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_tbl %>% 
  fx_plot(variables_color = 18)
mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_weighted_gg <- 
mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_weighted_tbl %>% 
  fx_plot(variables_color = 18)

# Export ---------------------------------------------------------------
artifacts_returns_data <- list (
  full_data = list(
    equal_returns_tbl = equal_returns_tbl,
    weighted_returns_tbl = weighted_returns_tbl
  ),
  groups = list(
    Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_tbl = Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_tbl,
    Consumer_durables_nondurables_wholesale_retail_some_services_group_weighted_tbl = Consumer_durables_nondurables_wholesale_retail_some_services_group_weighted_tbl,
    manufacturing_energy_utilities_group_equal_tbl = manufacturing_energy_utilities_group_equal_tbl,
    manufacturing_energy_utilities_group_weighted_tbl = manufacturing_energy_utilities_group_weighted_tbl,
    business_equipment_telephone_and_television_transmission_group_equal_tbl = business_equipment_telephone_and_television_transmission_group_equal_tbl,
    business_equipment_telephone_and_television_transmission_group_weighted_tbl = business_equipment_telephone_and_television_transmission_group_weighted_tbl,
    healthcare_medical_equipment_and_drugs_group_equal_tbl = healthcare_medical_equipment_and_drugs_group_equal_tbl,
    healthcare_medical_equipment_and_drugs_group_weighted_tbl = healthcare_medical_equipment_and_drugs_group_weighted_tbl,
    mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_tbl = mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_tbl,
    mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_weighted_tbl = mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_weighted_tbl
  ),
  graphs = list(
    Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_gg = Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_gg,
    Consumer_durables_nondurables_wholesale_retail_some_services_group_weighted_gg = Consumer_durables_nondurables_wholesale_retail_some_services_group_weighted_gg,
    manufacturing_energy_utilities_group_equal_gg = manufacturing_energy_utilities_group_equal_gg,
    manufacturing_energy_utilities_group_weighted_gg = manufacturing_energy_utilities_group_weighted_gg,
    healthcare_medical_equipment_and_drugs_group_equal_gg = healthcare_medical_equipment_and_drugs_group_equal_gg,
    healthcare_medical_equipment_and_drugs_group_weighted_gg = healthcare_medical_equipment_and_drugs_group_weighted_gg,
    business_equipment_telephone_and_television_transmission_group_equal_gg = business_equipment_telephone_and_television_transmission_group_equal_gg,
    business_equipment_telephone_and_television_transmission_group_gg = business_equipment_telephone_and_television_transmission_group_gg,
    mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_gg = mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_gg,
    mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_weighted_gg = mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_weighted_gg
  )
    
)
write_rds(artifacts_returns_data,
          file = here("Outputs", "artifacts_returns_data.rds"))

artifacts_returns_graphs_equal <- list (equal_returns_part_1_gg = equal_returns_part_1_gg,
                                        equal_returns_part_2_gg = equal_returns_part_2_gg)
write_rds(
  artifacts_returns_graphs_equal,
  file = here("Outputs", "artifacts_returns_graphs_equal.rds")
)

artifacts_returns_graphs_weighted_part_1 <- list (weighted_returns_part_1_gg = weighted_returns_part_1_gg)
write_rds(
  artifacts_returns_graphs_weighted_part_1,
  file = here("Outputs", "artifacts_returns_graphs_weighted_part_1.rds")
)

artifacts_returns_graphs_weighted_part_2 <- list (weighted_returns_part_2_gg = weighted_returns_part_2_gg)
write_rds(
  artifacts_returns_graphs_weighted_part_2,
  file = here("Outputs", "artifacts_returns_graphs_weighted_part_2.rds")
)
