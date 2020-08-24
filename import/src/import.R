#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/import/src/import.R
#
pacman::p_load("tidyverse", "janitor", "assertr", "tidycensus", "textreadr")

# AZ covid-19 data by zip obtained here
# https://azdhs.gov/preparedness/epidemiology-disease-control/infectious-disease-epidemiology/covid-19/dashboards/index.php

# SC covid-19 data by zip obtained here
# https://www.scdhec.gov/infectious-diseases/viruses/coronavirus-disease-2019-covid-19/sc-cases-county-zip-code-covid-19

inputs <- list(
  az_2016 = here::here("import/input/vip_az_2016primary/polling_location.txt"),
  az_2020 = here::here("import/input/vip_az_2020primary/polling_location.txt"),
  az_2020_maricopa = here::here("import/input/vip_az_maricopa_2020primary/polling_location.txt"),
  sc_2016 = here::here("import/input/vip_sc_2016primary/polling_location.txt"),
  sc_2020 = here::here("import/input/vip_sc_2020primary/polling_location.txt"),
  az_covid_zip = here::here("import/input/covid/COVID19CONFIRMED_BYZIP_excel.csv"),
  sc_covid_zip = here::here("import/input/covid/TableOption2.pdf"))

outputs <- list(
  VIPinlist_imp = here::here("clean/input/VIPdata_imported.rds"),
  covid_az_imp = here::here("clean/input/covid_az_imported.rds"),
  census_imp = here::here("clean/input/census_imported.rds"),
  covid_sc_imp = here::here("clean/input/covid_sc_imported.rds"))

# import VIP data
## creates a list of VIP files as connections
inputslist <- list(inputs$az_2016, inputs$az_2020, inputs$az_2020_maricopa,
                  inputs$sc_2016, inputs$sc_2020)

names(inputslist) <- c("az_2016", "az_2020", "az_2020_maricopa",
                       "sc_2016", "sc_2020")

stopifnot(length(inputslist) == 5)

# verifications won't break with new data yet

inlist <- lapply(inputslist, function(x) {

  expected_cols <- c("id", "name", "address_line")

  x_df <- read_csv(x, col_names = TRUE, na = "NA",
                   col_types = cols_only(id = 'c',
                                         name = 'c',
                                         address_line = 'c')) %>%
    clean_names() %>%
    verify(colnames(.) == expected_cols) %>%
    verify(ncol(.) == 3)

})

stopifnot(length(inlist) == 5)
saveRDS(inlist, outputs$VIPinlist_imp)

# verifications will break with new data

# import covid-19 related data
# nrow will break with new data

expected_cols2 <- c("postcode","confirmed_case_category","confirmed_case_count")

az_covid_data <- read_csv(inputs$az_covid_zip, col_names = TRUE) %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols2) %>%
  rename(zipcode = postcode) %>%
  mutate_at(c("confirmed_case_category","confirmed_case_count"), as.factor) %>%
  verify(ncol(.) == 3 & nrow(.) == 410)

expected_cols2 <- c("postcode","confirmed_case_category","confirmed_case_count")

sc_covid_data <- read_pdf(inputs$sc_covid_zip, skip = 1,
                          trim = TRUE, remove.empty = TRUE) %>%
  clean_names() %>%
  select(text)

# import census data for SC and AZ ending in 2018
# data come from 2014-2018 5 year ACS

jrkey <- census_api_key("0e50711a6878668e3244305cfdd42faaa9e7a66c")

expected_cols3 <- c("geoid", "name", "variable", "estimate", "moe")

demo_1418 <- get_acs(geography = "zcta",
                     variables = c(total = "B03003_001",
                                   not_h_l = "B03003_002",
                                   h_l = "B03003_003"),
                     year = 2018,
                     geometry = FALSE,
                     key = jrkey) %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols3) %>%
  select(geoid, name, variable, estimate) %>%
  saveRDS(outputs$census_imp)

az_covid_data <- az_covid_data %>%
  saveRDS(outputs$covid_az_imp)

sc_covid_data <- sc_covid_data %>%
  saveRDS(outputs$covid_sc_imp)

# done.