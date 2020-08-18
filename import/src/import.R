#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/import/src/import.R
#
pacman::p_load("tidyverse", "janitor", "assertr", "tidycensus")

inputs <- list(
  az_2016 = here::here("HRW_pollingplaces/import/input/vip_az_2016primary/polling_location.txt"),
  az_2020 = here::here("HRW_pollingplaces/import/input/vip_az_2020primary/polling_location.txt"),
  az_2020_maricopa = here::here("HRW_pollingplaces/import/input/vip_az_maricopa_2020primary/polling_location.txt"),
  sc_2016 = here::here("HRW_pollingplaces/import/input/vip_sc_2016primary/polling_location.txt"),
  sc_2020 = here::here("HRW_pollingplaces/import/input/vip_sc_2020primary/polling_location.txt"),
  zc1_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_COUNTY_032016.csv"),
  zc2_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_COUNTY_062016.csv"),
  zc3_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_COUNTY_092016.csv"),
  zc4_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_COUNTY_122016.csv"),
  zc1_2020 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_COUNTY_032020.csv"),
  zt1_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_TRACT_032016.csv"),
  zt2_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_TRACT_062016.csv"),
  zt3_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_TRACT_092016.csv"),
  zt4_2016 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_TRACT_122016.csv"),
  zt1_2020 = here::here("HRW_pollingplaces/import/input/usps_api/ZIP_TRACT_032020.csv")
  )

outputs <- list(
  VIPinlist_imp = here::here("HRW_pollingplaces/clean/input/VIPdata_imported.rds"),
  covid_imp = here::here("HRW_pollingplaces/clean/input/covid_imported.rds"),
  census_imp = here::here("HRW_pollingplaces/clean/input/census_imported.rds")
  )

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

expected_cols2 <- c("date", "county", "state", "fips", "cases", "deaths")

covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols2) %>%
  rename(date_rep = date) %>%
  select(-c(date_rep)) %>%
  filter(state == "Arizona" | state == "South Carolina") %>%
  verify(ncol(.) == 5 & nrow(.) == 9277) %>%
  saveRDS(outputs$covid_imp)

# import census data for SC and AZ ending in 2018
# data come from 2014-2018 5 year ACS

jrkey <- census_api_key("0e50711a6878668e3244305cfdd42faaa9e7a66c", install = TRUE)

expected_cols3 <- c("geoid", "name", "variable", "estimate", "moe")

demo_2016 <- get_acs(geography = "zcta", 
                     variables = c(total = "B03001_001", 
                                   no_h_l = "B03001_002", 
                                   yes_h_l = "B03001_012"), 
                     year = 2018, 
                     geometry = FALSE, 
                     key = jrkey) %>%
  janitor::clean_names() %>%
  verify(colnames(.) == expected_cols3) %>%
  select(geoid, name, variable, estimate) %>%
  saveRDS(outputs$census_imp)

# done.