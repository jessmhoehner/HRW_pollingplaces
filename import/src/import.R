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
# AZ and SC zipcode and county data obtained here
# https://www.unitedstateszipcodes.org/az/#zips-list and https://www.unitedstateszipcodes.org/sc/#zips-list

inputs <- list(
  az_2016 = here::here("import/input/vip_az_2016primary/polling_location.txt"),
  az_2020 = here::here("import/input/vip_az_2020primary/polling_location.txt"),
  az_2020_maricopa = here::here("import/input/vip_az_maricopa_2020primary/polling_location.txt"),

  sc_2016 = here::here("import/input/vip_sc_2016primary/polling_location.txt"),
  sc_2020 = here::here("import/input/vip_sc_2020primary/polling_location.txt"),
  az_covid_zip = here::here("import/input/covid/COVID19CONFIRMED_BYZIP_excel.csv"),
  sc_covid_zip = here::here("import/input/covid/TableOption2.pdf"),

  zip_counties = here::here("import/input/zips/zip_code_database.csv"),

  pa_2016 = here::here("import/input/penn/PollingPlaceList20161104.csv"),
  pa_2016_2 = here::here("import/input/penn/CopyofPollingPlacesList_20160425.csv"),
  pa_2020 = here::here("import/input/penn/PollingPlaceList20200601(1).csv")

)
outputs <- list(
  VIPinlist_imp = here::here("clean/input/VIPdata_imported.rds"),
  pa_2016_imp = here::here("clean/input/pa2016_imported.rds"),
  pa_2016_2_imp = here::here("clean/input/pa2016_2_imported.rds"),
  pa_2020_imp = here::here("clean/input/pa2020_imported.rds"),

  covid_az_imp = here::here("clean/input/covid_az_imported.rds"),
  covid_sc_imp = here::here("clean/input/covid_sc_imported.rds"),
  covid_pa_imp = here::here("clean/input/covid_pa_imported.rds"),

  census_imp = here::here("clean/input/census_imported.rds"),
  counnzip_azscpa_imp = here::here("clean/input/counzip_azscpa_imported.rds")
)

# import VIP data
## creates a list of VIP files as connections
inputslist <- list(inputs$az_2016, inputs$az_2020, inputs$az_2020_maricopa,
                   inputs$sc_2016, inputs$sc_2020)

names(inputslist) <- c("az_2016", "az_2020", "az_2020_maricopa",
                       "sc_2016", "sc_2020")

stopifnot(length(inputslist) == 5)

# verification won't break with new data yet

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

# data from 04 Nov 2016
expected_colspa2016 <- c("county_name","precinct_code","precinct_name","description",
                     "house_num","prefix_direction_desc","street","street_type_desc",
                     "suffixdirection_desc","city","state_desc","postal_code",
                     "line2","comment","day_phone","handicap_accessible")

pa2016_df <- read_csv(inputs$pa_2016, col_names = TRUE,
                      na = "NULL", col_types =
                        c(Comment = 'c',
                          HouseNum = 'c',
                          PostalCode = 'n')) %>%
  clean_names() %>%
  verify(colnames(.) == expected_colspa2016) %>%
  verify(ncol(.) == 16 & nrow(.) == 9160) %>%
  saveRDS(outputs$pa_2016_imp)

# data from 25 April 2016
expected_colspa2016_2 <- c("county_name", "precinct_code", "precinct_name",
                           "description", "house_num", "prefix_direction_desc",
                           "street", "street_type_desc", "suffix_direction_desc",
                           "city", "state_desc", "postal_code", "line2",
                           "comment", "day_phone")

pa2016_2_df <- read_csv(inputs$pa_2016_2, col_names = TRUE,
                      na = "NULL", col_types =
                        c(Comment = 'c',
                          HouseNum = 'c',
                          PostalCode = 'n',
                          PrecinctCode = 'c')) %>%
  clean_names() %>%
  verify(colnames(.) == expected_colspa2016_2) %>%
  verify(ncol(.) == 15 & nrow(.) == 9155) %>%
  saveRDS(outputs$pa_2016_2_imp)


expected_colspa2020 <- c("county_name","precinct_code","precinct_name",
                         "precinct_split_code","description","house_num",
                         "prefix_direction","street","street_type",
                         "suffix_direction","city","state","postal_code",
                         "line2","comment","day_phone","handicap_accessible")

pa2020_df <- read_csv(inputs$pa_2020, col_names = TRUE,
                      na = "NULL", col_types =
                        c(SuffixDirection = 'c',
                          Comment = 'c',
                          HouseNum = 'c',
                          PostalCode = 'n')) %>%
  clean_names() %>%
  verify(colnames(.) == expected_colspa2020) %>%
  verify(ncol(.) == 17 & nrow(.) == 9234) %>%
  saveRDS(outputs$pa_2020_imp)

# verification will break with new data

# import covid-19 related data
expected_cols2 <- c("postcode","confirmed_case_category","confirmed_case_count")

az_covid_data <- read_csv(inputs$az_covid_zip, col_names = TRUE) %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols2) %>%
  rename(zipcode = postcode) %>%
  mutate_at(c("confirmed_case_category","confirmed_case_count"), as.factor) %>%
  verify(ncol(.) == 3 & nrow(.) == 410) %>%
  saveRDS(outputs$covid_az_imp)

expected_cols25 <- c("county", "state", "cases")

# breaks with new covid data
sc_covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
                          col_types = cols_only(county = 'c',
                                                state = 'c',
                                                cases = 'd')) %>%
  clean_names() %>%
  filter(state == "South Carolina") %>%
  verify(colnames(.) == expected_cols25) %>%
  verify(state == "South Carolina") %>%
  verify(ncol(.) == 3 & nrow(.) == 7525) %>%
  saveRDS(outputs$covid_sc_imp)

# breaks with new covid data
pa_covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
                          col_types = cols_only(county = 'c',
                                                state = 'c',
                                                cases = 'd')) %>%
  clean_names() %>%
  filter(state == "Pennsylvania") %>%
  verify(colnames(.) == expected_cols25) %>%
  verify(state == "Pennsylvania") %>%
  verify(ncol(.) == 3 & nrow(.) == 10834) %>%
  saveRDS(outputs$covid_pa_imp)

# zips in SC and AZ
zc <- read_csv(inputs$zip_counties, col_names = TRUE, na = "",
               col_types = cols_only(zip = 'n',
                                     state = 'c',
                                     county = 'c')) %>%
  clean_names() %>%
  filter(state == "AZ" | state == "SC" | state == "PA") %>%
  filter(is.na(county) != TRUE) %>%
  verify(ncol(.) == 3 & nrow(.) == 3318) %>%
  verify(min(zip) == 15001 & max(zip) == 86556)

# import census data for SC and AZ ending in 2018
# data come from 2014-2018 5 year ACS
# bring in the geometry data with write task, it's messing up
# the clean task
jrkey <- census_api_key("0e50711a6878668e3244305cfdd42faaa9e7a66c")
expected_cols3 <- c("geoid", "name", "variable", "estimate", "moe")

demo_1418 <- get_acs(geography = "zcta",
                     variables = c(total = "B03002_001",
                                   total_nhl = "B03002_002",
                                   nhl_white = "B03002_003",
                                   nhl_black = "B03002_004",
                                   nhl_ai_an = "B03002_005",
                                   nhl_asian = "B03002_006",
                                   nhl_nhi_pi = "B03002_007",
                                   nhl_sor = "B03002_008",
                                   nhl_tom = "B03002_009",
                                   total_hl = "B03002_012"),
                     year = 2018,
                     geometry = FALSE,
                     key = jrkey) %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols3) %>%
  select(geoid, name, variable, estimate) %>%
  filter(geoid %in% zc$zip) %>%
  verify(ncol(.) == 4 & nrow(.) == 26270) %>%
  saveRDS(outputs$census_imp)

zc <- zc %>%
  saveRDS(outputs$counnzip_azscpa_imp)

# done.