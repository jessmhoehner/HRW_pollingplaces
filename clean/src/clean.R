#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/clean/src/clean.R
#
pacman::p_load("tidyverse", "janitor", "assertr")

inputs <- list(
  VIPinlist_imp = here::here("HRW_pollingplaces/clean/input/VIPdata_imported.rds"),
  covid_imp = here::here("HRW_pollingplaces/clean/input/covid_imported.rds"),
  census_imp = here::here("HRW_pollingplaces/clean/input/census_imported.rds")
)

outputs <- list(
  az_2016_clean = here::here("HRW_pollingplaces/write/input/az_2016_clean.rds"),
  az_2020_clean = here::here("HRW_pollingplaces/write/input/az_2020_clean.rds"),
  sc_2016_clean = here::here("HRW_pollingplaces/write/input/sc_2016_clean.rds"),
  sc_2020_clean = here::here("HRW_pollingplaces/write/input/sc_2020_clean.rds")
  )

# clean VIP data
# keep at county level for now, like Brennan report, and 

# combine 2020 az data with maricopa-specific data
# parse and create var for zip code, verify they are in range for each state
# rewrite this to read RDs as a list of listst

inlist <- lapply(input_vip, function(x) {
  
  expected_cols <- c("id", "name", "address_line")
  n_last <- 5
  
  x_df <- readRDS(x) %>%
    clean_names() %>%
    verify(colnames(.) == expected_cols) %>%
    mutate_at(c("id", "name", "address_line"), as.character) %>%
    verify(ncol(.) == 3) %>%
    mutate(zipcode = substr(address_line, nchar(address_line) - n_last + 1, 
                            nchar(address_line)))
})

stopifnot(length(inlist) == 5)

# verifications will break with new data

az_2016_df <- as.data.frame(pluck(inlist, 1)) %>%
  verify(ncol(.) == 4 & nrow(.) == 342) %>%
  verify(min(id) == 401333310112 & max(id) == 4610933) %>%
  verify(is.na(zipcode) == FALSE)

az_2020_df <- as.data.frame(pluck(inlist, 2)) %>%
  verify(ncol(.) == 4 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999)  %>%
  verify(is.na(zipcode) == FALSE)

az_2020_maricopa_df <- as.data.frame(pluck(inlist, 3)) %>%
  verify(ncol(.) == 4 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622) %>%
  verify(is.na(zipcode) == FALSE)

# join all az 2020 data together to include maricopa

az_2020_df_full <- full_join(az_2020_df, az_2020_maricopa_df) %>%
  verify(ncol(.) == 4 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622) %>%
  verify(is.na(zipcode) == FALSE)

sc_2016_df <- as.data.frame(pluck(inlist, 4)) %>%
  verify(ncol(.) == 4 & nrow(.) == 2194) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  verify(is.na(zipcode) == FALSE)

sc_2020_df <- as.data.frame(pluck(inlist, 5)) %>%
  verify(ncol(.) == 4 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  verify(is.na(zipcode) == FALSE)

# add in census race data and link to each state/year dataset groupoed by county
# and with zip

# import covid-19 related data

expected_cols2 <- c("date", "county", "state", "fips", "cases", "deaths")

covid_data <- readRDS(input_covid$covid_imp) %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols2) %>%
  rename(date_rep = date) %>%
  filter(state == "Arizona" | state == "South Carolina") %>%
  verify(ncol(.) == 6 & nrow(.) == 8971) %>%
  saveRDS(files$covid_imp)

# create 1 rds which is a list of dataframes with each state/year combo 
# which inclides zip, county, cases, demographics

# done.