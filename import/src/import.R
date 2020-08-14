#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/import/src/import.R
#
pacman::p_load("tidyverse", "janitor", "assertr", "tidycensus")

files <- list(

  az_2016 = here::here("HRW_pollingplaces/import/input/vip_az_2016primary/polling_location.txt"),
  az_2016_imp = here::here("HRW_pollingplaces/clean/input/az_2016_imported.rds"),

  az_2020 = here::here("HRW_pollingplaces/import/input/vip_az_2020primary/polling_location.txt"),
  az_2020_imp = here::here("HRW_pollingplaces/clean/input/az_2020_imported.rds"),

  az_2020_maricopa = here::here("HRW_pollingplaces/import/input/vip_az_maricopa_2020primary/polling_location.txt"),
  az_2020_imp_maricopa = here::here("HRW_pollingplaces/clean/input/az_2020_maricopa_imported.rds"),

  sc_2016 = here::here("HRW_pollingplaces/import/input/vip_sc_2016primary/polling_location.txt"),
  sc_2016_imp = here::here("HRW_pollingplaces/clean/input/sc_2016_imported.rds"),

  sc_2020 = here::here("HRW_pollingplaces/import/input/vip_sc_2020primary/polling_location.txt"),
  sc_2020_imp = here::here("HRW_pollingplaces/clean/input/sc_2020_imported.rds"),

  covid_imp = here::here("HRW_pollingplaces/clean/input/covid_imported.rds"), 
  
  az_census_imp = here::here("HRW_pollingplaces/clean/input/az_census_imp.rds"), 
  sc_census_imp = here::here("HRW_pollingplaces/clean/input/sc_census_imp.rds"))

# import VIP data

## creates a list of all files as connections
fileslist <- list(files$az_2016, files$az_2020, files$az_2020_maricopa, 
                  files$sc_2016, files$sc_2020)

stopifnot(length(fileslist) == 5)

inlist <- lapply(fileslist, function(x) {
  
  expected_cols <- c("id", "name", "address_line", 
                     "directions", "hours", "hours_open_id", 
                     "is_drop_box", "is_early_voting", "latitude", 
                     "longitude", "latlng_source", "photo_uri")
  
  x_df <- as.data.frame(read_csv(x, col_names = TRUE, na = "NA")) %>%
    clean_names() %>%
    verify(colnames(.) == expected_cols) %>%
    mutate_at(c("id", "name", "address_line", "hours"), as.character) %>%
    select(-c("hours_open_id", "is_drop_box","is_early_voting", "photo_uri")) %>%
    verify(ncol(.) == 8)
  
})

stopifnot(length(inlist) == 5)

# verifications will break with new data

az_2016_df <- as.data.frame(pluck(inlist, 1)) %>%
  verify(ncol(.) == 8 & nrow(.) == 342) %>%
  verify(min(id) == 401333310112 & max(id) == 4610933) %>%
  saveRDS(files$az_2016_imp)

az_2020_df <- as.data.frame(pluck(inlist, 2)) %>%
  verify(ncol(.) == 8 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999) %>%
  saveRDS(files$az_2020_imp)

az_2020_maricopa_df <- as.data.frame(pluck(inlist, 3)) %>%
  verify(ncol(.) == 8 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622) %>%
  saveRDS(files$az_2020_imp_maricopa)

sc_2016_df <- as.data.frame(pluck(inlist, 4)) %>%
  verify(ncol(.) == 8 & nrow(.) == 2194) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  saveRDS(files$sc_2016_imp)

sc_2020_df <- as.data.frame(pluck(inlist, 5)) %>%
  verify(ncol(.) == 8 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  saveRDS(files$sc_2020_imp)

# import covid-19 related data
# nrow will break with new data

expected_cols2 <- c("date", "county", "state", "fips", "cases", "deaths")

covid_data <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols2) %>%
  rename(date_rep = date) %>%
  filter(state == "Arizona" | state == "South Carolina") %>%
  verify(ncol(.) == 6 & nrow(.) == 9033) %>%
  saveRDS(files$covid_imp)

# import census data for SC and AZ ending in 2016

jrkey <- census_api_key("0e50711a6878668e3244305cfdd42faaa9e7a66c", install = TRUE)

# make states a list in case we want to change or add more
states <- (c("AZ", "SC"))

clist <- lapply(states, function(y) {
  
  expected_cols3 <- c("statefp",  "countyfp", "countyns", "affgeoid", "geoid",
                      "name_x", "lsad", "aland", "awater", "name_y", "value", 
                      "agegroup", "sex","race","hisp","geometry")
  
  cen <- get_estimates(state = y, 
                          geography = "county", 
                          year = 2016,
                          geometry = TRUE,
                          product = "characteristics", 
                          breakdown = c("AGEGROUP", "SEX", "RACE", "HISP"), 
                          breakdown_labels = TRUE,
                          key = jrkey, 
                          keep_geo_vars = TRUE) %>%
    clean_names() %>%
    verify(colnames(.) == expected_cols3) %>%
    mutate_at(c("statefp",  "countyfp", "countyns", "affgeoid", "geoid",
                "name_x", "lsad", "aland", "awater", "name_y",
                "agegroup", "sex","race","hisp"), as.factor) %>%
    mutate_at(c("value"), as.double) %>%
    verify(ncol(.) == 16)
  
})

# verifications will break with new data

az_census_df <- as.data.frame(pluck(clist, 1)) %>%
  verify(ncol(.) == 16 & nrow(.) == 31365) %>%
  verify(min(value) == 0 & max(value) == 4242997) %>%
  saveRDS(files$az_census_imp)

sc_census_df <- as.data.frame(pluck(clist, 2)) %>%
  verify(ncol(.) == 16 & nrow(.) == 96186) %>%
  verify(min(value) == 0 & max(value) == 498766) %>%
  saveRDS(files$sc_census_imp)

# done.