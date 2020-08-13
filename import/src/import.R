#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/import/src/import.R
#

pacman::p_load("tidyverse", "janitor", "assertr")

here::here()

files <- list(

  az_2016 = here::here("import/input/vip_az_2016primary/polling_location.txt"),
  az_2016_imp = here::here("clean/input/az_2016_imported.rds"),

  az_2020 = here::here("import/input/vip_az_2020primary/polling_location.txt"),
  az_2020_imp = here::here("clean/input/az_2020_imported.rds"),

  az_2020_maricopa = here::here("import/input/vip_az_maricopa_2020primary/polling_location.txt"),
  az_2020_imp_maricopa = here::here("clean/input/az_2020_maricopa_imported.rds"),

  sc_2016 = here::here("import/input/vip_sc_2016primary/polling_location.txt"),
  sc_2016_imp = here::here("clean/input/sc_2016_imported.rds"),

  sc_2020 = here::here("import/input/vip_sc_2020primary/polling_location.txt"),
  sc_2020_imp = here::here("clean/input/sc_2020_imported.rds"),

  covid_imp = here::here("clean/input/covid_imported.rds"))

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
    select(-c("hours_open_id", "is_drop_box","is_early_voting", "latitude", 
              "longitude", "latlng_source", "photo_uri")) %>%
    verify(ncol(.) == 5)
  
})

stopifnot(length(inlist) == 5)

# verifications will break with new data

az_2016_df <- as.data.frame(pluck(inlist, 1)) %>%
  verify(ncol(.) == 5 & nrow(.) == 342) %>%
  verify(min(id) == 401333310112 & max(id) == 4610933) %>%
  saveRDS(files$az_2016_imp)

az_2020_df <- as.data.frame(pluck(inlist, 2)) %>%
  verify(ncol(.) == 5 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999) %>%
  saveRDS(files$az_2020_imp)

az_2020_maricopa_df <- as.data.frame(pluck(inlist, 3)) %>%
  verify(ncol(.) == 5 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622) %>%
  saveRDS(files$az_2020_imp_maricopa)

sc_2016_df <- as.data.frame(pluck(inlist, 4)) %>%
  verify(ncol(.) == 5 & nrow(.) == 2194) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  saveRDS(files$sc_2016_imp)

sc_2020_df <- as.data.frame(pluck(inlist, 5)) %>%
  verify(ncol(.) == 5 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  saveRDS(files$sc_2020_imp)

# import covid-19 related data

expected_cols2 <- c("date", "county", "state", "fips", "cases", "deaths")

covid_data <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols2) %>%
  rename(date_rep = date) %>%
  filter(state == "Arizona" | state == "South Carolina") %>%
  verify(ncol(.) == 6 & nrow(.) == 8971) %>%
  saveRDS(files$covid_imp)

# done.