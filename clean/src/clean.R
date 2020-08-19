#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/clean/src/clean.R
#
pacman::p_load("tidyverse", "assertr", "janitor")

inputs <- list(
  VIPinlist_imp = here::here("clean/input/VIPdata_imported.rds"),
  covid_imp = here::here("clean/input/covid_imported.rds"),
  census_imp = here::here("clean/input/census_imported.rds")
)

outputs <- list(
  az_2016_clean = here::here("write/input/az_2016_clean.rds"),
  az_2016_freq_clean = here::here("write/input/az_2016_freq_clean.rds"),
  az_2020_clean = here::here("write/input/az_2020_clean.rds"),
  az_2020_freq_clean = here::here("write/input/az_2020_freq_clean.rds"),
  az_demo_clean = here::here("write/input/az_demo_clean.rds"),
  sc_2016_clean = here::here("write/input/sc_2016_clean.rds"),
  sc_2016_freq_clean = here::here("write/input/sc_2016_freq_clean.rds"),
  sc_2020_clean = here::here("write/input/sc_2020_clean.rds"),
  sc_2020_freq_clean = here::here("write/input/sc_2020_freq_clean.rds"),
  sc_demo_clean = here::here("write/input/sc_demo_clean.rds"), 
  covid_clean = here::here("write/input/covid_clean.rds") 
)

# VIP data
n_last <- 5
invalid_info <- c("4600331", "46002736", "4600617", "4600618", 
                  "4610205126","46002463", "4600178", "46003124")

# nrows = how many unique polling places were open in 2016?
az_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 1) %>%
  verify(ncol(.) == 3 & nrow(.) == 342) %>%
  verify(min(id) == 401333310112 & max(id) == 4610933) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  filter(address_line != "PLEASE CHECK THE COUNTY WEBSITE FOR THE VOTE") %>%
  mutate(zipcode = if_else(id == "46002418", "85142", zipcode), 
         zipcode = if_else(id == "46002423", "85544", zipcode), 
         zipcode = if_else(id == "46002453", "85553", zipcode),
         zipcode = if_else(id == "46002446", "85006", zipcode),
         zipcode = if_else(id == "46003324", "85501", zipcode),
         zipcode = if_else(id == "46002435", "85539", zipcode),
         zipcode = if_else(id == "46003328", "85648", zipcode),
         zipcode = if_else(id == "46002414", "85941", zipcode),
         zipcode = if_else(id == "46002442", "85541", zipcode),
         zipcode = if_else(id == "46002443", "85554", zipcode),
         zipcode = if_else(id == "46001841", "86511", zipcode),
         zipcode = if_else(id == "460041", "86040", zipcode),
         zipcode = if_else(id == "46003330", "85621", zipcode),
         zipcode = if_else(id == "46002452", "85541", zipcode), 
         zipcode = if_else(id == "46002429", "85541", zipcode),
         zipcode = if_else(id == "46002483", "85192", zipcode),
         zipcode = if_else(id == "46002417", "85501", zipcode),
         zipcode = if_else(id == "46003314", "85624", zipcode),
         zipcode = if_else(id == "46002420", "85541", zipcode),
         zipcode = if_else(id == "460019", "86040", zipcode),
         zipcode = if_else(id == "46006", "86020", zipcode),
         zipcode = if_else(id == "46001848", "85940", zipcode),
         zipcode = if_else(id == "460036", "86044", zipcode),
         zipcode = if_else(id == "46002412", "85941", zipcode),
         zipcode = if_else(id == "46003325", "85637", zipcode),
         zipcode = if_else(id == "46002737", "85346", zipcode)) %>%
  filter(!id %in% invalid_info) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(unique(id) != FALSE)  %>%
  verify(nrow(.) == 333 & ncol(.) == 4)

# nrows = number of unique zip codes in 2016 = 198
az_zips_freq_2016 <- as.data.frame(table(az_2016_df$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2016 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 198)

# 2020 data
az_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 2) %>%
  verify(ncol(.) == 3 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(zipcode != "12345") %>%
  filter(unique(id) != FALSE) %>%
  verify(nrow(.) == 329 & ncol(.) == 4)

az_2020_maricopa_df <- pluck(read_rds(inputs$VIPinlist_imp), 3) %>%
  verify(ncol(.) == 3 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(unique(id) != FALSE) %>%
  verify(nrow(.) == 151 & ncol(.) == 4)

# nrows = how many unique polling places were open in 2020?
az_2020_df_full <- full_join(az_2020_df, az_2020_maricopa_df) %>%
  verify(ncol(.) == 4 & nrow(.) == 480) %>%
  verify(min(id) == 7700104067 & max(id) == 8850015622)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(unique(id) != FALSE) %>%
  verify(nrow(.) == 480 & ncol(.) == 4)

# nrows = number of unique zip codes in 2020 = 278
az_zips_freq_2020 <- as.data.frame(table(az_2020_df_full$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2020 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 278)

# which zipcodes saw an increase, decrease, or maitenence in polling places?
n_places_az <- full_join(az_zips_freq_2020, az_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
         delta_cat = as.factor(if_else(delta_n_places < 0, 
                                       "Fewer Polling Places", 
                                       "The Same or More Polling Places"))) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 291)

### south carolina ###
#nrows = how many unique polling places were open in 2016?
sc_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 4) %>%
  verify(ncol(.) == 3 & nrow(.) == 2194) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  mutate(
    zipcode = as.character(if_else(zipcode == "23222", "29532", zipcode)),
    zipcode = as.factor(if_else(zipcode == "40 SC", "29840", zipcode))) %>%
  filter(zipcode != "	SC") %>%
  filter(unique(id) != FALSE) %>%
  verify(ncol(.) == 4 & nrow(.) == 2194) 

# nrows = number of unique zip codes in 2016 = 382
sc_zips_freq_2016 <- as.data.frame(table(sc_2016_df$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2016 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 382)

#nrows = how many unique polling places were open in 2016? 1968
sc_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 5) %>%
  verify(ncol(.) == 3 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  mutate(zipcode = as.factor(if_else(zipcode == "40 SC", "29840", zipcode))) %>%
  filter(zipcode != "SC") %>%
  filter(unique(id) != FALSE) %>%
  verify(ncol(.) == 4 & nrow(.) == 1968)

# nrows = number of unique zip codes in 2020 = 383
sc_zips_freq_2020 <- as.data.frame(table(sc_2020_df$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2020 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 383)

# which zipcodes saw an increase, decrease, or maitenence in polling places?
n_places_sc <- full_join(sc_zips_freq_2020, sc_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
         delta_cat = as.factor(if_else(delta_n_places < 0, 
                                       "Fewer Polling Places", 
                                       "The Same or More Polling Places"))) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 387)

# add in census race data and link to each state/year dataset grouped by zip
az_demo <- read_rds(inputs$census_imp) %>% 
  select(geoid, variable, estimate) %>% 
  filter(geoid %in% n_places_az$zipcode) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(zip_pct_hl = as.numeric(round(signif_half_up((yes_h_l/total)*100), 1), 
                                 digits = 2),
         zip_pct_not_hl = as.numeric(round(signif_half_up((no_h_l/total)*100), 1), 
                                     digits = 2)) %>%
  group_by(geoid) %>%
  full_join(n_places_az, by = c("geoid" = "zipcode")) %>%
  verify(ncol(.) == 10 & nrow(.) == 291) %>%
  saveRDS(outputs$az_demo_clean)

sc_demo <- pluck(read_rds(inputs$census_imp)) %>%
  select(geoid, variable, estimate) %>% 
  filter(geoid %in% n_places_sc$zipcode) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(zip_pct_hl = as.numeric(round(signif_half_up((yes_h_l/total)*100), 1), 
                                 digits = 2),
         zip_pct_not_hl = as.numeric(round(signif_half_up((no_h_l/total)*100), 1), 
                                     digits = 2)) %>%
  group_by(geoid) %>%
  full_join(n_places_sc, by = c("geoid" = "zipcode")) %>%
  verify(ncol(.) == 10 & nrow(.) == 387) %>%
  saveRDS(outputs$sc_demo_clean)

# export all objects for write task here so as not to have null objects for 
# demography data

# q1a
az_2016_df  <- az_2016_df %>%
  saveRDS(outputs$az_2016_clean)

az_2020_df_full  <- az_2020_df_full %>%
  saveRDS(outputs$az_2020_clean)

sc_2016_df <- sc_2016_df %>%
  saveRDS(outputs$sc_2016_clean)

sc_2020_df <- sc_2020_df %>%
  saveRDS(outputs$sc_2020_clean)

#q1b

az_zips_freq_2016 <- az_zips_freq_2016 %>%
  saveRDS(outputs$az_2016_freq_clean)

az_zips_freq_2020 <- az_zips_freq_2020 %>%
  saveRDS(outputs$az_2020_freq_clean)

sc_zips_freq_2016 <- sc_zips_freq_2016 %>%
  saveRDS(outputs$sc_2016_freq_clean)

sc_zips_freq_2020 <- sc_zips_freq_2020 %>%
  saveRDS(outputs$sc_2020_freq_clean)

# import covid-19 related data
expected_cols2 <- c("county", "state", "fips", "cases", "deaths")

covid_data <- readRDS(inputs$covid_imp) %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols2) %>%
  filter(state == "Arizona" | state == "South Carolina") %>%
  group_by(state, county) %>%
  summarize(cases_by_county = n()) %>%
  verify(ncol(.) == 3 & nrow(.) == 63) %>%
  saveRDS(outputs$covid_clean)

# done.