#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/clean/src/clean.R
#
pacman::p_load("tidyverse", "assertr", "tidycensus")

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

# VIP data
# keep at county level for now

# combine 2020 az data with maricopa-specific data
# parse and create var for zip code, verify they are in range for each state

n_last <- 5

# verifications will break with new data
# added zipcodes for several polling places with no zipcode data

invalid_info <- c("4600331", "46002736", "4600617", "4600618", 
                  "4610205126","46002463", "4600178", "46003124")

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
  group_by(address_line) %>%
  slice(1) %>% 
  ungroup()

#how many polling places were open in each zip code in 2016?

az_zips_freq_2016 <- as.data.frame(table(az_2016_df$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2016 = as.numeric(Freq))

az_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 2) %>%
  verify(ncol(.) == 3 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(zipcode != "12345") %>%
  group_by(address_line) %>%
  slice(1) %>% 
  ungroup()

no_overlap_az <- anti_join(az_2020_df, az_2016_df, by = "address_line")

# there isn't much overlap between polling locationss open from 2016 to 2020, 
# would we expect that most polling places would have changed completely?

az_2020_maricopa_df <- pluck(read_rds(inputs$VIPinlist_imp), 3) %>%
  verify(ncol(.) == 3 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  group_by(address_line) %>%
  slice(1) %>% 
  ungroup()

az_2020_df_full <- full_join(az_2020_df, az_2020_maricopa_df) %>%
  verify(ncol(.) == 4 & nrow(.) == 470) %>%
  verify(min(id) == 7700104067 & max(id) == 8850015622)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(unique(id) != FALSE)  %>%
  group_by(address_line) %>%
  slice(1) %>% 
  ungroup()

az_zips_freq_2020 <- as.data.frame(table(az_2020_df_full$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2020 = as.numeric(Freq))

# this suggests that there are more polling places in 2020 than 2016 but
# do 2016 data also account for maricopa?

# which zipcodes saw an increase, decrease, or maitenence in polling places?

n_places_az <- full_join(az_zips_freq_2020, az_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
         delta_cat = as.factor(if_else(delta_n_places < 0, 
                                       "Fewer Polling Places", 
                                       "The Same or More Polling Places"))) %>%
  arrange(delta_n_places)

# how many lost, gained, maintained?

az_table <- table(n_places_az$delta_n_places)

### south carolina ###

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
  group_by(address_line) %>%
  slice(1) %>% 
  ungroup()

sc_zips_freq_2016 <- as.data.frame(table(sc_2016_df$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2016 = as.numeric(Freq))

sc_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 5) %>%
  verify(ncol(.) == 3 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(address_line, nchar(address_line) - n_last + 1, 
                          nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  mutate(zipcode = as.factor(if_else(zipcode == "40 SC", "29840", zipcode))) %>%
  filter(zipcode != "SC") %>%
  group_by(address_line) %>%
  slice(1) %>% 
  ungroup()

sc_zips_freq_2020 <- as.data.frame(table(sc_2020_df$zipcode)) %>%
  mutate(zipcode = Var1, 
         n_pp_2020 = as.numeric(Freq))

# there are 815 different polling places in the 2020 dataset as compared to the
# 2016 dataset

no_overlap_sc <- anti_join(sc_2020_df, sc_2016_df, by = "address_line")

# which zipcodes saw an increase, decrease, or maitenence in polling places?

n_places_sc <- full_join(sc_zips_freq_2020, sc_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
         delta_cat = as.factor(if_else(delta_n_places < 0, 
                                       "Fewer Polling Places", 
                                       "The Same or More Polling Places"))) %>%
  filter(zipcode != "SC") %>%
  arrange(delta_n_places)

# how many lost, gained, maintained?

sc_table <- table(n_places_sc$delta_n_places)

# add in census race data and link to each state/year dataset grouped by zip
# for zips which lost polling places

lost_places_az <- n_places_az %>%
  filter(delta_cat == "Fewer Polling Places")

lost_places_sc <- n_places_sc %>%
  filter(delta_cat == "Fewer Polling Places")

az_demo_2016 <- pluck(read_rds(inputs$census_imp), 1) %>%
  filter(geoid %in% lost_places_az$zipcode) %>%
  group_by(geoid, variable)

az_demo_2018 <- pluck(read_rds(inputs$census_imp), 2) %>%
  filter(geoid %in% lost_places_az$zipcode) %>%
  group_by(geoid)

sc_demo_2016 <- pluck(read_rds(inputs$census_imp), 1) %>%
  filter(geoid %in% lost_places_sc$zipcode) %>%
  group_by(geoid)

sc_demo_2018 <- pluck(read_rds(inputs$census_imp), 2) %>%
  filter(geoid %in% lost_places_sc$zipcode) %>%
  group_by(geoid)

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