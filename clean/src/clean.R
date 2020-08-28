#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/clean/src/clean.R
#
pacman::p_load("tidyverse", "assertr", "janitor", "gtools")

inputs <- list(
  VIPinlist_imp = here::here("clean/input/VIPdata_imported.rds"),
  pa_2016_imp = here::here("clean/input/pa2016_imported.rds"),
  pa_2020_imp = here::here("clean/input/pa2020_imported.rds"),

  census_imp = here::here("clean/input/census_imported.rds"),

  az_covid_data = here::here("clean/input/covid_az_imported.rds"),
  sc_covid_data = here::here("clean/input/covid_sc_imported.rds"),
  covid_pa_imp = here::here("clean/input/covid_pa_imported.rds"),

  counnzip_azscpa_imp = here::here("clean/input/counzip_azscpa_imported.rds")
)

outputs <- list(
  az_2016_clean = here::here("write/input/az_2016_clean.rds"),
  az_2016_freq_clean = here::here("write/input/az_2016_freq_clean.rds"),
  az_2020_clean = here::here("write/input/az_2020_clean.rds"),
  az_2020_freq_clean = here::here("write/input/az_2020_freq_clean.rds"),
  az_demo_clean = here::here("write/input/az_demo_clean.rds"),
  az_demo_covid_clean = here::here("write/input/az_demo_covid_clean.rds"),

  sc_2016_clean = here::here("write/input/sc_2016_clean.rds"),
  sc_2016_freq_clean = here::here("write/input/sc_2016_freq_clean.rds"),
  sc_2020_clean = here::here("write/input/sc_2020_clean.rds"),
  sc_2020_freq_clean = here::here("write/input/sc_2020_freq_clean.rds"),
  sc_demo_clean = here::here("write/input/sc_demo_clean.rds"),
  sc_demo_covid_clean = here::here("write/input/sc_demo_covid_clean.rds"),

  pa_2016_clean = here::here("write/input/pa_2016_clean.rds"),
  pa_2016_freq_clean = here::here("write/input/pa_2016_freq_clean.rds"),
  pa_2020_clean = here::here("write/input/pa_2020_clean.rds"),
  pa_2020_freq_clean = here::here("write/input/pa_2020_freq_clean.rds"),
  pa_demo_clean = here::here("write/input/pa_demo_clean.rds"),
  pa_demo_covid_clean = here::here("write/input/pa_demo_covid_clean.rds")
)

# VIP data Arizon and South Carolina ##########################################
n_last <- 5
invalid_info <- c("4600331", "46002736", "4600617", "4600618",
                  "4610205126","46002463", "4600178", "46003124")

# nrows = unique polling places were open in 2016
az_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 1) %>%
  verify(ncol(.) == 3 & nrow(.) == 342) %>%
  verify(min(id) == 401333310112 & max(id) == 4610933) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(address_line,
                                       nchar(address_line) - n_last + 1,
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
         zipcode = if_else(id == "46002737", "85346", zipcode),
         zipcode = if_else(id == "460043" | id == "460034", "86001", zipcode),
         zipcode = if_else(id == "4600928", "85925", zipcode),
         address_line = make_clean_names(address_line, sep_out = " ",
                                         unique_sep = NULL),
         address_line = sub("_.*", "", address_line),
         name = make_clean_names(name, sep_out = " ",unique_sep = NULL),
         name = sub("_.*", "", name),
         id = if_else(id == "4600956", "4600977", id),
         id = if_else(id == "46003067", "4600205063", id),
         id = if_else(id == "46001808", "46001809", id),
         id = if_else(id == "4600183", "4600186", id),
         id = if_else(id == "46002737", "46002718", id)) %>%
  filter(address_line != "az") %>%
  filter(!id %in% invalid_info) %>%
  verify(is.na(zipcode) == FALSE) %>%
  distinct(id, .keep_all = TRUE) %>%
  verify(nrow(.) == 328 & ncol(.) == 4) %>%
  arrange(address_line) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(nrow(.) == 325 & ncol(.) == 4)

# nrows = number of unique zip codes in 2016
az_zips_freq_2016 <- as.data.frame(table(az_2016_df$zipcode)) %>%
  mutate(zipcode = Var1,
         n_pp_2016 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 195)

# AZ 2020 data
az_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 2) %>%
  verify(ncol(.) == 3 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1,
                                    nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(zipcode != "12345") %>%
  filter(unique(id) != FALSE) %>%
  verify(n_distinct(id) == 329) %>%
  verify(nrow(.) == 329 & ncol(.) == 4)

az_2020_maricopa_df <- pluck(read_rds(inputs$VIPinlist_imp), 3) %>%
  verify(ncol(.) == 3 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(address_line = tolower(address_line),
         name = tolower(name),
         zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1,
                                    nchar(address_line))),
         address_line = if_else(id == "8850012015",
                                "16402 n fort mcdowell rd fort mcdowell az 85264",
                                address_line)) %>%
  verify(is.na(zipcode) == FALSE) %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(n_distinct(id) == 151) %>%
  verify(nrow(.) == 151 & ncol(.) == 4)

# nrows = unique polling places were open in 2020
az_2020_df_full <- full_join(az_2020_df, az_2020_maricopa_df) %>%
  verify(ncol(.) == 4 & nrow(.) == 480) %>%
  verify(min(id) == 7700104067 & max(id) == 8850015622)  %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(address_line = tolower(address_line),
         name = tolower(name),
         zipcode = as.character(substr(address_line,
                                       nchar(address_line) - n_last + 1,
                                       nchar(address_line))),
         zipcode = if_else(id == "770039622" | id == "770039624",
                           "85941", zipcode),
         zipcode = if_else(id == "770039781", "86336", zipcode),
         zipcode = if_else(id == "770039481" | id == "770040157",
                           "85925", zipcode),
         zipcode = if_else(id == "770039899" | id == "770039783",
                           "86001", zipcode),
         address_line = tolower(address_line),
         name = tolower(name),
         address_line = if_else(id == "7700237255",
                                "10 s 6th st cottonwood az 86326", address_line),
         address_line = if_else(id == "770039550",
                                "yavapai county administration building",
                                address_line),
         id = if_else(id == "770039554", "7700237255", id),
         id = if_else(id == "770039639", "770040368", id),
         address_line = if_else(id == "770039620",
                                "11711 williams st wellton az 85356", address_line),
         address_line = if_else(id == "770040357",
                                 "1380 e patagonia hwy nogales az 85621",
                                 address_line),
         address_line = if_else(id == "770039624",
                                "v-10 road  az", address_line),
         id = if_else(id == "770040370" | id == "770040369",
                       "770039702", id),
         address_line = if_else(id == "770040018",
                                "1478 queen valley dr queen valley az 85118",
                                address_line),
         id = if_else(id == "770040394", "770040395", id),
         address_line = if_else(id == "8850012015",
                                "16402 n fort mcdowell rd fort mcdowell az 85264",
                                address_line),
         id = if_else(id == "770040123", "7700206366", id),
         address_line = if_else(id == "770039640",
                                "240 canal street somerton az 85350",
                                address_line),
         id = if_else(id == "770040396", "770040397", id),
         id = if_else(id == "770040077", "7700237271", id),
         id = if_else(id == "770039484", "770039485", id)) %>%
  verify(is.na(zipcode) == FALSE) %>%
  distinct(id, .keep_all = TRUE) %>%
  verify(n_distinct(id) == 471) %>%
  verify(nrow(.) == 471 & ncol(.) == 4) %>%
  arrange(address_line) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(nrow(.) == 470 & ncol(.) == 4)

# nrows = number of unique zip codes in 2020
az_zips_freq_2020 <- as.data.frame(table(az_2020_df_full$zipcode)) %>%
  mutate(zipcode = Var1,
         n_pp_2020 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 274) %>%
  verify(is.numeric(n_pp_2020) == TRUE)

# which zipcodes saw an increase, decrease, or maintenance in polling places?
n_places_az <- full_join(az_zips_freq_2020, az_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
         delta_cat = case_when(
           delta_n_places == 0 ~ "Maintained Polling Locations",
           delta_n_places < 0 ~ "Lost Polling Locations",
           delta_n_places > 0 ~ "Gained Polling Locations")) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 285)

## how many new polling locations are there in AZ between 2016 to 2020?

# looking for slight differences in names and addresses which would
# incorrectly miss duplicates, name seems to be a less consistent field
# than address so I'll use that

# 345/470 addresses in 2020 data not found in 2016 data
new_addsaz <- anti_join(az_2020_df_full, az_2016_df, by = "address_line")

# how many polling locations did maricopa have in 2016 vs 2020?

# clean maricopa 2020 data
mc2020 <- az_2020_maricopa_df %>%
  mutate(address_line = tolower(address_line),
         name = tolower(name)) %>%
  filter(id %in% az_2020_df_full$id) %>%
  filter(address_line %in% az_2020_df_full$address_line)
#151 polling places with unique ids and addresses

# how many new locations in az without maricopa?
az_san_mar2020 <- az_2020_df_full %>%
  filter(!id %in% mc2020$id)

# how many new locations in az without maricopa?
az_san_mar2016 <- az_2016_df %>%
  filter(!id %in% mc2020$id)

new_addsaz_sanmar <- anti_join(az_san_mar2020, az_san_mar2016, by = "address_line")
#221/319 locations appear only in 2020

# how many polling places persisted between 2016 and 2020?
mc_both <- semi_join(mc2020, az_2016_df, by = "address_line")
#27/151 locations open in maricopa in 2020 appear in both lists

# how many were only active in 2020/new in 2020?
mc_2020only <- anti_join(mc2020, az_2016_df, by = "address_line")
#124/151 locations open in maricopa in 2020 do not appear in the 2016 list

### south carolina ###
#nrows = unique polling places were open in 2016
sc_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 4) %>%
  verify(ncol(.) == 3 & nrow(.) == 2194) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(address_line,
                                       nchar(address_line) - n_last + 1,
                                       nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  mutate(
    address_line = tolower(address_line),
    name = tolower(name),
    zipcode = as.character(if_else(zipcode == "23222", "29532", zipcode)),
    zipcode = as.character(if_else(zipcode == "40 SC", "29840", zipcode)),
    zipcode = if_else(id == "88801220", "29410", zipcode),
    zipcode = if_else(id == "88802414", "29036", zipcode),
    zipcode = if_else(id == "8880132", "29302", zipcode),
    zipcode = if_else(id == "88801207", "29849", zipcode),
    address_line = if_else(id == "8880526",
                          "1 brams point rd hilton head island sc 29926",
                          address_line),
    address_line = if_else(id == "8880108",
                           "1001 longtown rd columbia sc 29229",
                           address_line),
    id = if_else(id == "8880802", "8880955", id),
    id = if_else(id == "88801932","88802078", id),
    address_line = if_else(id == "88802078",
                           "110 winter st winnsboro sc 29180",
                           address_line),
    id = if_else(id == "8880745","88801163", id),
    address_line = if_else(id == "88801163",
                           "1139 hillsboro rd orangeburg sc 29118",
                           address_line),
    address_line = if_else(id == "8880140",
                           "2001 spann rd batesburg sc 29006",
                           address_line),
    address_line = if_else(id == "8880203",
                           "201 school house ln summerville sc 29486",
                           address_line),
    address_line = if_else(id == "8880990",
                           "2345 state highway 41/51 s hemingway sc 29554",
                           address_line),
    id = if_else(id == "88801752","88801333", id),
    id = if_else(id == "88801582","8880924", id),
    id = if_else(id == "88801220","8880333", id)) %>%
  filter(zipcode != "SC") %>%
  distinct(id, .keep_all = TRUE) %>%
  verify(n_distinct(id) == 2173) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  arrange(address_line) %>%
  verify(nrow(.) == 2070 & ncol(.) == 4)

# nrows = number of unique zip codes in 2016
sc_zips_freq_2016 <- as.data.frame(table(sc_2016_df$zipcode)) %>%
  mutate(zipcode = Var1,
         n_pp_2016 = as.numeric(Freq)) %>%
  filter(zipcode != "SC") %>%
  verify(ncol(.) == 4 & nrow(.) == 377) %>%
  verify(zipcode != "SC")

## SC 2020 data #####################################

# summerville is now in zipcode 29483 not 29486
summerville <- c("88802641", "88802496", "88802274", "88801273",
                 "88801246", "88801201", "8880203")

#nrows = how many unique polling places were open in 2016? 1968
sc_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 5) %>%
  verify(ncol(.) == 3 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(address_line, nchar(address_line) - n_last + 1,
                                       nchar(address_line)))) %>%
  verify(is.na(zipcode) == FALSE) %>%
  mutate(address_line = tolower(address_line),
        name = tolower(name),
        zipcode = as.character(if_else(zipcode == "40 SC", "29840", zipcode)),
         zipcode = if_else(id == "88801220", "29410", zipcode),
         zipcode = if_else(id == "88802414", "29036", zipcode),
         zipcode = if_else(id == "8880132", "29302", zipcode),
         zipcode = if_else(id == "88801207", "29849", zipcode),
         zipcode = if_else(id %in% summerville, "29483", zipcode),
         id = if_else(id == "8880960", "88802013", id ),
         id = if_else(id == "88801932","88802078", id),
         address_line = if_else(id == "88802078",
                                "110 winter st winnsboro sc 29180",
                                address_line),
         id = if_else(id == "8880768","	88802203", id),
         id = if_else(id == "8880745","88801163", id),
         address_line = if_else(id == "88801163",
                                "1139 hillsboro rd orangeburg sc 29118",
                                address_line),
         id = if_else(id == "88801440","88802442", id),
         id = if_else(id == "8880189","88801108", id),
         address_line = if_else(id == "88801693",
                                "13255 ashevelle hwy inman sc 29349",
                                address_line),
         id = if_else(id == "88801893","88802149", id),
         address_line = if_else(id == "88801067",
                                "200 powe st cherawd sc 29520",
                                address_line),
         id = if_else(id == "88801434","88802218", id),
         id = if_else(id == "88802030","88801168", id),
         id = if_else(id == "88802100","88801710", id),
         id = if_else(id == "8880672","88801930", id)) %>%
  filter(zipcode != "SC") %>%
  distinct(id, .keep_all = TRUE) %>%
  verify(n_distinct(id) == 1958) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(n_distinct(address_line) == 1920) %>%
  arrange(address_line) %>%
  verify(nrow(.) == 1920 & ncol(.) == 4)

# nrows = number of unique zip codes in 2020 = 383
sc_zips_freq_2020 <- as.data.frame(table(sc_2020_df$zipcode)) %>%
  mutate(zipcode = Var1,
         n_pp_2020 = as.numeric(Freq)) %>%
  filter(zipcode != "SC") %>%
  verify(ncol(.) == 4 & nrow(.) == 378) %>%
  verify(zipcode != "SC")

# which zip codes saw an increase, decrease, or maintenance in polling places?
n_places_sc <- full_join(sc_zips_freq_2020, sc_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
         delta_cat = case_when(
           delta_n_places == 0 ~ "Maintained Polling Locations",
           delta_n_places < 0 ~ "Lost Polling Locations",
           delta_n_places > 0 ~ "Gained Polling Locations")) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 381)

# how many addresses appear only in 2020 data and not 2016 data?

# 807/1920 polling places are new in 2020
new_addssc <- anti_join(sc_2020_df, sc_2016_df, by = "address_line",
                       suffix = c("2020", "2016"))

## PA 2016 data###################################################
pa_2016_df <- read_rds(inputs$pa_2016) %>%
  clean_names() %>%
  mutate(
    street = tolower(if_else(street == "OLD ROUTE  30", "OLD ROUTE 30", street)),
    name = make_clean_names(description, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    comment = tolower(comment),
    prefix_direction_desc = tolower(prefix_direction_desc),
    street_type_desc = tolower(street_type_desc),
    city = tolower(city),
    line2 = tolower(line2),
    zipcode = as.character(postal_code),
    county = as.character(snakecase::to_any_case(county_name, case = "big_camel"))) %>%
  select(county, name, house_num, street, street_type_desc, city, zipcode) %>%
  unite("address_line", house_num:zipcode, remove = FALSE, sep = " ") %>%
  mutate(address_line = make_clean_names(address_line, sep_out = " ",
                                         unique_sep = NULL),
         address_line = sub("_.*", "", address_line)) %>%
  filter(zipcode > 13235 & zipcode != 191) %>%
  select(county, name, address_line, zipcode) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(n_distinct(address_line) == 5756) %>%
  arrange(address_line) %>%
  verify(nrow(.) == 5756 & ncol(.) == 4)

# 1325 locations with no zip code, will have to either discard or only do county
# level analysis
no_zips_pa <- pa_2016_df %>%
  filter(is.na(zipcode) == TRUE)

# nrows = number of unique zip codes in 2016
pa_zips_freq_2016 <- as.data.frame(table(pa_2016_df$zipcode)) %>%
  mutate(zipcode = Var1,
         n_pp_2016 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 1488)

# PA 2020 data ##########
pa_2020_df <- read_rds(inputs$pa_2020) %>%
  clean_names() %>%
  mutate(
    street = tolower(if_else(street == "OLD ROUTE  30", "OLD ROUTE 30", street)),
    name = make_clean_names(description, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    comment = tolower(comment),
    prefix_direction = tolower(prefix_direction),
    street_type = tolower(street_type),
    city = tolower(city),
    line2 = tolower(line2),
    zipcode = as.character(postal_code),
    county = as.character(snakecase::to_any_case(county_name, case = "big_camel"))) %>%
    select(county, name, house_num, street, street_type, city, zipcode) %>%
    unite("address_line", house_num:zipcode, remove = FALSE, sep = " ") %>%
  mutate(address_line = make_clean_names(address_line, sep_out = " ",
                                         unique_sep = NULL),
         address_line = sub("_.*", "", address_line)) %>%
    filter(zipcode > 13235 & zipcode != 191) %>%
    select(county, name, address_line, zipcode) %>%
    distinct(address_line, .keep_all = TRUE) %>%
    verify(n_distinct(address_line) == 5259) %>%
    arrange(address_line) %>%
    verify(nrow(.) == 5280 & ncol(.) == 4)

# nrows = number of unique zip codes in 2020 = 1514
pa_zips_freq_2020 <- as.data.frame(table(pa_2020_df$zipcode)) %>%
  mutate(zipcode = Var1,
         n_pp_2020 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 1516)

# which zip codes saw an increase, decrease, or maintenance in polling places?
n_places_pa <- full_join(pa_zips_freq_2020, pa_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
         delta_cat = case_when(
           delta_n_places == 0 ~ "Maintained Polling Locations",
           delta_n_places < 0 ~ "Lost Polling Locations",
           delta_n_places > 0 ~ "Gained Polling Locations")) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 1592)

# how many addresses appear only in 2020 data and not 2016 data?

# 1926/1516 polling places are new in 2020
unique_places <- full_join(pa_2020_df, pa_2016_df, by = "address_line",
                        suffix = c("2020", "2016"))

uniques2020 <- all_addspa %>%
  distinct(name2020, county2020, zipcode2020, .keep_all = TRUE)

new_addspa <- anti_join(pa_2020_df, pa_2016_df, by = "address_line",
                        suffix = c("2020", "2016"))










##########################################################################
# add in county information
az_cos <- read_rds(inputs$az_sc_counzips) %>%
  filter(state == "AZ") %>%
  mutate(zip = as.character(zip)) %>%
  filter(zip %in% n_places_az$zipcode) %>%
  verify(ncol(.) == 3 & nrow(.) == 284)

sc_cos <- read_rds(inputs$az_sc_counzips) %>%
  filter(state == "SC") %>%
  mutate(zip = as.character(zip)) %>%
  filter(zip %in% n_places_sc$zipcode) %>%
  mutate(county = gsub(' [A-z ]*', '' , county)) %>%
  verify(ncol(.) == 3 & nrow(.) == 380)

# add in census race data and link to each state/year data set grouped by zip
az_demo <- read_rds(inputs$census_imp) %>%
  select(geoid, variable, estimate) %>%
  filter(geoid %in% n_places_az$zipcode) %>%
  spread(key = "variable", value = "estimate") %>%
  verify(sum(total) == sum(total_nhl) + sum(total_hl)) %>%
  mutate(nhl_ao = as.numeric(nhl_sor + nhl_tom),
         pct_hl = as.numeric((total_hl/total)*100),
         pct_nhl = as.numeric((total_nhl/total)*100),
         pct_nhl_white = as.numeric((nhl_white/total)*100),
         pct_nhl_black = as.numeric((nhl_black/total)*100),
         pct_nhl_ai_an = as.numeric((nhl_ai_an/total)*100),
         pct_nhl_asian = as.numeric((nhl_asian/total)*100),
         pct_nhl_nhi_pi = as.numeric((nhl_nhi_pi/total)*100),
         pct_nhl_ao = as.numeric((nhl_ao/total)*100),
         gp_hl = case_when(
           total_hl > 50001 & total_hl <= 60000 ~ "50,001-60,000",
         total_hl > 40001 & total_hl <= 50000 ~ "40,001-50,000",
         total_hl > 30001 & total_hl <= 40000 ~ "30,001-40,000",
         total_hl > 20001 & total_hl <= 30000 ~ "20,001-30,000",
         total_hl > 10001 & total_hl <= 20000 ~ "10,001-20,000",
         total_hl <= 10000 ~ "0-10,000"),
         gp_white = case_when(
           nhl_white > 50001 & nhl_white <= 60000 ~ "50,001-60,000",
           nhl_white > 40001 & nhl_white <= 50000 ~ "40,001-50,000",
           nhl_white > 30001 & nhl_white <= 40000 ~ "30,001-40,000",
           nhl_white > 20001 & nhl_white <= 30000 ~ "20,001-30,000",
           nhl_white > 10001 & nhl_white <= 20000 ~ "10,001-20,000",
           nhl_white <= 10000 ~ "0-10,000"),
         gp_black = case_when(
           nhl_black > 50001 & nhl_black <= 60000 ~ "50,001-60,000",
           nhl_black > 40001 & nhl_black <= 50000 ~ "40,001-50,000",
           nhl_black > 30001 & nhl_black <= 40000 ~ "30,001-40,000",
           nhl_black > 20001 & nhl_black <= 30000 ~ "20,001-30,000",
           nhl_black > 10001 & nhl_black <= 20000 ~ "10,001-20,000",
           nhl_black <= 10000 ~ "0-10,000"),
         gp_ai_an = case_when(
           nhl_ai_an > 50001 & nhl_ai_an <= 60000 ~ "50,001-60,000",
           nhl_ai_an > 40001 & nhl_ai_an <= 50000 ~ "40,001-50,000",
           nhl_ai_an > 30001 & nhl_ai_an <= 40000 ~ "30,001-40,000",
           nhl_ai_an > 20001 & nhl_ai_an <= 30000 ~ "20,001-30,000",
           nhl_ai_an > 10001 & nhl_ai_an <= 20000 ~ "10,001-20,000",
           nhl_ai_an <= 10000 ~ "0-10,000"),
         qao = quantcut(nhl_ao),
         qhl = quantcut(total_hl),
         qnhl = quantcut(total_nhl),
         qwhite = quantcut(nhl_white),
         qblack = quantcut(nhl_black),
         qai_an = quantcut(nhl_ai_an),
         qasian = quantcut(nhl_asian),
         qnhi_pi = quantcut(nhl_nhi_pi),
         qtotal = quantcut(total)) %>%
  group_by(geoid) %>%
  verify(sum(nhl_ao) == sum(nhl_sor + nhl_tom)) %>%
  verify(is.na(pct_nhl_ao) == FALSE) %>%
  full_join(n_places_az, by = c("geoid" = "zipcode")) %>%
  full_join(az_cos, by = c("geoid" = "zip")) %>%
  mutate(county = if_else(is.na(county) == TRUE, "Missing County", county)) %>%
  filter(county != "Missing County") %>%
  mutate(county = if_else(county == "McKinley County", "Apache County", county),
         county = if_else(county == "San Juan County", "Coconino County", county)) %>%
  verify(ncol(.) == 39 & nrow(.) == 284) %>%
  verify(county != "McKinley County" | county != "San Juan County" |
           county != "Missing County")

sc_demo <- pluck(read_rds(inputs$census_imp)) %>%
  select(geoid, variable, estimate) %>%
  filter(geoid %in% n_places_sc$zipcode) %>%
  spread(key = "variable", value = "estimate") %>%
  verify(sum(total) == sum(total_nhl) + sum(total_hl)) %>%
  mutate(nhl_ao = as.numeric(nhl_sor + nhl_tom),
         pct_hl = as.numeric((total_hl/total)*100),
         pct_nhl = as.numeric((total_nhl/total)*100),
         pct_nhl_white = as.numeric((nhl_white/total)*100),
         pct_nhl_black = as.numeric((nhl_black/total)*100),
         pct_nhl_ai_an = as.numeric((nhl_ai_an/total)*100),
         pct_nhl_asian = as.numeric((nhl_asian/total)*100),
         pct_nhl_nhi_pi = as.numeric((nhl_nhi_pi/total)*100),
         pct_nhl_ao = as.numeric((nhl_ao/total)*100),
         gp_hl = case_when(
           total_hl > 50001 & total_hl <= 60000 ~ "50,001-60,000",
           total_hl > 40001 & total_hl <= 50000 ~ "40,001-50,000",
           total_hl > 30001 & total_hl <= 40000 ~ "30,001-40,000",
           total_hl > 20001 & total_hl <= 30000 ~ "20,001-30,000",
           total_hl > 10001 & total_hl <= 20000 ~ "10,001-20,000",
           total_hl <= 10000 ~ "0-10,000"),
         gp_white = case_when(
           nhl_white > 50001 & nhl_white <= 60000 ~ "50,001-60,000",
           nhl_white > 40001 & nhl_white <= 50000 ~ "40,001-50,000",
           nhl_white > 30001 & nhl_white <= 40000 ~ "30,001-40,000",
           nhl_white > 20001 & nhl_white <= 30000 ~ "20,001-30,000",
           nhl_white > 10001 & nhl_white <= 20000 ~ "10,001-20,000",
           nhl_white <= 10000 ~ "0-10,000"),
         gp_black = case_when(
           nhl_black > 50001 & nhl_black <= 60000 ~ "50,001-60,000",
           nhl_black > 40001 & nhl_black <= 50000 ~ "40,001-50,000",
           nhl_black > 30001 & nhl_black <= 40000 ~ "30,001-40,000",
           nhl_black > 20001 & nhl_black <= 30000 ~ "20,001-30,000",
           nhl_black > 10001 & nhl_black <= 20000 ~ "10,001-20,000",
           nhl_black <= 10000 ~ "0-10,000"),
         qao = quantcut(nhl_ao),
         qhl = quantcut(total_hl),
         qnhl = quantcut(total_nhl),
         qwhite = quantcut(nhl_white),
         qblack = quantcut(nhl_black),
         qai_an = quantcut(nhl_ai_an),
         qasian = quantcut(nhl_asian),
         qnhi_pi = quantcut(nhl_nhi_pi),
         qtotal = quantcut(total)) %>%
  group_by(geoid) %>%
  verify(sum(nhl_ao) == sum(nhl_sor + nhl_tom)) %>%
  verify(is.na(pct_nhl_ao) == FALSE) %>%
  full_join(n_places_sc, by = c("geoid" = "zipcode")) %>%
  full_join(sc_cos, by = c("geoid" = "zip")) %>%
  mutate(county = if_else(is.na(county) == TRUE, "Missing County", county)) %>%
  filter(county != "Missing County") %>%
  verify(ncol(.) == 38 & nrow(.) == 380) %>%
  verify(county != "Missing County")

# export all objects for write task here so as not to have null objects
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

# combine covid data and demographic/polling data
az_covid_demo_df <- read_rds(inputs$az_covid_data) %>%
  mutate(zipcode = gsub(' [A-z ]*', " ", zipcode)) %>%
  full_join(az_demo, by = c("zipcode" = "geoid")) %>%
  mutate(
    polling_cat = as.character(delta_cat),
    covid_cat = case_when(
      is.na(total) == TRUE & is.na(polling_cat) == TRUE ~ "Missing Polling Data",
      is.na(total) == FALSE & is.na(polling_cat) == FALSE &
        confirmed_case_count == 0  ~ "COVID Data Suppressed Pending Tribal Approval",
      is.na(total) == FALSE & is.na(polling_cat) == FALSE &
        confirmed_case_count != 0 ~ polling_cat,
      is.na(total) == FALSE & is.na(polling_cat) == FALSE &
        confirmed_case_count == 0  ~ "COVID Data Suppressed Pending Tribal Approval",
      is.na(confirmed_case_count) == TRUE & is.na(polling_cat) == FALSE
      ~ "No COVID Data Reported")) %>%
  filter(covid_cat != "Missing Polling Data") %>%
  verify(covid_cat != "Missing Polling Data") %>%
  verify(ncol(.) == 43 & nrow(.) == 284) %>%
  saveRDS(outputs$az_demo_covid_clean)

az_demo <- az_demo %>%
  saveRDS(outputs$az_demo_clean)

# south carolina covid data, by county, join with zip code and demo data
sc_covid_demo_df <- read_rds(inputs$sc_covid_data) %>%
  select(c(county, cases)) %>%
  group_by(county, cases) %>%
  summarize_if(is.numeric, list(count = sum)) %>%
  summarize(cases = sum(cases)) %>%
  filter(county != "Unknown") %>%
  full_join(sc_cos, by = c("county" = "county")) %>%
  select(-c(state)) %>%
  full_join(sc_demo, by = c("zip" = "geoid")) %>%
  rename(county = county.y,
         zipcode = zip) %>%
  select(-c(county.x, state)) %>%
  verify(ncol(.) == 38 & nrow(.) == 380) %>%
  saveRDS(outputs$sc_demo_covid_clean)

sc_demo <- sc_demo %>%
  saveRDS(outputs$sc_demo_clean)

# done.
