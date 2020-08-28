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
         id = if_else(id == "4600956", "4600977", id),
         id = if_else(id == "46003067", "4600205063", id),
         id = if_else(id == "46001808", "46001809", id),
         id = if_else(id == "4600183", "4600186", id),
         id = if_else(id == "46002737", "46002718", id),
         address_line = if_else(id == "46001811",
                                "cornfields chapter house ganado az 86505",
                                address_line),
         address_line = if_else(id == "46001812",
                                "cottonwood chapter house chinle az 86503",
                                address_line),
         address_line = if_else(id == "46001206",
                                "black mesa chapter house black mesa az 86033",
                                address_line),
         address_line = if_else(id == "46001841",
                                "st. michaels chapter house st. michaels az 85611",
                                address_line),
         address_line = if_else(id == "46001828",
                                "mexican water chapter mexican water az 86514",
                                address_line),
         address_line = if_else(id == "46001822",
                                "kinlichee chapter house st michaels az 86511",
                                address_line),
         address_line = if_else(id == "46001213",
                                "forest lake chapter house forest lake az 86033",
                                address_line),
         address_line = if_else(id == "46003069",
                                "federal route 15 & saint  augustine street sells az 85122",
                                address_line),
         address_line = if_else(id == "46003018",
                                "962 w gila bend highway casa grande az 85122",
                                address_line),
         address_line = if_else(id == "46003017",
                                "8470 n overfield rd  coolidge az 85128",
                                address_line),
         address_line = if_else(id == "46002483",
                                "824 thorn avenue winkelman az 85192",
                                address_line),
         address_line = if_else(id == "46003056",
                                "5782 s mountainbrook dr gold canyon az 85118",
                                address_line),
         address_line = if_else(id == "46003047",
                                "575 n idaho rd apache junction az 85119",
                                address_line),
         address_line = if_else(id == "46003011",
                                "550 s ironwood drive apache junction az 85120",
                                address_line),
         address_line = if_else(id == "46003324",
                                "50 bridge road tubac az 85646",
                                address_line),
         address_line = if_else(id == "46002412",
                                "4621 south 9th street canyon day az 85491",
                                address_line),
         address_line = if_else(id == "46003096",
                                "39315 n cortona dr san tan valley az 85140",
                                address_line),
         address_line = if_else(id == "46003078",
                                "3650 w shedd road eloy az 85131",
                                address_line),
         address_line = if_else(id == "46003082",
                                "3496 w casa blanca rd bapchule az 85121",
                                address_line),
         address_line = if_else(id == "46003314",
                                "346 duquesne ave. patagonia az 85624",
                                address_line),
         address_line = if_else(id == "4600205063",
                                "33622 n mountain vista blvd san tan valley az 85142",
                                address_line),
         address_line = if_else(id == "46003039",
                                "28479 n main street san tan valley az 85143",
                                address_line),
         address_line = if_else(id == "40133334004",
                                "222 e javelina ave mesa az 85210",
                                address_line),
         address_line = if_else(id == "4600911",
                                "1730 kino ave kingman az 86409", address_line),
         address_line = if_else(id == "46003071",
                                "1084 w san tan hills dr san tan valley az 85143",
                                address_line),
         address_line = if_else(id == "46003049",
                                "100 sunset drive superior az 85173", address_line),
         address_line = make_clean_names(address_line, sep_out = " ",
                                         unique_sep = NULL),
         address_line = sub("_.*", "", address_line),
         name = make_clean_names(name, sep_out = " ",unique_sep = NULL),
         name = sub("_.*", "", name),
         address_line = as.character(gsub(" s ", " south ", address_line)),
         address_line = as.character(gsub(" n ", " north ", address_line)),
         address_line = as.character(gsub(" w ", " west ", address_line)),
         address_line = as.character(gsub(" e ", " east ", address_line))) %>%
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
  mutate(address_line = make_clean_names(address_line, sep_out = " ",
                                                  unique_sep = NULL),
         address_line = sub("_.*", "", address_line),
         name = make_clean_names(name, sep_out = " ",unique_sep = NULL),
         name = sub("_.*", "", name),
         zipcode = as.factor(substr(address_line, nchar(address_line) - n_last + 1,
                                    nchar(address_line)))) %>%
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
         id = if_else(id == "770039554", "7700237255", id),
         id = if_else(id == "770039639", "770040368", id),
         id = if_else(id == "770040370" | id == "770040369","770039702", id),
         id = if_else(id == "770040394", "770040395", id),
         id = if_else(id == "770040123", "7700206366", id),
         id = if_else(id == "770040396", "770040397", id),
         id = if_else(id == "770040077", "7700237271", id),
         id = if_else(id == "770039484", "770039485", id),
         address_line = if_else(id == "7700237255",
                                "10 s 6th st cottonwood az 86326", address_line),
         address_line = if_else(id == "770039550",
                                "yavapai county administration building",
                                address_line),
         address_line = if_else(id == "770039620",
                                "11711 williams st wellton az 85356", address_line),
         address_line = if_else(id == "770040357",
                                "1380 e patagonia hwy nogales az 85621",
                                address_line),
         address_line = if_else(id == "770039624",
                                "v-10 road  az", address_line),
         address_line = if_else(id == "770040018",
                                "1478 queen valley dr queen valley az 85118",
                                address_line),
         address_line = if_else(id == "8850012015",
                                "16402 n fort mcdowell rd fort mcdowell az 85264",
                                address_line),
         address_line = if_else(id == "770039640",
                                "240 canal street somerton az 85350",
                                address_line),
         address_line = make_clean_names(address_line, sep_out = " ",
                                         unique_sep = NULL),
         address_line = sub("_.*", "", address_line),
         name = make_clean_names(name, sep_out = " ",unique_sep = NULL),
         name = sub("_.*", "", name),
         address_line = as.character(gsub(" s ", " south ", address_line)),
         address_line = as.character(gsub(" n ", " north ", address_line)),
         address_line = as.character(gsub(" w ", " west ", address_line)),
         address_line = as.character(gsub(" e ", " east ", address_line))) %>%
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

# 353/470 addresses in 2020 data not found in 2016 data
new_addsaz <- anti_join(az_2020_df_full, az_2016_df, by = "address_line")

# how many polling locations did maricopa have in 2016 vs 2020?

# clean maricopa 2020 data
mc2020 <- az_2020_maricopa_df %>%
  mutate(address_line = tolower(address_line),
         name = tolower(name)) %>%
  filter(id %in% az_2020_df_full$id) %>%
  filter(address_line %in% az_2020_df_full$address_line)
#2 polling places with unique ids and addresses

# how many new locations in az without maricopa?
az_san_mar2020 <- az_2020_df_full %>%
  filter(!id %in% mc2020$id)
#468

# how many new locations in az without maricopa?
az_san_mar2016 <- az_2016_df %>%
  filter(!id %in% mc2020$id)
#325

new_addsaz_sanmar <- anti_join(az_san_mar2020, az_san_mar2016, by = "address_line")
#229/320 locations appear only in 2020

# how many polling places persisted between 2016 and 2020?
mc_both <- semi_join(mc2020, az_2016_df, by = "address_line")
#26/150 locations open in maricopa in 2020 appear in both lists

# how many were only active in 2020/new in 2020?
mc_2020only <- anti_join(mc2020, az_2016_df, by = "address_line")
#124/150 locations open in maricopa in 2020 do not appear in the 2016 list

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
    id = if_else(id == "88801220","8880333", id),
    address_line = as.character(gsub(" s ", " south ", address_line)),
    address_line = as.character(gsub(" n ", " north ", address_line)),
    address_line = as.character(gsub(" w ", " west ", address_line)),
    address_line = as.character(gsub(" e ", " east ", address_line))) %>%
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
         id = if_else(id == "8880672","88801930", id),
        address_line = as.character(gsub(" s ", " south ", address_line)),
        address_line = as.character(gsub(" n ", " north ", address_line)),
        address_line = as.character(gsub(" w ", " west ", address_line)),
        address_line = as.character(gsub(" e ", " east ", address_line))) %>%
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

# 805/1920 polling places are new in 2020
new_addssc <- anti_join(sc_2020_df, sc_2016_df, by = "address_line",
                       suffix = c("2020", "2016"))

## PA 2016 data###################################################
# started with 9160 rows and so far at 5718 unique, accurate addresses but still
# working on it
pa_2016_df <- read_rds(inputs$pa_2016) %>%
  clean_names() %>%
  mutate(
    street = tolower(if_else(street == "OLD ROUTE  30", "OLD ROUTE 30", street)),
    name = as.character(make_clean_names(description, sep_out = " ", unique_sep = NULL)),
    name = as.character(sub("_.*", "", name)),
    comment = tolower(comment),
    prefix_direction_desc = tolower(prefix_direction_desc),
    street_type_desc = tolower(street_type_desc),
    city = tolower(city),
    line2 = tolower(line2),
    zipcode = as.character(postal_code),
    county = as.character(snakecase::to_any_case(county_name, case = "big_camel"))) %>%
  select(county, name, house_num, street, street_type_desc, city, zipcode) %>%
  unite("address_line", house_num:zipcode, remove = FALSE, sep = " ") %>%
  mutate(address_line = as.character(make_clean_names(address_line, sep_out = " ",
                                         unique_sep = NULL)),
         address_line = as.character(sub("_.*", "", address_line)),
         address_line = as.character(gsub(" na ", " ", address_line)),
         address_line = as.character(gsub("na ", "", address_line)),
         name = as.character(gsub(" na ", " ", name)),
         address_line = as.character(gsub(" street ", " st ", address_line)),
         address_line = as.character(gsub(" avenue ", " ave ", address_line)),
         address_line = as.character(gsub(" road ", " rd ", address_line)),
         address_line = as.character(gsub(" drive ", " dr ", address_line)),
         address_line = as.character(gsub(" south ", " s ", address_line)),
         address_line = as.character(gsub(" north ", " n ", address_line)),
         address_line = as.character(gsub(" west ", " w ", address_line)),
         address_line = as.character(gsub(" east ", " e ", address_line)),
         address_line = as.character(gsub(" first ", " 1st ", address_line)),
         address_line = as.character(gsub(" second ", " 2nd ", address_line)),
         address_line = as.character(gsub(" third ", " 3rd ", address_line)),
         address_line = as.character(gsub(" fourth ", " 4th ", address_line)),
         address_line = as.character(gsub(" fifth ", " 5th ", address_line)),
         address_line = as.character(gsub(" sixth ", " 6th ", address_line)),
         address_line = as.character(gsub(" seventh ", " 7th ", address_line)),
         address_line = as.character(gsub(" eigth ", " 8th ", address_line)),
         address_line = as.character(gsub(" ninth ", " 9th ", address_line)),
         address_line = as.character(gsub(" tenth ", " 10th ", address_line)),
         address_line = as.character(gsub(" 01st ", " 1st ", address_line)),
         address_line = as.character(gsub(" 02nd ", " 2nd ", address_line)),
         address_line = as.character(gsub(" 03rd ", " 3rd ", address_line)),
         address_line = as.character(gsub(" 04th ", " 4th ", address_line)),
         address_line = as.character(gsub(" 05th ", " 5th ", address_line)),
         address_line = as.character(gsub(" 06th ", " 6th ", address_line)),
         address_line = as.character(gsub(" 07th ", " 7th ", address_line)),
         address_line = as.character(gsub(" 08th ", " 8th ", address_line)),
         address_line = as.character(gsub(" 09th ", " 9th ", address_line)),
         address_line = if_else(name == "joseph a hardy connellsville airport terminal bldg",
                                "x988 sky drive lemont furnace 15456",
                                address_line),
         address_line = if_else(name == "carroll vol fd carroll vol fire hall eldora",
                                "x988 route 837 monongahela 15063",
                                address_line),
         address_line = if_else(name == "derry area school admin building",
                                "x982 chestnut st derry 15627",address_line),
         address_line = if_else(name == "blairsville vfw post 5821",
                                "x9626 rte 22 w blairsville 15717",address_line),
         address_line = if_else(name == "blairsville vfw post 5821",
                                "x9607 rt 75 s east waterford 17021", address_line),
         address_line = if_else(name == "webster vol fire department",
                                "x956 rte 906 webster 15087",address_line),
         name = if_else(name == "st marks ev ch miesel hall",
                        "st marks evan church",name),
         address_line = if_else(address_line ==
                                  "x933 briookline blvd pittsburgh 15226",
                                "x933 brookline blvd pittsburgh 15226",
                                address_line),
         address_line = if_else(address_line == "x915 linc ave west chester 19380",
                                "x915 lincoln ave west chester 19380",
                                address_line),
         name = if_else(name == "w mifflin hi sch aud lobby ent",
                        "west mifflin hi sch aud lobby ent",name),
         address_line = if_else(address_line == "x91 commonwealth dr west mifflin 15122",
                                "x91 commonwealth ave west mifflin 15122",
                                address_line),
         name = if_else(name == "liberty vfd","liberty volunteer fire department",
                        name),
         address_line = if_else(address_line == "x900 haslage ave liberty 15133",
                                "x900 haslage st mckeesport 15133",address_line),
         name = if_else(name == "mckinley park rec center","mckinley park rec ctr gym",
                        name),
         address_line = if_else(address_line == "x900 desdemont ave pittsburgh 15210",
                                "x900 desmont ave pittsburgh 15210",
                                address_line),
         address_line = if_else(address_line == "x889 milledgeville rd sandy lake 16145",
                                "x889 milledgeville rd hadley 16130",
                                address_line),
         address_line = if_else(address_line == "x8800 peebles rd pittsburgh 15137",
                                "x8800 peebles rd allison park 15101",
                                address_line),
         address_line = if_else(address_line == "x860 colonial manor large bldg rd n huntingdon 15642",
                                "x860 colonial manor rd n huntingdon 15642",
                                address_line),
         address_line = if_else(address_line == "x850 cranberry woods rd sewickley 15066",
                                "x850 cranberry woods rd warrendale 16066",
                                address_line),
         address_line = if_else(address_line == "x8711 old perry hwy pittsburgh 15237",
                                "x8711 old perry hwy way mccandless 15237",
                                address_line),
         address_line = if_else(address_line == "x800 coopertown rd bryn mawr 19010",
                                "x800 coopertown rd haverford 19041",address_line),
         address_line = if_else(address_line == "x778 rt 66 apollo 15613",
                                "x778 rte 66 apollo 15613",address_line),
         address_line = if_else(address_line == "x767 5th st oakmont 15139",
                                "x767 fifth st oakmont 15139",address_line),
         address_line = if_else(address_line == "x7605 saltsburg st pittsburgh 15239",
                                "x7605 saltsburg rd pittsburgh 15239",address_line),
         address_line = if_else(address_line == "x751 sugar run rd altoona 16602",
                                "x751 sugar run rd altoona 16601",address_line),
         address_line = if_else(address_line == "x718 wallace ave wilkinsburg 15221",
                                "x718 wallace ave pittsburgh 15221",address_line),
         address_line = if_else(address_line == "x700 york mitchell ave lansdale 19446",
                                "x700 york ave lansdale 19446",address_line),
         address_line = if_else(address_line == "x695 freeeport rd freeport 16229",
                                "x695 freeport rd freeport 16229",address_line),
         address_line = if_else(address_line == "x660 noble rd west mifflin 15122",
                                "x660 noble dr west mifflin 15122", address_line),
         address_line = if_else(address_line == "x6051 west chester park newtown
                                square 19073","x6051 west chester pike newtown
                                square 19073", address_line),
         address_line = if_else(address_line == "x602 ingomar rd sewickley 15090",
                                "x602 ingomar rd pittsburgh 15237",address_line),
         address_line = if_else(address_line == "x60 ganaldo rd pittsburgh 15108",
                                "x60 gawaldo dr coraopolis 15108",address_line),
         address_line = if_else(address_line == "x5901 library hgts rd bethel park 15102",
                                "x5901 library rd bethel park 15102",address_line),
         address_line = if_else(address_line == "x555 broadway ave stowe 15136",
                                "x555 broadway ave mckees rocks 15136",address_line),
         address_line = if_else(address_line == "x551 ravensburg blvd pittsburgh 15025",
                                "x551 ravensburg blvd clairton 15025",address_line),
         address_line = if_else(address_line == "x539 chicora st e mckeesport 15035",
                                "x539 chicora st east mc keesport 15035",address_line),
         address_line = if_else(address_line == "x537 bayne ave bellevue 15202",
                                "x537 bayne ave pittsburgh 15202",address_line),
         address_line = if_else(address_line == "x525 pleasant hill rd marshall 15090",
                                "x525 pleasant hill rd wexford 15090",address_line),
         address_line = if_else(address_line == "x519 58th st altoona 16601",
                                "x519 58th st altoona 16602",address_line),
         address_line = if_else(address_line == "x5145 wexford run rd marshall 15090",
                                "x5145 wexford run rd wexford 15090",address_line),
         address_line = if_else(address_line == "x503 speer st n belle vern 15012",
                                "x503 speer st belle vernon 15012",address_line),
         address_line = if_else(address_line == "x480 ridge rd pottstown 19465",
                                "x480 ridge rd spring city 19475",address_line),
         address_line = if_else(address_line == "x48 factory ln e smithfield 18817",
                                "x48 factory ln east smithfield 18817",address_line),
         address_line = if_else(address_line == "x4600 old wm penn hiway murrysville 15668",
                                "x4600 old wm penn hwy murrysville 15668",address_line),
         address_line = if_else(address_line == "x4565 prestwick dr reading 19601",
                                "x4565 prestwick dr reading 19606",address_line),
         address_line = if_else(address_line == "x456 first st heidelberg 15106",
                                "x456 first st carnegie 15106",address_line),
         address_line = if_else(address_line == "x423 allegheny st h0llidaysburg 16648",
                                "x423 allegheny st hollidaysburg 16648",address_line),
         address_line = if_else(address_line == "x417 lincoln ave walnutport 18035",
                                "x417 lincoln ave walnutport 18088",address_line),
         address_line = if_else(address_line == "x415 three sq hollow rd newburg 17240",
                                "x415 three square hollow rd newburg 17240",address_line),
         address_line = if_else(address_line == "x415 brennan ave loyalhanna 15661",
                                "x415 brennan ave latrobe 15650",address_line),
         address_line = if_else(address_line == "x414 railroad st pittsburgh 15028" |
                                  address_line == "x414 railroad st coulters 15028",
                                "x414 railroad st white oak 15131",address_line),
         address_line = if_else(address_line == "x411 babylon rd horsham 19002",
                                "x411 babylon rd horsham 19044",address_line),
         address_line = if_else(address_line == "x409 ashland ave primos 19018",
                                "x409 ashland ave secane 19018",address_line),
         address_line = if_else(address_line == "x403 fox chapel rd fox chapel 15238",
                                "x403 fox chapel rd pittsburgh 15238",address_line),
         address_line = if_else(address_line == "x4021 parkside ave philadelphia 19014",
                                "x4021 parkside ave philadelphia 19104",address_line),
         address_line = if_else(address_line == "x401 washington rd mt lebanon 15228",
                                "x401 washington rd pittsburgh 15228",address_line),
         name = if_else(name == "springfield country club temporary",
                        "springfield country club",name),
         address_line = if_else(address_line == "x400 sproul rd springfield 19064",
                                "x400 sproul rd springield 19064",address_line),
         address_line = if_else(address_line == "x400 sproul rd springield 19064",
                                "x400 sproul rd springfield 19064",address_line),
         address_line = if_else(address_line == "x4 walnut st dunlevy 15342",
                                "x4 walnut st dunlevy 15432",address_line),
         name = if_else(name == "dunlevy boro recreation bldg",
                        "dunlevy boro recreation bldg",name),
         address_line = if_else(address_line == "x4 and b sts girardville 17935",
                                "x4 and b st girardville 17935",address_line),
         address_line = if_else(address_line == "x3916 old wm penn murrysville 15668",
                                "x3916 old william penn hwy murrysville 15668",
                                address_line),
         address_line = if_else(address_line == "x3735 7 th st new kensingtn 15068",
                                "x3735 7th street rd new kensington 15068",
                                address_line),
         address_line = if_else(address_line == "x373 beaver rd leetsdale 15056",
                                "x373 beaver st leetsdale 15056", address_line),
         address_line = if_else(address_line == "x3710 saxonburg blvd indianola 15051",
                                "x3710 saxonburg blvd indiana 15051", address_line),
         address_line = if_else(address_line == "x3640 old oakdale rd mc donald 15057",
                                "x3640 old oakdale rd mcdonald 15057",address_line),
         address_line = if_else(address_line == "x349 magee rd sewickley hl 15143",
                                "x349 magee rd sewickley 15143",address_line),
         address_line = if_else(address_line == "x3454 pleasantvue dr baldwin br 15227",
                                "x3454 pleasantvue dr pittsburgh 15227",
                                address_line),
         address_line = if_else(address_line == "x344 winthrop st south
                                williamsport 17702","x344 winthrop st s
                                williamsport 17702", address_line),
         address_line = if_else(address_line == "x344 ninth ave tarentum 15084",
                                "x344 9th ave tarentum 15084", address_line),
         address_line = if_else(address_line == "x3436 windchester rd allentown 18104",
                                "x3436 winchester rd allentown 18104", address_line),
         address_line = if_else(name == "kerr elementary school multi purp" &
                                  address_line != "x341 kittanning pike pittsburgh 15215",
                                "x341 kittanning pike pittsburgh 15215", address_line),
         address_line = if_else(address_line == "x330 hill top rd hummelstown 17036",
                                "x330 hilltop rd hummelstown 17036", address_line),
         name = if_else(name == "mt troy vol fire co ballroom",
                                "reserve township police department", name),
         address_line = if_else(address_line == "x3260 queneshukney rd linden 17744",
                                "x3260 queneshukeny rd linden 17744", address_line),
         address_line = if_else(address_line == "x325 nicholson rd pittsburgh 15143",
                                "x325 nicholson rd sewickley 15143", address_line),
         address_line = if_else(address_line == "x325 fox chapel rd rd pittsburgh 15215",
                                "x325 fox chapel rd pittsburgh 15238", address_line),
         address_line = if_else(address_line == "x320 fairies st wilcox 15870",
                                "x320 faries st wilcox 15870", address_line),
         address_line = if_else(address_line == "x315 susquehanst lancaster 17602",
                                "x315 susquehanna st lancaster 17602", address_line),
         address_line = if_else(address_line == "x315 40th st altoo16602",
                                "x315 40th st altoona 16602", address_line),
         address_line = if_else(address_line == "x3101 mccully rd hampton 15101",
                                "x3101 mccully rd allison park 15101", address_line),
         address_line = if_else(address_line == "x310 middleton rd carlisle 17013",
                                "x310 n middleton rd carlisle 17013", address_line),
         address_line = if_else(address_line == "x306 st james ln n alexandria 15670",
                                "x306 st james ln new alexandria 15670", address_line),
         address_line = if_else(address_line == "x306 bessemer st e pittsburgh 15112",
                                "x306 bessemer ave e pittsburgh 15112", address_line),
         address_line = if_else(address_line == "x3054 howes run rd natroheights 15065",
                                "x3054 howes run rd tarentum 15084", address_line),
         address_line = if_else(address_line == "x3017 union ave altoo16602",
                                "x3017 union ave altoona 16602", address_line),
         address_line = if_else(address_line == "x301 station ave wilmerding 15148",
                                "x301 station st wilmerding 15148", address_line),
         address_line = if_else(address_line == "x30 charter way wilkes barre 18702",
                                "x30 charter school way wilkes barre 18702",
                                address_line),
         address_line = if_else(address_line == "x3 keystone cmns white haven 18661",
                                "x3 keystone commons white haven 18661", address_line)) %>%
  filter(zipcode > 13235 & zipcode != 191) %>%
  select(county, name, address_line, zipcode) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(n_distinct(address_line) == 5718) %>%
  arrange(address_line) %>%
  verify(nrow(.) == 5718 & ncol(.) == 4)

# nrows = number of unique zip codes in 2016
pa_zips_freq_2016 <- as.data.frame(table(pa_2016_df$zipcode)) %>%
  mutate(zipcode = Var1,
         n_pp_2016 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 1488)

# PA 2020 data ##########
# started with 9234 rows and so far at 5190 unique, accurate addresses but still
# working on it
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
  mutate(address_line = as.character(make_clean_names(address_line, sep_out = " ",
                                                      unique_sep = NULL)),
         address_line = as.character(sub("_.*", "", address_line)),
         address_line = as.character(gsub(" na ", "", address_line)),
         name = as.character(gsub(" na ", "", name)),
         address_line = as.character(gsub(" street ", " st ", address_line)),
         address_line = as.character(gsub(" avenue ", " ave ", address_line)),
         address_line = as.character(gsub(" road ", " rd ", address_line)),
         address_line = as.character(gsub(" drive ", " dr ", address_line)),
         address_line = as.character(gsub(" south ", " s ", address_line)),
         address_line = as.character(gsub(" north ", " n ", address_line)),
         address_line = as.character(gsub(" west ", " w ", address_line)),
         address_line = as.character(gsub(" east ", " e ", address_line)),
         address_line = as.character(gsub(" first ", " 1st ", address_line)),
         address_line = as.character(gsub(" second ", " 2nd ", address_line)),
         address_line = as.character(gsub(" third ", " 3rd ", address_line)),
         address_line = as.character(gsub(" fourth ", " 4th ", address_line)),
         address_line = as.character(gsub(" fifth ", " 5th ", address_line)),
         address_line = as.character(gsub(" sixth ", " 6th ", address_line)),
         address_line = as.character(gsub(" seventh ", " 7th ", address_line)),
         address_line = as.character(gsub(" eigth ", " 8th ", address_line)),
         address_line = as.character(gsub(" ninth ", " 9th ", address_line)),
         address_line = as.character(gsub(" tenth ", " 10th ", address_line)),
         address_line = as.character(gsub(" 01st ", " 1st ", address_line)),
         address_line = as.character(gsub(" 02nd ", " 2nd ", address_line)),
         address_line = as.character(gsub(" 03rd ", " 3rd ", address_line)),
         address_line = as.character(gsub(" 04th ", " 4th ", address_line)),
         address_line = as.character(gsub(" 05th ", " 5th ", address_line)),
         address_line = as.character(gsub(" 06th ", " 6th ", address_line)),
         address_line = as.character(gsub(" 07th ", " 7th ", address_line)),
         address_line = as.character(gsub(" 08th ", " 8th ", address_line)),
         address_line = as.character(gsub(" 09th ", " 9th ", address_line)),
         address_line = if_else(address_line == "x9800 roosevelt blv philadelphia 19115",
                                "x9800 roosevelt blvd philadelphia 19115",
                                address_line),
         address_line = if_else(address_line == "x95 center circle rd west hickory 16370",
                                "x95 center circle st west hickory 16370",
                                address_line),
         name = if_else(name == "hickory township","harmony township building",
                                name),
         address_line = if_else(address_line == "x9151 old newton rd philadelphia 19115",
                                "x9151 old newtown rd philadelphia 19115",
                                address_line),
         address_line = if_else(address_line == "x8th olive sts coatesville 19320",
                                "x8th olive st coatesville 19320",address_line),
         address_line = if_else(address_line == "x860 colonial manor large bldg rd n huntingdon 15642",
                                "x860 colonial manor rd n huntingdon 15642",
                                address_line),
         address_line = if_else(address_line == "x825 poplar ave pittsburgh 15220",
                                "x825 poplar st pittsburgh 15220",address_line),
         address_line = if_else(address_line == "x81 commonwealth dr west mifflin 15122",
                                "x81 commonwealth ave west mifflin 15122",
                                address_line),
         address_line = if_else(address_line == "x800 coopertown rd bryn mawr 19010",
                                "x800 coopertown rd haverford 19041",
                                address_line),
         address_line = if_else(address_line == "x7701 mansfield ave philadelphia 19138",
                                "x7701 mansfield ave philadelphia 19150",
                                address_line),
         address_line = if_else(address_line == "x761 47th st philadelphia 19104",
                                "x761 47th st philadelphia 19139",address_line),
         address_line = if_else(address_line == "x7600 evans st swissvale 15218",
                                "x7600 evans st pittsburgh 15218",address_line),
         address_line = if_else(address_line == "x751 sugar run rd altoona 16602",
                                "x751 sugar run rd altoona 16601",address_line),
         address_line = if_else(address_line == "x75 evas st pringle 18704",
                                "x75 evans st pringle 18704",address_line),
         address_line = if_else(address_line == "x703 stevenson blvd new kensingtn 15068",
                                "x703 stevenson blvd new kensington 15068",
                                address_line),
         address_line = if_else(address_line == "x700 york mitchell ave lansdale 19446",
                                "x700 york ave lansdale 19446",address_line),
         address_line = if_else(address_line == "x6841 19th st philadelphia 19137",
                                "x6841 19th st philadelphia 19126",address_line),
         address_line = if_else(address_line == "x6801 grovers st philadelphia 19142",
                                "x6801 grovers ave philadelphia 19142",address_line),
         address_line = if_else(address_line == "x661 newberry st 17404",
                                "x661 newberry st york 17404",address_line),
         address_line = if_else(address_line == "x6501 limekiln pk philadelphia 19138",
                                "x6501 limekiln pike philadelphia 19138",address_line),
         address_line = if_else(address_line == "x6435 frankstown rd pittsburgh 15206",
                                "x6435 frankstown ave pittsburgh 15206",address_line),
         address_line = if_else(address_line == "x605 ross ave pitsburgh 15221",
                                "x605 ross ave pittsburgh 15221",address_line),
         address_line = if_else(address_line == "x6001 cedar ave philadelphia 19139",
                                "x6001 cedar ave philadelphia 19143",address_line),
         address_line = if_else(address_line == "x60 gawaldo rd pittsburgh 15108" |
                                  address_line == "x60 gawaldo rd coraopolis 15108",
                                "x60 gawaldo dr coraopolis 15108",address_line),
         address_line = if_else(address_line == "x590 crane a pittsburgh 15216",
                                "x590 crane ave pittsburgh 15216",address_line),
         address_line = if_else(address_line == "x5898 lancaster ave philadelphia 19124",
                                "x5898 lancaster ave philadelphia 19131",address_line),
         address_line = if_else(address_line == "x5801 media st philadelphia 19146",
                                "x5801 media st philadelphia 19131",address_line),
         address_line = if_else(address_line == "x5800 chester ave philadelphia 19142",
                                "x5800 chester ave philadelphia 19143",address_line),
         address_line = if_else(address_line == "x5799 hampton st pittsburgh 15201",
                                "x5799 hampton st pittsburgh 15206",address_line),
         address_line = if_else(address_line == "x551 ravensburg blvd pittsburgh 15025",
                                "x551 ravensburg blvd clairton 15025",address_line),
         address_line = if_else(address_line == "x5470 mcalevy fort rd huntingdon 16652",
                                "x5470 mcalevys fort rd petersburg 16669",address_line),
         address_line = if_else(address_line == "x545 ppermastone drive northumberland 17857",
                                "x545 permastone drive northumberland 17857",address_line),
         address_line = if_else(address_line == "x522 rock run rd pittsburgh 15037",
                                "x522 rock run rd elizabeth 15037",address_line),
         address_line = if_else(address_line == "x506 bessemer st e pittsburgh 15112",
                                "x506 bessemer ave e pittsburgh 15112",address_line),
         address_line = if_else(address_line == "x503 speer st n belle vern 15012",
                                "x503 speer st belle vernon 15012",address_line),
         address_line = if_else(address_line == "x503 main st meadville 16335",
                                "x503 main st ext meadville 16335",address_line),
         address_line = if_else(address_line == "x501 butler st springdale 15144",
                                "x501 butler rd springdale 15144",address_line),
         address_line = if_else(address_line == "x4905 fifth ave pittsburgh 15213",
                                "x4905 5th ave pittsburgh 15213",address_line),
         address_line = if_else(address_line == "x4901 parrish philadelphia 19139",
                                "x4901 parrish st philadelphia 19139",address_line),
         address_line = if_else(address_line == "x4901 chestnut st philadelphia 19104",
                                "x4901 chestnut st philadelphia 19139",address_line),
         address_line = if_else(address_line == "x4565 prestwick dr reading 19601",
                                "x4565 prestwick dr reading 19606",address_line),
         address_line = if_else(address_line == "x4300 ave of republice philadelphia 19131",
                                "x4300 ave of republic philadelphia 19131",address_line),
         address_line = if_else(address_line == "x429 berwick hazleton hwy nescopeck 18635",
                                "x429 berwick hazelton hwy nescopeck 18635",address_line),
         address_line = if_else(address_line == "x428 georgetown rd lawrence 15055",
                                "x428 georgetown road lawrence 15055",address_line),
         address_line = if_else(address_line == "x423 allegheny st h0llidaysburg 16648",
                                "x423 allegheny st hollidaysburg 16648",address_line),
         address_line = if_else(address_line == "x419 pearson rd lititz 17543",
                                "x419 pierson rd lititz 17543",address_line),
         address_line = if_else(address_line == "x417 07th st allentown 18102",
                                "x417 7th st allentown 18102",address_line),
         address_line = if_else(address_line == "x416 lincoln ave pittsburgh 15209",
                                "x416 lincoln ave millvale 15209",address_line),
         address_line = if_else(address_line == "x415 brennan ave loyalhanna 15661",
                                "x415 brennan ave latrobe 15650",address_line),
         address_line = if_else(address_line == "x414 railroad st pittsburgh 15028" |
                                address_line == "x414 railroad st coulters 15028",
                                "x414 railroad st white oak 15131",address_line),
         address_line = if_else(address_line == "x400 sproul rd springfield 19064",
                                "x400 sproul rd springield 19064",address_line),
         address_line = if_else(address_line == "x400 sproul rd springield 19064",
                                "x400 sproul rd springfield 19064",address_line),
         address_line = if_else(address_line == "x40 prospect ave pitsburgh 15205",
                                "x40 prospect ave pittsburgh 15205",address_line),
         address_line = if_else(address_line == "x4 westtown thornton rd thornton 19373",
                                "x4 westtown rd thornton 19373",address_line),
         address_line = if_else(address_line == "x4 and b sts girardville 17935",
                                "x4 and b st girardville 17935",address_line),
         address_line = if_else(address_line == "x3856 10 th st erie 16505",
                                "x3856 10th st erie 16505",address_line),
         address_line = if_else(address_line == "x370 carvertown rd shavertown 18708",
                                "x370 carverton rd shavertown 18708",address_line),
         address_line = if_else(address_line == "x3640 old oakdale rd mc donald 15057",
                                "x370 carverton rd shavertown 18708",address_line),
         address_line = if_else(address_line == "x3501 midvale ave philadelphia 19125" |
                                  address_line == "x3501 midvale ave philadelphia 19121",
                                "x3501 midvale ave philadelphia 19129",address_line),
         address_line = if_else(address_line == "x3436 windchester rd allentown 18104",
                                "x3436 winchester rd allentown 18104", address_line),
         address_line = if_else(name == "kerr elementary school" &
                                  address_line != "x341 kittanning pike pittsburgh 15215",
                                "x341 kittanning pike pittsburgh 15215", address_line),
         address_line = if_else(address_line == "x338 28th st philadelphia 19103",
                                "x338 26th st philadelphia 19103", address_line),
         address_line = if_else(address_line == "x3351 millers run rd mcdonald 15057",
                                "x3351 millers run rd cecil 15321", address_line),
         address_line = if_else(address_line == "x3344 churchview ave ext ave pittsburgh 15227",
                                "x3344 churchview ave pittsburgh 15227", address_line),
         address_line = if_else(address_line == "x334 carlisle av york 17404",
                                "x334 carlisle ave york 17404", address_line),
         address_line = if_else(address_line == "x330 hill top rd hummelstown 17036",
                                "x330 hilltop rd hummelstown 17036", address_line),
         address_line = if_else(address_line == "x33 lonsdale st pittsburgh 15214",
                                "x33 lonsdale st pittsburgh 15212", address_line),
         name = if_else(name == "reserve township volunteer fire department",
                        "reserve township police department", name),
         address_line = if_else(address_line == "x33 lewin ln pittsburgh 15235",
                                "x33 lewin lane pittsburgh 15235", address_line),
         address_line = if_else(address_line == "x3260 quenshukeny rd linden 17744",
                                "x3260 queneshukeny rd", address_line),
         address_line = if_else(address_line == "x326 napoleon st johnstown 15905",
                                "x326 napoleon st johnstown 15901", address_line),
         address_line = if_else(address_line == "x325 07th st philadelphia 19107",
                                "x325 07th st philadelphia 19106", address_line),
         address_line = if_else(address_line == "x3245 oakland rd bethlehem 18017",
                                "x3245 oakland rd bethlehem 18020", address_line),
         address_line = if_else(address_line == "x320 hawkins ave braddock 15104",
                                "x320 hawkins ave rankin 15104", address_line),
         address_line = if_else(address_line == "x306 st james ln n alexandria 15670",
                                "x306 st james ln new alexandria 15670", address_line),
         address_line = if_else(address_line == "x3010 route 212 springtown 18055",
                                "x3010 route 212 springtown 18081", address_line)) %>%
    filter(zipcode > 13235 & zipcode != 191) %>%
    select(county, name, address_line, zipcode) %>%
    distinct(address_line, .keep_all = TRUE) %>%
    verify(n_distinct(address_line) == 5190) %>%
    arrange(address_line) %>%
    verify(nrow(.) == 5190 & ncol(.) == 4)

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

# 1926/5219 polling places are new in 2020
all_places <- full_join(pa_2020_df, pa_2016_df, by = "address_line",
                        suffix = c("2020", "2016"))
# 7280, stopped at the x2's on 8/28
# there are so many incorrect addresses, data entry inconsistencies, etc..




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
