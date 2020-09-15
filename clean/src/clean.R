#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/clean/src/clean.R

# Load libraries ---------------------------------------------------------------
pacman::p_load("here", "tidyverse", "assertr", "janitor",
               "stringr")

# Specify file locations -------------------------------------------------------
inputs <- list(
  VIPinlist_imp = here("clean/input/VIPdata_imported.rds"),
  pa_2016_imp = here("clean/input/pa2016_imported.rds"),
  pa_2020_imp = here("clean/input/pa2020_imported.rds"),
  census_imp = here("clean/input/census_imported.rds"),
  counnzip_azscpa_imp = here("clean/input/counzip_azscpa_imported.rds"),
  az_adds = here("hand/az_addresses.txt"),
  sc_adds = here("hand/sc_addresses.txt"),
  pa_adds = here("hand/pa_addresses.txt"),
  pa_adds_2 = here("hand/pa_addresses_byname.txt")
)
outputs <- list(
  az_2016_clean = here("write/input/az_2016_clean.rds"),
  az_2020_clean = here("write/input/az_2020_clean.rds"),
  az_demo_clean = here("write/input/az_demo_clean.rds"),
  az_places = here::here("write/input/az_places.rds"),

  sc_2016_clean = here("write/input/sc_2016_clean.rds"),
  sc_2020_clean = here("write/input/sc_2020_clean.rds"),
  sc_demo_clean = here("write/input/sc_demo_clean.rds"),
  sc_places = here::here("write/input/sc_places.rds"),

  pa_2016_clean = here("write/input/pa_2016_clean.rds"),
  pa_2020_clean = here("write/input/pa_2020_clean.rds"),
  pa_demo_clean = here("write/input/pa_demo_clean.rds"),
  pa_places = here::here("write/input/pa_places.rds")
)

# custom functions -------------------------------------------------------------
# create function to replace non-standard address parts
stn_adds <- function(df, x) {
  mutate(df,
         x = as.character(gsub(" s ", " south ", x)),
         x = as.character(gsub(" n ", " north ", x)),
         x = as.character(gsub(" w ", " west ", x)),
         x = as.character(gsub(" e ", " east ", x)),
         x = as.character(gsub(" street ", " st ", x)),
         x = as.character(gsub(" sts ", " st ", x)),
         x = as.character(gsub(" avenue ", " ave ", x)),
         x = as.character(gsub(" av ", " ave ", x)),
         x = as.character(gsub(" road ", " rd ", x)),
         x = as.character(gsub(" drive ", " dr ", x)),
         x = as.character(gsub(" boulevard ", " blvd ", x)),
         x = as.character(gsub(" blv ", " blvd ", x)),
         x = as.character(gsub(" route ", " rte ", x)),
         x = as.character(gsub(" hiway ", " hwy ", x)),
         x = as.character(gsub(" highway ", " hwy ", x)),
         x = as.character(gsub(" parkway ", " pkwy ", x)),
         x = as.character(gsub(" rt ", " rte ", x)),
         x = as.character(gsub(" lane ", " ln ", x)),
         x = as.character(gsub(" first ", " 1st ", x)),
         x = as.character(gsub(" second ", " 2nd ", x)),
         x = as.character(gsub(" third ", " 3rd ", x)),
         x = as.character(gsub(" fourth ", " 4th ", x)),
         x = as.character(gsub(" fifth ", " 5th ", x)),
         x = as.character(gsub(" sixth ", " 6th ", x)),
         x = as.character(gsub(" seventh ", " 7th ", x)),
         x = as.character(gsub(" eigth ", " 8th ", x)),
         x = as.character(gsub(" ninth ", " 9th ", x)),
         x = as.character(gsub(" tenth ", " 10th ", x)),
         x = as.character(gsub(" 01st ", " 1st ", x)),
         x = as.character(gsub(" 02nd ", " 2nd ", x)),
         x = as.character(gsub(" 03rd ", " 3rd ", x)),
         x = as.character(gsub(" 04th ", " 4th ", x)),
         x = as.character(gsub(" 05th ", " 5th ", x)),
         x = as.character(gsub(" 06th ", " 6th ", x)),
         x = as.character(gsub(" 07th ", " 7th ", x)),
         x = as.character(gsub(" 08th ", " 8th ", x)),
         x = as.character(gsub(" 09th ", " 9th ", x)),
         x = as.character(gsub("first ", "1st ", x)),
         x = as.character(gsub("second ", "2nd ", x)),
         x = as.character(gsub("third ", "3rd ", x)),
         x = as.character(gsub("fourth ", "4th ", x)),
         x = as.character(gsub("fifth ", "5th ", x)),
         x = as.character(gsub("sixth ", "6th ", x)),
         x = as.character(gsub("seventh ", "7th ", x)),
         x = as.character(gsub("eigth ", "8th ", x)),
         x = as.character(gsub("ninth ", "9th ", x)),
         x = as.character(gsub("tenth ", "10th ", x)),
         x = as.character(gsub("01st ", "1st ", x)),
         x = as.character(gsub("02nd ", "2nd ", x)),
         x = as.character(gsub("03rd ", "3rd ", x)),
         x = as.character(gsub("04th ", "4th ", x)),
         x = as.character(gsub("05th ", "5th ", x)),
         x = as.character(gsub("06th ", "6th ", x)),
         x = as.character(gsub("07th ", "7th ", x)),
         x = as.character(gsub("08th ", "8th ", x)),
         x = as.character(gsub("09th ", "9th ", x)))
  return(x)
}

# Add in counties for each zip --------------------------------------------------
az_cos <- read_rds(inputs$counnzip_azscpa_imp) %>%
  filter(state == "AZ") %>%
  mutate(zip = as.character(zip)) %>%
  verify(ncol(.) == 3 & nrow(.) == 568)

sc_cos <- read_rds(inputs$counnzip_azscpa_imp) %>%
  filter(state == "SC") %>%
  mutate(zip = as.character(zip)) %>%
  mutate(county = gsub(" [A-z ]*", "", county)) %>%
  verify(ncol(.) == 3 & nrow(.) == 540)

# Re-add county information to pa data because so many zip codes were inaccurate
pa_cos <- read_rds(inputs$counnzip_azscpa_imp) %>%
  mutate(zip = as.character(zip)) %>%
  mutate(county = gsub(" [A-z ]*", "", county)) %>%
  verify(ncol(.) == 3 & nrow(.) == 3318)

# VIP data AZ and SC -----------------------------------------------------------
# AZ ---------------------------------------------------------------------------
n_last <- 5
# these ids contain statements about voting rather than addresses
#   ex: "PLEASE CHECK THE COUNTY WEBSITE FOR THE VOTE" or
#   "NO POLLING PLACES - A BALLOT WILL BE MAILED  AZ", or zip codes like "0 AZ"
invalid_info <- c(
  "4600331", "46002736", "4600617", "4600618",
  "4610205126", "46002463", "4600178", "46003124", "46001645",
  "46002418", "46002423", "46002453", "46002446"
)

az_address_data <- read_delim(inputs$az_adds, delim = "|") %>%
  mutate_all(as.character)

az_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 1) %>%
  verify(ncol(.) == 3 & nrow(.) == 342) %>%
  verify(min(id) == 401333310112 & max(id) == 4610933) %>%
  mutate_all(as.character) %>%
  mutate(zipcode = as.character(substr(
    address_line, nchar(address_line) - n_last + 1, nchar(address_line)))) %>%
  filter(!id %in% invalid_info) %>%
  full_join(az_address_data, by = "id") %>%
  mutate(
  zipcode = if_else(is.na(new_zip) == TRUE, zipcode, new_zip),
  id = if_else(is.na(new_id) == TRUE, id, new_id),
  address_line = as.character(if_else(is.na(new_address) == TRUE,
                                      address_line, new_address)),
  address_line = make_clean_names(address_line, sep_out = " ", unique_sep = NULL),
  address_line = as.character(sub("_.*", "", address_line)),
  name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
  name = sub("_.*", "", name),
  address_line = as.character(stn_adds(., address_line))) %>%
  filter(name != "na") %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(nrow(.) == 322 & ncol(.) == 7) %>%
  arrange(address_line)

az_zips_freq_2016 <- as.data.frame(table(az_2016_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2016 = as.numeric(Freq)) %>%
  verify(ncol(.) == 4 & nrow(.) == 195)

az_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 2) %>%
  verify(ncol(.) == 3 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999) %>%
  mutate_all(as.character) %>%
  full_join(az_address_data, by = "id") %>%
  mutate(
    zipcode = as.character(substr(address_line,
                                   nchar(address_line) - n_last + 1,
                                   nchar(address_line))),
    address_line = make_clean_names(address_line, sep_out = " ", unique_sep = NULL),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = stn_adds(., address_line),
    zipcode = if_else(is.na(new_zip) == TRUE, zipcode, new_zip)) %>%
  filter(zipcode != "12345") %>%
  filter(name != "na") %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(n_distinct(id, address_line) == 318) %>%
  verify(nrow(.) == 318 & ncol(.) == 7) %>%
  arrange(address_line)

az_2020_maricopa_df <- pluck(read_rds(inputs$VIPinlist_imp), 3) %>%
  verify(ncol(.) == 3 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622) %>%
  mutate_all(as.character) %>%
  mutate(
    zipcode = as.character(substr(address_line,
                                  nchar(address_line) - n_last + 1,
                                  nchar(address_line))),
    address_line = make_clean_names(address_line, sep_out = " ", unique_sep = NULL),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = stn_adds(., address_line)) %>%
  filter(name != "na") %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(n_distinct(id, address_line) == 151) %>%
  verify(nrow(.) == 151 & ncol(.) == 4) %>%
  arrange(address_line)

az_2020_df_full <- full_join(az_2020_df, az_2020_maricopa_df) %>%
  verify(ncol(.) == 7 & nrow(.) == 469) %>%
  verify(min(id) == 7700104067 & max(id) == 8850015622) %>%
  full_join(az_address_data, by = "id") %>%
  mutate(
    address_line = tolower(address_line),
    name = tolower(name),
    zipcode = as.character(substr(address_line,nchar(address_line) - n_last + 1, nchar(address_line))),
    zipcode = if_else(is.na(new_zip.y) == TRUE, zipcode, new_zip.y),
    id = if_else(is.na(new_id.y) == TRUE, id, new_id.y),
    address_line = if_else(is.na(new_address.y) == TRUE, address_line, new_address.y),
    address_line = stn_adds(., address_line)) %>%
  filter(name != "na") %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(n_distinct(id, address_line) == 469) %>%
  verify(nrow(.) == 469 & ncol(.) == 10) %>%
  arrange(address_line)

az_zips_freq_2020 <- as.data.frame(table(az_2020_df_full$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2020 = as.numeric(Freq)
  ) %>%
  verify(is.numeric(n_pp_2020) == TRUE) %>%
  verify(ncol(.) == 4 & nrow(.) == 273)

n_places_az <- full_join(az_zips_freq_2020, az_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(
    delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
    delta_cat = case_when(
      delta_n_places == 0 ~ "Maintained Polling Locations",
      delta_n_places < 0 ~ "Lost Polling Locations",
      delta_n_places > 0 ~ "Gained Polling Locations")) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 284)

# SC ---------------------------------------------------------------------------

sc_address_data <- read_delim(inputs$sc_adds, delim = "|") %>%
  mutate_all(as.character)

sc_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 4) %>%
  verify(ncol(.) == 3 & nrow(.) == 2194) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_all(as.character) %>%
  full_join(sc_address_data, by = "id") %>%
  mutate(
    zipcode = as.character(substr(address_line,
                                  nchar(address_line) - n_last + 1,
                                  nchar(address_line))),
    address_line = make_clean_names(address_line, sep_out = " ", unique_sep = NULL),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = if_else(is.na(new_address) == TRUE, address_line, new_address),
    zipcode = if_else(is.na(new_zip) == TRUE, zipcode, new_zip),
    address_line = stn_adds(., address_line),
    id = if_else(is.na(new_id) == TRUE, id, new_id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  filter(!zipcode == "SC") %>%
  verify(zipcode %in% sc_cos$zip) %>%
  verify(n_distinct(id, address_line) == 2062) %>%
  verify(nrow(.) == 2062 & ncol(.) == 7) %>%
  arrange(address_line)

sc_zips_freq_2016 <- as.data.frame(table(sc_2016_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2016 = as.numeric(Freq)
  ) %>%
  filter(zipcode != "SC") %>%
  verify(zipcode != "SC") %>%
  verify(ncol(.) == 4 & nrow(.) == 377)

# Summerville is now in zipcode 29483 not 29486
summerville <- c(
  "88802641", "88802496", "88802274", "88801273",
  "88801246", "88801201", "8880203"
)

sc_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 5) %>%
  verify(ncol(.) == 3 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_all(as.character) %>%
  full_join(sc_address_data, by = "id") %>%
  mutate(
    zipcode = as.character(substr(address_line,
                                  nchar(address_line) - n_last + 1,
                                  nchar(address_line))),
    address_line = make_clean_names(address_line, sep_out = " ", unique_sep = NULL),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = if_else(is.na(new_address) == TRUE, address_line, new_address),
    zipcode = if_else(is.na(new_zip) == TRUE, zipcode, new_zip),
    address_line = stn_adds(., address_line),
    id = if_else(is.na(new_id) == TRUE, id, new_id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% sc_cos$zip)) %>%
  verify(n_distinct(address_line) == 1920) %>%
  verify(nrow(.) == 1920 & ncol(.) == 7) %>%
  arrange(address_line)

sc_zips_freq_2020 <- as.data.frame(table(sc_2020_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2020 = as.numeric(Freq)
  ) %>%
  filter(zipcode != "SC") %>%
  verify(zipcode != "SC") %>%
  verify(ncol(.) == 4 & nrow(.) == 379)

n_places_sc <- full_join(sc_zips_freq_2020, sc_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(
    delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
    delta_cat = case_when(
      delta_n_places == 0 ~ "Maintained Polling Locations",
      delta_n_places < 0 ~ "Lost Polling Locations",
      delta_n_places > 0 ~ "Gained Polling Locations"
    )
  ) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 381)

# PA data from state election officials ----------------------------------------

# Invalid zip codes in PA data
odd_zips <- c("0", "1", "13235", "1910", "191", "190", "191119", "194")

pa_address_data <- read_delim(inputs$pa_adds, delim = "|") %>%
  mutate_all(as.character)

pa_address_byname <- read_delim(inputs$pa_adds_2, delim = "|") %>%
  mutate_all(as.character)

pa_2016_df <- read_rds(inputs$pa_2016_imp) %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  mutate(
    street = tolower(if_else(street == "OLD ROUTE  30", "OLD ROUTE 30", street)),
    name = as.character(make_clean_names(description, sep_out = " ",
                                         unique_sep = NULL)),
    name = as.character(sub("_.*", "", name)),
    comment = tolower(comment),
    prefix_direction_desc = tolower(prefix_direction_desc),
    street_type_desc = tolower(street_type_desc),
    city = tolower(city),
    line2 = tolower(line2),
    zipcode = as.character(postal_code),
    county = as.character(snakecase::to_any_case(county_name,
      case = "big_camel"))) %>%
  select(county, name, house_num, street, street_type_desc, city, zipcode) %>%
  unite("address_line", house_num:zipcode, remove = FALSE, sep = " ") %>%
  drop_na(zipcode) %>%
  mutate(
    address_line = as.character(make_clean_names(address_line,
      sep_out = " ",
      unique_sep = NULL)),
    address_line = as.character(sub("_.*", "", address_line)),
    address_line = as.character(gsub(" na ", " ", address_line)),
    name = as.character(gsub(" na ", " ", name)),
    address_line = as.character(gsub("na ", " ", address_line)),
    name = as.character(gsub("na ", "", name)),
    address_line = as.character(gsub(" na", " ", address_line)),
    name = as.character(gsub(" na", "", name)),
    address_line = as.character(gsub(" pitsburgh ", " pittsburgh ",address_line)),
    address_line = as.character(gsub(" altoo ", " altoona ",address_line))) %>%
  full_join(pa_address_data, by = c("address_line" = "old_address")) %>%
  mutate(
    address_line = stn_adds(., address_line),
    address_line = if_else(is.na(new_address) == TRUE, address_line, new_address),
    zipcode = if_else(is.na(new_zip) == TRUE, zipcode, new_zip)) %>%
  full_join(pa_address_byname, by = "name") %>%
  mutate(
    address_line = stn_adds(., address_line),
    address_line = if_else(is.na(new_address.y) == TRUE, address_line, new_address.y),
    zipcode = if_else(is.na(new_zip.y) == TRUE, zipcode, new_zip.y)) %>%
  filter(!zipcode %in% odd_zips) %>%
  select(name, address_line, zipcode) %>%
  filter(!address_line == "x285 pine run church rd apollo 15613") %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify((zipcode %in% pa_cos$zip) == TRUE)


%>%
  verify(n_distinct(address_line) == 5061) %>%
  verify(nrow(.) == 5061 & ncol(.) == 3) %>%
  arrange(address_line)

# x285 pine run church rd apollo 15613 is a random empty house

pa_zips_freq_2020 <- as.data.frame(table(pa_2020_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2020 = as.numeric(Freq)
  ) %>%
  verify(ncol(.) == 4 & nrow(.) == 1503)

n_places_pa <- full_join(pa_zips_freq_2020, pa_zips_freq_2016, by = "zipcode") %>%
  replace_na(list(n_pp_2016 = 0, n_pp_2020 = 0)) %>%
  mutate(
    delta_n_places = as.numeric(n_pp_2020 - n_pp_2016),
    delta_cat = case_when(
      delta_n_places == 0 ~ "Maintained Polling Locations",
      delta_n_places < 0 ~ "Lost Polling Locations",
      delta_n_places > 0 ~ "Gained Polling Locations"
    )
  ) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  filter(!zipcode %in% odd_zips) %>%
  verify(!zipcode %in% odd_zips == TRUE) %>%
  verify(ncol(.) == 5 & nrow(.) == 1581)

# Add in ACS data --------------------------------------------------------------

az_demo <- read_rds(inputs$census_imp) %>%
  filter(geoid %in% n_places_az$zipcode) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(c(starts_with("nhl_"), starts_with("total"), "geoid", "name")) %>%
  verify(sum(total) == sum(total_nhl) + sum(total_hl)) %>%
  mutate(
    nhl_ao = as.numeric(nhl_sor + nhl_tom),
    gp_total =
      case_when(
        total > 70001 & total <= 80000 ~ "70,001-80,000",
        total > 60001 & total <= 70000 ~ "60,001-70,000",
        total > 50001 & total <= 60000 ~ "50,001-60,000",
        total > 40001 & total <= 50000 ~ "40,001-50,000",
        total > 30001 & total <= 40000 ~ "30,001-40,000",
        total > 20001 & total <= 30000 ~ "20,001-30,000",
        total > 10001 & total <= 20000 ~ "10,001-20,000",
        total <= 10000 ~ "0-10,000"
      )
  ) %>%
  group_by(geoid) %>%
  verify(sum(nhl_ao) == sum(nhl_sor + nhl_tom)) %>%
  verify(is.na(gp_total) == FALSE) %>%
  verify(is.na(name) == FALSE) %>%
  verify(is.na(geoid) == FALSE) %>%
  full_join(n_places_az, by = c("geoid" = "zipcode")) %>%
  full_join(az_cos, by = c("geoid" = "zip")) %>%
  mutate(county = if_else(is.na(county) == TRUE, "Missing County", county)) %>%
  filter(county != "Missing County") %>%
  mutate(
    county = if_else(county == "McKinley County", "Apache County", county),
    county = if_else(county == "San Juan County", "Coconino County", county)
  ) %>%
  filter(is.na(total) == FALSE) %>%
  verify(county != "McKinley County" | county != "San Juan County" |
    county != "Missing County") %>%
  verify(ncol(.) == 20 & nrow(.) == 284)

sc_demo <- pluck(read_rds(inputs$census_imp)) %>%
  filter(geoid %in% n_places_sc$zipcode) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  select(c(starts_with("nhl_"), starts_with("total"), "geoid", "name")) %>%
  verify(sum(total) == sum(total_nhl) + sum(total_hl)) %>%
  mutate(
    nhl_ao = as.numeric(nhl_sor + nhl_tom),
    gp_total =
      case_when(
        total > 80001 & total <= 90000 ~ "80,001-90,000",
        total > 70001 & total <= 80000 ~ "70,001-80,000",
        total > 60001 & total <= 70000 ~ "60,001-70,000",
        total > 50001 & total <= 60000 ~ "50,001-60,000",
        total > 40001 & total <= 50000 ~ "40,001-50,000",
        total > 30001 & total <= 40000 ~ "30,001-40,000",
        total > 20001 & total <= 30000 ~ "20,001-30,000",
        total > 10001 & total <= 20000 ~ "10,001-20,000",
        total <= 10000 ~ "0-10,000"
      )
  ) %>%
  group_by(geoid) %>%
  verify(sum(nhl_ao) == sum(nhl_sor + nhl_tom)) %>%
  verify(is.na(gp_total) == FALSE) %>%
  verify(is.na(name) == FALSE) %>%
  verify(is.na(geoid) == FALSE) %>%
  full_join(n_places_sc, by = c("geoid" = "zipcode")) %>%
  full_join(sc_cos, by = c("geoid" = "zip")) %>%
  mutate(county = if_else(is.na(county) == TRUE, "Missing County", county)) %>%
  filter(county != "Missing County") %>%
  filter(is.na(total) == FALSE) %>%
  verify(county != "Missing County") %>%
  verify(ncol(.) == 20 & nrow(.) == 380)

pa_demo <- pluck(read_rds(inputs$census_imp)) %>%
  filter(geoid %in% n_places_pa$zipcode) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  verify(sum(total) == sum(total_nhl) + sum(total_hl)) %>%
  mutate(
    nhl_ao = as.numeric(nhl_sor + nhl_tom),
    gp_total =
      case_when(
        total > 70001 & total <= 80000 ~ "70,001-80,000",
        total > 60001 & total <= 70000 ~ "60,001-70,000",
        total > 50001 & total <= 60000 ~ "50,001-60,000",
        total > 40001 & total <= 50000 ~ "40,001-50,000",
        total > 30001 & total <= 40000 ~ "30,001-40,000",
        total > 20001 & total <= 30000 ~ "20,001-30,000",
        total > 10001 & total <= 20000 ~ "10,001-20,000",
        total <= 10000 ~ "0-10,000"
      )
  ) %>%
  filter(geoid != 18936) %>%
  group_by(geoid) %>%
  verify(sum(nhl_ao) == sum(nhl_sor + nhl_tom)) %>%
  verify(is.na(gp_total) == FALSE) %>%
  verify(is.na(name) == FALSE) %>%
  verify(is.na(geoid) == FALSE) %>%
  full_join(n_places_pa, by = c("geoid" = "zipcode")) %>%
  full_join(pa_cos, by = c("geoid" = "zip")) %>%
  filter(is.na(total) == FALSE) %>%
  verify(ncol(.) == 21 & nrow(.) == 1521)

# Export all objects for write task --------------------------------------------

az_2016_df <- az_2016_df %>%
  saveRDS(outputs$az_2016_clean)

az_2020_df_full <- az_2020_df_full %>%
  saveRDS(outputs$az_2020_clean)

sc_2016_df <- sc_2016_df %>%
  saveRDS(outputs$sc_2016_clean)

sc_2020_df <- sc_2020_df %>%
  saveRDS(outputs$sc_2020_clean)

pa_2016_df <- pa_2016_df %>%
  saveRDS(outputs$pa_2016_clean)

pa_2020_df <- pa_2020_df %>%
  saveRDS(outputs$pa_2020_clean)

az_demo <- az_demo %>%
  saveRDS(outputs$az_demo_clean)

sc_demo <- sc_demo %>%
  saveRDS(outputs$sc_demo_clean)

pa_demo <- pa_demo %>%
  saveRDS(outputs$pa_demo_clean)

n_places_az %>%
  saveRDS(outputs$az_places)

n_places_sc %>%
  saveRDS(outputs$sc_places)

n_places_pa %>%
  saveRDS(outputs$pa_places)

# done.
