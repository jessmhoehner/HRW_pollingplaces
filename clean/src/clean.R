#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/clean/src/clean.R

# Load libraries ---------------------------------------------------------------
pacman::p_load("here", "tidyverse", "assertr", "janitor")

# Specify file locations -------------------------------------------------------
inputs <- list(
  VIPinlist_imp = here("clean/input/VIPdata_imported.rds"),
  pa_2016_imp = here("clean/input/pa2016_imported.rds"),
  pa_2020_imp = here("clean/input/pa2020_imported.rds"),
  census_imp = here("clean/input/census_imported.rds"),
  counnzip_azscpa_imp = here("clean/input/counzip_azscpa_imported.rds")
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

az_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 1) %>%
  verify(ncol(.) == 3 & nrow(.) == 342) %>%
  verify(min(id) == 401333310112 & max(id) == 4610933) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(
    address_line, nchar(address_line) - n_last + 1, nchar(address_line)))) %>%
  filter(!id %in% invalid_info) %>%
  mutate(
    zipcode = if_else(id == "46002418", "85142", zipcode),
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
      "cornfields chapter house ganado az 86505", address_line
    ),
    address_line = if_else(id == "46001812",
      "cottonwood chapter house chinle az 86503", address_line
    ),
    address_line = if_else(id == "46001206",
      "black mesa chapter house black mesa az 86033", address_line
    ),
    address_line = if_else(id == "46001841",
      "st. michaels chapter house st. michaels az 85611", address_line
    ),
    address_line = if_else(id == "46001828",
      "mexican water chapter mexican water az 86514", address_line
    ),
    address_line = if_else(id == "46001822",
      "kinlichee chapter house st michaels az 86511", address_line
    ),
    address_line = if_else(id == "46001213",
      "forest lake chapter house forest lake az 86033", address_line
    ),
    address_line = if_else(id == "46003069",
      "federal route 15 & saint  augustine street sells az 85122", address_line
    ),
    address_line = if_else(id == "46003018",
      "962 w gila bend highway casa grande az 85122",
      address_line
    ),
    address_line = if_else(id == "46003017",
      "8470 n overfield rd  coolidge az 85128",
      address_line
    ),
    address_line = if_else(id == "46002483",
      "824 thorn avenue winkelman az 85192",
      address_line
    ),
    address_line = if_else(id == "46003056",
      "5782 s mountainbrook dr gold canyon az 85118",
      address_line
    ),
    address_line = if_else(id == "46003047",
      "575 n idaho rd apache junction az 85119",
      address_line
    ),
    address_line = if_else(id == "46003011",
      "550 s ironwood drive apache junction az 85120",
      address_line
    ),
    address_line = if_else(id == "46003324",
      "50 bridge road tubac az 85646",
      address_line
    ),
    address_line = if_else(id == "46002412",
      "4621 south 9th street canyon day az 85491",
      address_line
    ),
    address_line = if_else(id == "46003096",
      "39315 n cortona dr san tan valley az 85140",
      address_line
    ),
    address_line = if_else(id == "46003078",
      "3650 w shedd road eloy az 85131",
      address_line
    ),
    address_line = if_else(id == "46003082",
      "3496 w casa blanca rd bapchule az 85121",
      address_line
    ),
    address_line = if_else(id == "46003314",
      "346 duquesne ave. patagonia az 85624",
      address_line
    ),
    address_line = if_else(id == "4600205063",
      "33622 n mountain vista blvd san tan valley az 85142",
      address_line
    ),
    address_line = if_else(id == "46003039",
      "28479 n main street san tan valley az 85143",
      address_line
    ),
    address_line = if_else(id == "40133334004",
      "222 e javelina ave mesa az 85210",
      address_line
    ),
    address_line = if_else(id == "4600911",
      "1730 kino ave kingman az 86409", address_line
    ),
    address_line = if_else(id == "46003071",
      "1084 w san tan hills dr san tan valley az 85143",
      address_line
    ),
    address_line = if_else(id == "46003049",
      "100 sunset drive superior az 85173", address_line
    ),
    address_line = if_else(id == "46002418",
      "100 sunset drive superior az 85173", address_line
    ),
    address_line = make_clean_names(address_line,
      sep_out = " ",
      unique_sep = NULL
    ),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = as.character(gsub(" s ", " south ", address_line)),
    address_line = as.character(gsub(" n ", " north ", address_line)),
    address_line = as.character(gsub(" w ", " west ", address_line)),
    address_line = as.character(gsub(" e ", " east ", address_line)),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line))) %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(nrow(.) == 322 & ncol(.) == 4) %>%
  arrange(address_line)

az_zips_freq_2016 <- as.data.frame(table(az_2016_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2016 = as.numeric(Freq)
  ) %>%
  verify(ncol(.) == 4 & nrow(.) == 193)

az_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 2) %>%
  verify(ncol(.) == 3 & nrow(.) == 330) %>%
  verify(min(id) == 7700104067 & max(id) == 770099999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(
    zipcode = as.character(substr(
      address_line, nchar(address_line) - n_last + 1,
      nchar(address_line))),
    address_line = make_clean_names(address_line,
      sep_out = " ",
      unique_sep = NULL),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = as.character(gsub(" s ", " south ", address_line)),
    address_line = as.character(gsub(" n ", " north ", address_line)),
    address_line = as.character(gsub(" w ", " west ", address_line)),
    address_line = as.character(gsub(" e ", " east ", address_line)),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line)),
    zipcode = if_else(zipcode == "89501", "85901", zipcode),
    zipcode = if_else(zipcode == "85491", "85941", zipcode),
    zipcode = if_else(zipcode == "86529", "85925", zipcode)
    ) %>%
  filter(zipcode != "12345") %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(n_distinct(id, address_line) == 318) %>%
  verify(nrow(.) == 318 & ncol(.) == 4) %>%
  arrange(address_line)

az_2020_maricopa_df <- pluck(read_rds(inputs$VIPinlist_imp), 3) %>%
  verify(ncol(.) == 3 & nrow(.) == 151) %>%
  verify(min(id) == 8850010055 & max(id) == 8850015622) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(
    zipcode = as.factor(substr(
      address_line, nchar(address_line) - n_last + 1,
      nchar(address_line)
    )),
    address_line = make_clean_names(address_line,
      sep_out = " ",
      unique_sep = NULL
    ),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = as.character(gsub(" s ", " south ", address_line)),
    address_line = as.character(gsub(" n ", " north ", address_line)),
    address_line = as.character(gsub(" w ", " west ", address_line)),
    address_line = as.character(gsub(" e ", " east ", address_line)),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line))
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(n_distinct(id, address_line) == 151) %>%
  verify(nrow(.) == 151 & ncol(.) == 4) %>%
  arrange(address_line)

az_2020_df_full <- full_join(az_2020_df, az_2020_maricopa_df) %>%
  verify(ncol(.) == 4 & nrow(.) == 469) %>%
  verify(min(id) == 7700104067 & max(id) == 8850015622) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(
    address_line = tolower(address_line),
    name = tolower(name),
    zipcode = as.character(substr(
      address_line,
      nchar(address_line) - n_last + 1,
      nchar(address_line)
    )),
    zipcode = if_else(id == "770039622" | id == "770039624",
      "85941", zipcode
    ),
    zipcode = if_else(zipcode == "89501", "85901", zipcode),
    zipcode = if_else(id == "770039781", "86336", zipcode),
    zipcode = if_else(id == "770039481" | id == "770040157",
      "85925", zipcode
    ),
    zipcode = if_else(id == "770039899" | id == "770039783",
      "86001", zipcode
    ),
    id = if_else(id == "770039554", "7700237255", id),
    id = if_else(id == "770039639", "770040368", id),
    id = if_else(id == "770040370" | id == "770040369", "770039702", id),
    id = if_else(id == "770040394", "770040395", id),
    id = if_else(id == "770040123", "7700206366", id),
    id = if_else(id == "770040396", "770040397", id),
    id = if_else(id == "770040077", "7700237271", id),
    id = if_else(id == "770039484", "770039485", id),
    address_line = if_else(id == "7700237255",
      "10 s 6th st cottonwood az 86326", address_line
    ),
    address_line = if_else(id == "770039550",
      "yavapai county administration building",
      address_line
    ),
    address_line = if_else(id == "770039620",
      "11711 williams st wellton az 85356",
      address_line
    ),
    address_line = if_else(id == "770040357",
      "1380 e patagonia hwy nogales az 85621",
      address_line
    ),
    address_line = if_else(id == "770039624",
      "v-10 road  az", address_line
    ),
    address_line = if_else(id == "770040018",
      "1478 queen valley dr queen valley az 85118",
      address_line
    ),
    address_line = if_else(id == "8850012015",
      "16402 n fort mcdowell rd fort mcdowell az 85264",
      address_line
    ),
    address_line = if_else(id == "770039640",
      "240 canal street somerton az 85350",
      address_line
    ),
    address_line = make_clean_names(address_line,
      sep_out = " ",
      unique_sep = NULL
    ),
    address_line = sub("_.*", "", address_line),
    name = make_clean_names(name, sep_out = " ", unique_sep = NULL),
    name = sub("_.*", "", name),
    address_line = as.character(gsub(" s ", " south ", address_line)),
    address_line = as.character(gsub(" n ", " north ", address_line)),
    address_line = as.character(gsub(" w ", " west ", address_line)),
    address_line = as.character(gsub(" e ", " east ", address_line)),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line))
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% az_cos$zip) == TRUE) %>%
  verify(n_distinct(id, address_line) == 469) %>%
  verify(nrow(.) == 469 & ncol(.) == 4) %>%
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
      delta_n_places > 0 ~ "Gained Polling Locations"
    )
  ) %>%
  arrange(delta_n_places) %>%
  select(zipcode, n_pp_2020, n_pp_2016, delta_n_places, delta_cat) %>%
  verify(ncol(.) == 5 & nrow(.) == 284)

# SC ---------------------------------------------------------------------------
sc_2016_df <- pluck(read_rds(inputs$VIPinlist_imp), 4) %>%
  verify(ncol(.) == 3 & nrow(.) == 2194) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(zipcode = as.character(substr(
    address_line,
    nchar(address_line) - n_last + 1,
    nchar(address_line))),
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
      address_line
    ),
    address_line = if_else(id == "8880108",
      "1001 longtown rd columbia sc 29229",
      address_line
    ),
    id = if_else(id == "8880802", "8880955", id),
    id = if_else(id == "88801932", "88802078", id),
    address_line = if_else(id == "88802078",
      "110 winter st winnsboro sc 29180",
      address_line
    ),
    id = if_else(id == "8880745", "88801163", id),
    address_line = if_else(id == "88801163",
      "1139 hillsboro rd orangeburg sc 29118",
      address_line
    ),
    address_line = if_else(id == "8880140",
      "2001 spann rd batesburg sc 29006",
      address_line
    ),
    address_line = if_else(id == "8880203",
      "201 school house ln summerville sc 29486",
      address_line
    ),
    address_line = if_else(id == "8880990",
      "2345 state highway 41/51 s hemingway sc 29554",
      address_line
    ),
    id = if_else(id == "88801752", "88801333", id),
    id = if_else(id == "88801582", "8880924", id),
    id = if_else(id == "88801220", "8880333", id),
    address_line = as.character(gsub(" s ", " south ", address_line)),
    address_line = as.character(gsub(" n ", " north ", address_line)),
    address_line = as.character(gsub(" w ", " west ", address_line)),
    address_line = as.character(gsub(" e ", " east ", address_line)),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line)),
    zipcode = if_else(zipcode == "28138", "29138", zipcode),
    zipcode = as.character(zipcode)
  ) %>%
  filter(zipcode != "SC") %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify(zipcode %in% sc_cos$zip) %>%
  verify(n_distinct(id, address_line) == 2066) %>%
  verify(nrow(.) == 2066 & ncol(.) == 4) %>%
  arrange(address_line)

sc_zips_freq_2016 <- as.data.frame(table(sc_2016_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2016 = as.numeric(Freq)
  ) %>%
  filter(zipcode != "SC") %>%
  verify(zipcode != "SC") %>%
  verify(ncol(.) == 4 & nrow(.) == 376)

# Summerville is now in zipcode 29483 not 29486
summerville <- c(
  "88802641", "88802496", "88802274", "88801273",
  "88801246", "88801201", "8880203"
)

sc_2020_df <- pluck(read_rds(inputs$VIPinlist_imp), 5) %>%
  verify(ncol(.) == 3 & nrow(.) == 1968) %>%
  verify(min(id) == 88801000 & max(id) == 8880999) %>%
  mutate_at(c("id", "name", "address_line"), as.character) %>%
  mutate(
    zipcode =
      as.character(substr(
        address_line, nchar(address_line) - n_last + 1,
        nchar(address_line))),
    address_line = tolower(address_line),
    name = tolower(name),
    zipcode = as.character(if_else(zipcode == "40 SC", "29840", zipcode)),
    zipcode = if_else(id == "88801220", "29410", zipcode),
    zipcode = if_else(id == "88802414", "29036", zipcode),
    zipcode = if_else(id == "8880132", "29302", zipcode),
    zipcode = if_else(id == "88801207", "29849", zipcode),
    zipcode = if_else(id %in% summerville, "29483", zipcode),
    id = if_else(id == "8880960", "88802013", id),
    id = if_else(id == "88801932", "88802078", id),
    address_line = if_else(id == "88802078",
      "110 winter st winnsboro sc 29180",
      address_line
    ),
    id = if_else(id == "8880768", "	88802203", id),
    id = if_else(id == "8880745", "88801163", id),
    address_line = if_else(id == "88801163",
      "1139 hillsboro rd orangeburg sc 29118",
      address_line
    ),
    id = if_else(id == "88801440", "88802442", id),
    id = if_else(id == "8880189", "88801108", id),
    address_line = if_else(id == "88801693",
      "13255 ashevelle hwy inman sc 29349",
      address_line
    ),
    id = if_else(id == "88801893", "88802149", id),
    address_line = if_else(id == "88801067",
      "200 powe st cherawd sc 29520",
      address_line
    ),
    id = if_else(id == "88801434", "88802218", id),
    id = if_else(id == "88802030", "88801168", id),
    id = if_else(id == "88802100", "88801710", id),
    id = if_else(id == "8880672", "88801930", id),
    address_line = as.character(gsub(" s ", " south ", address_line)),
    address_line = as.character(gsub(" n ", " north ", address_line)),
    address_line = as.character(gsub(" w ", " west ", address_line)),
    address_line = as.character(gsub(" e ", " east ", address_line)),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line))
  ) %>%
  filter(zipcode != "SC" | zipcode != "40 sc") %>%
  distinct(id, .keep_all = TRUE) %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify(is.na(zipcode) == FALSE) %>%
  verify((zipcode %in% sc_cos$zip)) %>%
  verify(n_distinct(address_line) == 1919) %>%
  verify(nrow(.) == 1919 & ncol(.) == 4) %>%
  arrange(address_line)

sc_zips_freq_2020 <- as.data.frame(table(sc_2020_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2020 = as.numeric(Freq)
  ) %>%
  filter(zipcode != "SC") %>%
  verify(zipcode != "SC") %>%
  verify(ncol(.) == 4 & nrow(.) == 378)

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
  verify(ncol(.) == 5 & nrow(.) == 380)

# PA data from state election officials ----------------------------------------

# Invalid zip codes in PA data
odd_zips <- c("0", "1", "13235", "1910", "191", "190", "191119", "194")

pa_2016_df <- read_rds(inputs$pa_2016_imp) %>%
  clean_names() %>%
  mutate(
    street = tolower(if_else(street == "OLD ROUTE  30", "OLD ROUTE 30", street)),
    name = as.character(make_clean_names(description,
      sep_out = " ",
      unique_sep = NULL
    )),
    name = as.character(sub("_.*", "", name)),
    comment = tolower(comment),
    prefix_direction_desc = tolower(prefix_direction_desc),
    street_type_desc = tolower(street_type_desc),
    city = tolower(city),
    line2 = tolower(line2),
    zipcode = as.character(postal_code),
    county = as.character(snakecase::to_any_case(county_name,
      case = "big_camel"
    ))
  ) %>%
  select(county, name, house_num, street, street_type_desc, city, zipcode) %>%
  unite("address_line", house_num:zipcode, remove = FALSE, sep = " ") %>%
  drop_na(zipcode) %>%
  mutate(
    address_line = as.character(make_clean_names(address_line,
      sep_out = " ",
      unique_sep = NULL
    )),
    address_line = as.character(sub("_.*", "", address_line)),
    address_line = as.character(gsub(" na ", " ", address_line)),
    name = as.character(gsub(" na ", " ", name)),
    address_line = as.character(gsub("na ", " ", address_line)),
    name = as.character(gsub("na ", "", name)),
    address_line = as.character(gsub(" na", " ", address_line)),
    name = as.character(gsub(" na", "", name)),
    address_line = as.character(gsub(
      " pitsburgh ", " pittsburgh ",
      address_line
    )),
    address_line = as.character(gsub(
      " altoo ", " altoona ",
      address_line
    )),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line)),
    address_line = if_else(address_line == "x982 chestnut st ext derry 15627",
      "x982 chestnut st derry 15627", address_line
    ),
    name = if_else(name == "st marks ev ch miesel hall",
      "st marks evan church", name
    ),
    address_line = if_else(
      address_line == "x933 briookline blvd pittsburgh 15226",
      "x933 brookline blvd pittsburgh 15226", address_line
    ),
    address_line = if_else(
      address_line == "x915 linc ave west chester 19380",
      "x915 lincoln ave west chester 19380", address_line
    ),
    address_line = if_else(
      address_line == "x91 commonwealth dr west mifflin 15122",
      "x91 commonwealth ave west mifflin 15122", address_line
    ),
    address_line = if_else(address_line == "x900 haslage ave liberty 15133",
      "x900 haslage st mckeesport 15133", address_line
    ),
    address_line = if_else(
      address_line == "x900 desdemont ave pittsburgh 15210",
      "x900 desmont ave pittsburgh 15210", address_line
    ),
    address_line = if_else(
      address_line == "x889 milledgeville rd sandy lake 16145",
      "x889 milledgeville rd hadley 16130", address_line
    ),
    address_line = if_else(
      address_line == "x8800 peebles rd pittsburgh 15137",
      "x8800 peebles rd allison park 15101", address_line
    ),
    address_line = if_else(
      address_line == "x860 colonial manor large bldg rd n huntingdon 15642",
      "x860 colonial manor rd n huntingdon 15642", address_line
    ),
    address_line = if_else(
      address_line == "x850 cranberry woods rd sewickley 15066",
      "x850 cranberry woods rd warrendale 16066", address_line
    ),
    address_line = if_else(
      address_line == "x8711 old perry hwy pittsburgh 15237",
      "x8711 old perry hwy way mccandless 15237", address_line
    ),
    address_line = if_else(address_line == "x800 coopertown rd bryn mawr 19010",
      "x800 coopertown rd haverford 19041", address_line
    ),
    address_line = if_else(
      address_line == "x7605 saltsburg st pittsburgh 15239",
      "x7605 saltsburg rd pittsburgh 15239", address_line
    ),
    address_line = if_else(address_line == "x751 sugar run rd altoona 16602",
      "x751 sugar run rd altoona 16601", address_line
    ),
    address_line = if_else(
      address_line == "x718 wallace ave wilkinsburg 15221",
      "x718 wallace ave pittsburgh 15221", address_line
    ),
    address_line = if_else(
      address_line == "x700 york mitchell ave lansdale 19446",
      "x700 york ave lansdale 19446", address_line
    ),
    address_line = if_else(
      address_line == "x695 freeeport rd freeport 16229",
      "x695 freeport rd freeport 16229", address_line
    ),
    address_line = if_else(address_line == "x660 noble rd west mifflin 15122",
      "x660 noble dr west mifflin 15122", address_line
    ),
    address_line = if_else(
      address_line == "x6051 west chester park newtown square 19073",
      "x6051 west chester pike newtown square 19073", address_line
    ),
    address_line = if_else(address_line == "x602 ingomar rd sewickley 15090",
      "x602 ingomar rd pittsburgh 15237", address_line
    ),
    address_line = if_else(address_line == "x60 ganaldo rd pittsburgh 15108",
      "x60 gawaldo dr coraopolis 15108", address_line
    ),
    address_line = if_else(
      address_line == "x5901 library hgts rd bethel park 15102",
      "x5901 library rd bethel park 15102", address_line
    ),
    address_line = if_else(address_line == "x555 broadway ave stowe 15136",
      "x555 broadway ave mckees rocks 15136", address_line
    ),
    address_line = if_else(
      address_line == "x551 ravensburg blvd pittsburgh 15025",
      "x551 ravensburg blvd clairton 15025", address_line
    ),
    address_line = if_else(address_line == "x537 bayne ave bellevue 15202",
      "x537 bayne ave pittsburgh 15202", address_line
    ),
    address_line = if_else(
      address_line == "x525 pleasant hill rd marshall 15090",
      "x525 pleasant hill rd wexford 15090", address_line
    ),
    address_line = if_else(
      address_line == "x5145 wexford run rd marshall 15090",
      "x5145 wexford run rd wexford 15090", address_line
    ),
    address_line = if_else(
      address_line == "x503 speer st n belle vern 15012",
      "x503 speer st belle vernon 15012", address_line
    ),
    address_line = if_else(address_line == "x480 ridge rd pottstown 19465",
      "x480 ridge rd spring city 19475", address_line
    ),
    address_line = if_else(address_line == "x4565 prestwick dr reading 19601",
      "x4565 prestwick dr reading 19606", address_line
    ),
    address_line = if_else(address_line == "x456 first st heidelberg 15106",
      "x456 first st carnegie 15106",
      address_line
    ),
    address_line = if_else(
      address_line == "x423 allegheny st h0llidaysburg 16648",
      "x423 allegheny st hollidaysburg 16648",
      address_line
    ),
    address_line = if_else(address_line == "x417 lincoln ave walnutport 18035",
      "x417 lincoln ave walnutport 18088",
      address_line
    ),
    address_line = if_else(
      address_line == "x415 three sq hollow rd newburg 17240",
      "x415 three square hollow rd newburg 17240", address_line
    ),
    address_line = if_else(
      address_line == "x415 brennan ave loyalhanna 15661",
      "x415 brennan ave latrobe 15650",
      address_line
    ),
    address_line = if_else(
      address_line == "x414 railroad st pittsburgh 15028" |
        address_line == "x414 railroad st coulters 15028",
      "x414 railroad st white oak 15131",
      address_line
    ),
    address_line = if_else(address_line == "x411 babylon rd horsham 19002",
      "x411 babylon rd horsham 19044",
      address_line
    ),
    address_line = if_else(address_line == "x409 ashland ave primos 19018",
      "x409 ashland ave secane 19018",
      address_line
    ),
    address_line = if_else(
      address_line == "x403 fox chapel rd fox chapel 15238",
      "x403 fox chapel rd pittsburgh 15238", address_line
    ),
    address_line = if_else(
      address_line == "x401 washington rd mt lebanon 15228",
      "x401 washington rd pittsburgh 15228", address_line
    ),
    address_line = if_else(address_line == "x400 sproul rd springield 19064",
      "x400 sproul rd springfield 19064", address_line
    ),
    address_line = if_else(address_line == "x4 walnut st dunlevy 15342",
      "x4 walnut st dunlevy 15432", address_line
    ),
    address_line = if_else(
      address_line == "x3916 old wm penn murrysville 15668",
      "x3916 old william penn hwy murrysville 15668", address_line
    ),
    address_line = if_else(address_line == "x373 beaver rd leetsdale 15056",
      "x373 beaver st leetsdale 15056", address_line
    ),
    address_line = if_else(
      address_line == "x3710 saxonburg blvd indianola 15051",
      "x3710 saxonburg blvd indiana 15051", address_line
    ),
    address_line = if_else(
      address_line == "x3640 old oakdale rd mc donald 15057",
      "x3640 old oakdale rd mcdonald 15057", address_line
    ),
    address_line = if_else(
      address_line == "x349 magee rd sewickley hl 15143",
      "x349 magee rd sewickley 15143", address_line
    ),
    address_line = if_else(
      address_line == "x3454 pleasantvue dr baldwin br 15227",
      "x3454 pleasantvue dr pittsburgh 15227", address_line
    ),
    address_line = if_else(
      address_line == "x3436 windchester rd allentown 18104",
      "x3436 winchester rd allentown 18104", address_line
    ),
    address_line = if_else(
      name == "kerr elementary school multi purp" &
        address_line != "x341 kittanning pike pittsburgh 15215",
      "x341 kittanning pike pittsburgh 15215", address_line
    ),
    address_line = if_else(address_line == "x330 hill top rd hummelstown
                                17036", "x330 hilltop rd hummelstown 17036",
      address_line
    ),
    address_line = if_else(
      address_line == "x3260 queneshukney rd linden 17744",
      "x3260 queneshukeny rd linden 17744", address_line
    ),
    address_line = if_else(
      address_line == "x325 nicholson rd pittsburgh 15143",
      "x325 nicholson rd sewickley 15143", address_line
    ),
    address_line = if_else(
      address_line == "x325 fox chapel rd rd pittsburgh 15215",
      "x325 fox chapel rd pittsburgh 15238", address_line
    ),
    address_line = if_else(
      address_line == "x320 fairies st wilcox 15870",
      "x320 faries st wilcox 15870", address_line
    ),
    address_line = if_else(
      address_line == "x315 susquehanst lancaster 17602",
      "x315 susquehanna st lancaster 17602",
      address_line
    ),
    address_line = if_else(address_line == "x3101 mccully rd hampton 15101",
      "x3101 mccully rd allison park 15101",
      address_line
    ),
    address_line = if_else(address_line == "x310 middleton rd carlisle 17013",
      "x310 n middleton rd carlisle 17013",
      address_line
    ),
    address_line = if_else(
      address_line == "x306 st james ln n alexandria 15670",
      "x306 st james ln new alexandria 15670", address_line
    ),
    address_line = if_else(
      address_line == "x306 bessemer st e pittsburgh 15112",
      "x306 bessemer ave e pittsburgh 15112", address_line
    ),
    address_line = if_else(
      address_line == "x3054 howes run rd natroheights 15065",
      "x3054 howes run rd tarentum 15084", address_line
    ),
    address_line = if_else(
      address_line == "x301 station ave wilmerding 15148",
      "x301 station st wilmerding 15148",
      address_line
    ),
    address_line = if_else(
      address_line == "x30 charter way wilkes barre 18702",
      "x30 charter school way wilkes barre 18702",
      address_line
    ),
    address_line = if_else(
      address_line == "x3 keystone cmns white haven 18661",
      "x3 keystone commons white haven 18661",
      address_line
    ),
    address_line = if_else(
      address_line == "x280 normal s green steast stroudsburg 18301",
      "x280 normal s green st e stroudsburg 18301", address_line
    ),
    address_line = if_else(
      address_line == "x26 patterson st rlansford 18232",
      "x26 patterson st lansford 18232", address_line
    ),
    address_line = if_else(
      address_line == "x2565 susquehanna ave abington 19001",
      "x2565 susquehanna rd abington 19001", address_line
    ),
    address_line = if_else(
      address_line == "x2425 new falls rd newportville 19056",
      "x2425 new falls rd levittown 19054", address_line
    ),
    address_line = if_else(
      address_line == "x2415 allequippa st pittsburgh 152131",
      "x2415 allequippa st pittsburgh 15213", address_line
    ),
    address_line = if_else(address_line == "x241 maple ave edgewood 15221",
      "x241 maple ave pittsburgh 15218",
      address_line
    ),
    address_line = if_else(address_line == "x2344 ingomar rd wexford 15237",
      "x2344 ingomar rd pittsburgh 15237",
      address_line
    ),
    address_line = if_else(
      address_line == "x2341 delaware dr mount bethel 18434",
      "x2341 delaware dr mount bethel 18343", address_line
    ),
    address_line = if_else(
      address_line == "x2336 brownsville rd ent rd pittsburgh 15210",
      "x2336 brownsville rd pittsburgh 15210", address_line
    ),
    address_line = if_else(address_line == "x230 boquet st pittsburgh 15213",
      "x230 bouquet st pittsburgh 15213",
      address_line
    ),
    address_line = if_else(
      address_line == "x227 meetinghouse rd horsham 19044",
      "x227 meeting house rd horsham 19044", address_line
    ),
    address_line = if_else(
      address_line == "x2200 haines rd levittown 19054",
      "x2200 haines rd levittown 19055", address_line
    ),
    address_line = if_else(
      address_line == "x2200 cornerstone ln delmont 15626",
      "x2200 cornerstone ln export 15632", address_line
    ),
    address_line = if_else(
      address_line == "x211 sweet briar st pittsburgh 15211",
      "x211 sweetbriar st pittsburgh 15211", address_line
    ),
    address_line = if_else(
      address_line == "x2055 bedford ave pittsburgh 151219",
      "x2055 bedford ave pittsburgh 15219", address_line
    ),
    address_line = if_else(address_line == "x2025 lincoln rd 15147",
      "x2025 lincoln rd penn hills 15147",
      address_line
    ),
    address_line = if_else(address_line == "x2021 20 st erie 16510",
      "x2021 20th st erie 16510", address_line
    ),
    address_line = if_else(address_line == "x201 summit stnorth wales 19454",
      "x201 summit st n wales 19454", address_line
    ),
    address_line = if_else(
      address_line == "x201 center new texas roadpittsburgh 15239",
      "x201 center new texas rd pittsburgh 15239", address_line
    ),
    address_line = if_else(
      address_line == "x2000 ashbourne rd washington lnelkins park 19027",
      "x2000 ashbourne washington ln rd elkins park 19027", address_line
    ),
    address_line = if_else(address_line == "x194 donahoe rd greensburg 15601",
      "x194 donohoe rd greensburg 15601",
      address_line
    ),
    address_line = if_else(
      address_line == "x1919 8th st bethlehem 18017",
      "x1919 8th st bethlehem 18020", address_line
    ),
    address_line = if_else(
      address_line == "x1807 state rte 268east brady 16028",
      "x1807 state rte 268 e brady 16028", address_line
    ),
    address_line = if_else(
      address_line == "x1801 paper mill rd oreland 19075",
      "x1801 paper mill rd glenside 19038", address_line
    ),
    address_line = if_else(
      address_line == "x1800 7th st rd new kensingtn 15068",
      "x1800 7th st rd new kensington 15068", address_line
    ),
    address_line = if_else(
      address_line == "x175 green la philadelphia 19127",
      "x175 green ln philadelphia 19127", address_line
    ),
    address_line = if_else(
      address_line == "x17074 crogan pike shirleysburg 17260",
      "x17074 croghan pike shirleysburg 17260", address_line
    ),
    address_line = if_else(
      address_line == "x1701 15th st philadelphia 19146",
      "x1701 15th st philadelphia 19145", address_line
    ),
    address_line = if_else(
      address_line == "x166 village dr williamsport 17702",
      "x166 village dr s williamsport 17702", address_line
    ),
    address_line = if_else(
      name == "eisenhower middle school",
      "x1601 markley st norristown 19401", address_line
    ),
    address_line = if_else(
      address_line == "x1414 beaver st osborne 15143",
      "x1414 beaver st sewickley 15143", address_line
    ),
    address_line = if_else(
      name == "barbush langloth muni corp by langloth ballfield",
      "x554 5th ave langeloth 15054",
      address_line
    ),
    name = as.character(
      if_else(address_line == "x136 community building rd leechburg 15656",
        "allegheny twp community building",
        name
      )
    ),
    address_line = if_else(address_line ==
      "x1350 fulling millmiddletown 17057",
    "x1350 fulling mill rd middletown 17057",
    address_line
    ),
    address_line = if_else(address_line == "x998 lloyd st nty glo ",
      "x998 lloyd st nty glo 15943",
      address_line
    ),
    address_line = if_else(
      address_line == "x9722 cumberland hwy pleasant hall ",
      "x9722 cumberland hwy pleasant hall 17246",
      address_line
    ),
    address_line = if_else(address_line == "x965 burtner rd harrison ",
      "x965 burtner rd natrona heights 15065",
      address_line
    ),
    address_line = if_else(address_line == "x93 jones st lilly",
      "x93 jones st lilly 15938",
      address_line
    ),
    address_line = if_else(address_line == "x927 johnston st philadelphia",
      "x927 johnston st philadelphia 19148",
      address_line
    ),
    address_line = if_else(address_line == "x927 freeport rd e deer",
      "x927 freeport rd creighton 15030",
      address_line
    ),
    address_line = if_else(address_line == "x9244 five forks rd waynesboro",
      "x9244 five forks rd waynesboro 17268",
      address_line
    ),
    address_line = if_else(address_line == "x915 linc ave w chester 19380",
      "x915 lincoln ave w chester 19380",
      address_line
    ),
    address_line = if_else(address_line == "x9019 roberts hollow rd forward",
      "x9019 roberts hollow rd elizabeth 15037",
      address_line
    ),
    address_line = if_else(address_line == "x900 potomac st waynesboro",
      "x900 potomac st waynesboro 17268",
      address_line
    ),
    address_line = if_else(address_line == "x890 fowlers hollow rd blain",
      "x890 fowlers hollow rd blain 17006",
      address_line
    ),
    address_line = if_else(address_line == "x8800 peebles rd mccandless",
      "x8800 peebles rd allison park 15101",
      address_line
    ),
    address_line = if_else(
      address_line == "x8770 possum hollow rd shippensburg",
      "x8770 possum hollow rd shippensburg 17257",
      address_line
    ),
    address_line = if_else(address_line == "x8724 crispin dr philadelphia",
      "x8724 crispin st philadelphia",
      address_line
    ),
    address_line = if_else(address_line == "x8650 mcclays mill rd newburg",
      "x8650 mcclays mill rd newburg 17240",
      address_line
    ),
    address_line = if_else(
      address_line == "x860 colonial manor rd                                 n huntingdon 15642",
      "x860 colonial manor rd n huntingdon 15642",
      address_line
    ),
    address_line = if_else(address_line == "x825 poplar st greentree",
      "x825 poplar st pittsburgh 15220", address_line
    ),
    address_line = if_else(address_line == "x810 orchard dr chambersburg",
      "x810 orchard dr chambersburg 17201",
      address_line
    ),
    address_line = if_else(address_line == "x8098 anthony hwy waynesboro",
      "x8098 anthony hwy waynesboro 17268",
      address_line
    ),
    address_line = if_else(address_line == "x809 main st stroudsburg",
      "x809 main st stroudsburg 18360",
      address_line
    ),
    address_line = if_else(address_line == "x803 st augustine rd dysart",
      "x803 st augustine rd dysart 16636",
      address_line
    ),
    address_line = if_else(address_line == "x803 market st ex mckeesport",
      "x803 market st mckeesport",
      address_line
    ),
    address_line = if_else(address_line == "x797 thompson run rd ross ",
      "x797 thompson run rd pittsburgh 15237",
      address_line
    ),
    address_line = if_else(address_line == "x7911 wm penn hwy cresson",
      "x7911 wm penn hwy cresson 16630",
      address_line
    ),
    address_line = if_else(
      address_line == "x423 allegheny st h0llidaysburg 16648",
      "x423 allegheny st hollidaysburg 16648",
      address_line
    ),
    address_line = if_else(address_line == "x764 beverly dr w mifflin",
      "x764 beverly dr w mifflin 15122",
      address_line
    ),
    address_line = if_else(address_line == "x7600 evans st swissvale",
      "x7600 evans st pittsburgh 15218",
      address_line
    ),
    address_line = if_else(
      address_line == "x741 coldbrook ave chambersburg",
      "x741 coldbrook ave chambersburg 17201",
      address_line
    ),
    address_line = if_else(address_line == "x736 railroad ave vero",
      "x736 railroad ave vero 15147",
      address_line
    ),
    address_line = if_else(
      address_line == "x7301 germantown ave philadelphia",
      "x7301 germantown ave philadelphia 19119",
      address_line
    ),
    address_line = if_else(address_line == "x730 bloom st johnstown",
      "x730 bloom st johnstown 15902", address_line
    ),
    address_line = if_else(address_line == "x7289 ruritan dr chambersburg",
      "x7289 ruritan dr chambersburg 17202",
      address_line
    ),
    address_line = if_else(address_line == "x728 ben franklin hwy ebensburg",
      "x728 ben franklin hwy ebensburg 15931",
      address_line
    ),
    address_line = if_else(address_line == "x7219 church ave ben avon",
      "x7219 church ave pittsburgh 15202",
      address_line
    ),
    address_line = if_else(address_line == "x721 california ave avalon",
      "x721 california ave pittsburgh 15202",
      address_line
    ),
    address_line = if_else(address_line == "x951 1st st coraopolis",
      "x951 1st ave coraopolis 15108",
      address_line
    ),
    address_line = if_else(address_line == "x901 n blvd" |
      address_line == "x901 n blvd bethlehem",
    "x901 n blvd bethlehem 18017",
    address_line
    ),
    address_line = if_else(address_line == "x8800 peebles rd mccandless",
      "x8800 peebles rd allison park 15101",
      address_line
    ),
    address_line = if_else(address_line == "x803 market st ex mckeesport" |
      address_line == "x803 market st mckeesport",
    "x803 market st mckeesport 15132",
    address_line
    ),
    address_line = if_else(
      address_line == "x801 allegehny st h0llidaysburg 16648",
      "x801 allegehny st hollidaysburg 16648",
      address_line
    ),
    address_line = if_else(address_line == "x7745 tioga st pittsburgh",
      "x7745 tioga st pittsburgh 15208", address_line
    ),
    address_line = if_else(address_line == "x2300 morton rd up st clair",
      "x2300 morton rd pittsburgh 15241",
      address_line
    ),
    address_line = if_else(address_line == "x15 ible run rd shaler",
      "x15 wible run rd shaler",
      address_line
    ),
    address_line = if_else(address_line == "x1514 norcross erie 16509",
      "x1514 norcross rd erie 16509",
      address_line
    ),
    address_line = if_else(
      address_line == "x5 nov blue mountain dr cherryville 18035",
      "x675 blue mountain dr cherryville 18035",
      address_line
    ),
    address_line = if_else(address_line == "x2 race st edgewood",
      "x2 race st edgewood 15218",
      address_line
    ),
    address_line = if_else(name == "boys girls club of allentown",
      "x720 n 6th st allentown 18102", address_line
    ),
    address_line = if_else(address_line == "x1500 burchfield rd shaler",
      "x1500 burchfield rd 15101", address_line
    ),
    address_line = if_else(address_line == "x315 shady ave pittsburgh",
      "x315 shady ave pittsburgh 15206", address_line
    ),
    address_line = if_else(address_line == "x220 37th st pittsburgh",
      "x220 37th st pittsburgh 15224", address_line
    ),
    address_line = if_else(address_line == "x301 franklin ave carnegie",
      "x301 franklin ave carnegie 15106", address_line
    ),
    address_line = if_else(
      name == "cheltenham school admin building" &
        address_line != "x2000 ashbourne rd elkins park 19027",
      "x2000 ashbourne rd elkins park 19027", address_line
    ),
    address_line = if_else(
      address_line == "x551 ravensburg blvd clairton                                 15025",
      "x551 ravensburg blvd clairton 15025", address_line
    ),
    address_line = if_else(address_line == "main rd bedford",
      "main rd bedford 15522", address_line
    ),
    address_line = if_else(address_line == "x951 1st st coraopolis",
      "x951 1st ave coraopolis 15108",
      address_line
    ),
    address_line = if_else(
      address_line == "x539 chicora st mckeesport 15035",
      "x539 chicora st e mc keesport 15035", address_line
    ),
    address_line = if_else(
      address_line == "x8448 jonestown rd grantville 17028",
      "x8848 jonestown rd grantville 17028", address_line
    ),
    address_line = if_else(address_line == "x1801 colonial rd harrisburg",
      "x1801 colonial rd harrisburg 17112", address_line
    ),
    address_line = if_else(address_line == "x1606 sullivan easton",
      "x1606 sullivan trail easton 18040", address_line
    ),
    address_line = if_else(address_line == "x715 universal rd penn hills",
      "x715 universal rd pittsburgh 15235", address_line
    ),
    address_line = if_else(address_line == "x620 6th st n braddock",
      "x620 6th st n braddock 15104", address_line
    ),
    address_line = if_else(
      address_line == "x542 otterman st greensburg 15601",
      "x542 w otterman st greensburg 15601", address_line
    ),
    address_line = if_else(
      address_line == "vine st penn hills", "vine st pittsburgh 15235",
      address_line
    ),
    address_line = if_else(
      address_line == "x3710 saxonburg blvd india 15051",
      "x3710 saxonburg blvd indiana 15051", address_line
    ),
    address_line = if_else(
      address_line == "x26 patterson st r lansford 18232",
      "x26 patterson st lansford 18232", address_line
    ),
    address_line = if_else(
      address_line == "x13 nov robinson ave",
      "x13 nov robinson ave pen argyl 18072", address_line
    ),
    address_line = if_else(
      address_line == "x11 castle shanon blvd mt lebanon",
      "x11 castle shannon blvd mt lebanon 15228", address_line
    ),
    address_line = if_else(
      address_line == "x8800 peebles rd mccandless",
      "x8800 peebles rd allison park 15101", address_line
    ),
    address_line = if_else(
      address_line == "x6502 lilac st pittsburgh",
      "x6502 lilac st pittsburgh 15217", address_line
    ),
    address_line = if_else(address_line == "x33 lonsdale st reserve",
      "x33 lonsdale st pittsburgh 15212", address_line
    ),
    address_line = if_else(address_line == "x440 monogahela ave glassport",
      "x440 monongahela ave glassport", address_line
    ),
    address_line = if_else(address_line == "x1500 lincoln ave scranton 18504",
      "x1500 n lincoln ave scranton 18504",
      address_line
    ),
    address_line = if_else(address_line == "tioga st tunhannock",
      "tioga st tunkhannock 18657", address_line
    ),
    address_line = if_else(address_line == "rr 2 box 2914",
      "rr 2 box 2914 laceyville", address_line
    ),
    address_line = if_else(address_line == "x2375 levans rd coplay 18036",
      "x2375 levans rd coplay 18037",
      address_line
    ),
    address_line = if_else(address_line == "x419 maryland ave oakmont",
      "x419 maryland ave oakmont 15139",
      address_line
    ),
    address_line = if_else(address_line == "x1000 lindsay rd scott 15106",
      "x1000 lindsay rd carnegie 15106",
      address_line
    ),
    address_line = if_else(address_line == "x124 morgan st brackenridge",
      "x124 morgan brackenridge 15014",
      address_line
    ),
    address_line = if_else(address_line == "x2601 norwood ave pittsburgh",
      "x2601 norwood ave pittsburgh 15214",
      address_line
    ),
    address_line = if_else(address_line == "x2601 norwood ave pittsburgh",
      "x2601 norwood ave pittsburgh 15214",
      address_line
    ),
    address_line = if_else(address_line == "x930 main st rockwood 15557",
      "x630 main st rockwood 15557",
      address_line
    ),
    address_line = if_else(address_line == "x736 railroad ave vero",
      "x736 railroad ave vero 15147",
      address_line
    ),
    address_line = if_else(name == "rosedale vfd social hall",
      "x5806 verona rd penn hills 15147",
      address_line
    ),
    address_line = if_else(name == "roslyn elementary school",
      "x2565 susquehanna rd abington 19001",
      address_line
    ),
    address_line = if_else(address_line == "x1926 sarah st pittsburgh",
      "x1926 sarah st pittsburgh 15203",
      address_line
    ),
    address_line = if_else(address_line == "x901 n blvd" |
      address_line == "x901 n blvd bethlehem",
    "x901 n blvd bethlehem 18017",
    address_line
    ),
    address_line = if_else(address_line == "x3198 schieck st baldwin br",
      "x3198 schieck st baldwin br 15227",
      address_line
    ),
    address_line = if_else(address_line == "x509 dallas ave pittsburgh",
      "x509 dallas ave pittsburgh 15208",
      address_line
    ),
    address_line = if_else(
      address_line == "x1550 lincoln ave ave pittsburgh 15206",
      "x1550 lincoln ave pittsburgh 15206", address_line
    ),
    address_line = if_else(
      address_line == "x1550 lincoln ave pittsburgh 15208",
      "x1550 lincoln ave pittsburgh 15206", address_line
    ),
    address_line = if_else(
      address_line == "x519 58th st altoona 16601",
      "x519 58th st altoona 16602", address_line
    ),
    address_line = if_else(address_line == "x418 unity center rd plum",
      "x418 unity center rd pittsburgh 15239",
      address_line
    ),
    address_line = if_else(address_line == "x120 boggs ave pittsburgh",
      "x120 boggs ave pittsburgh 15211",
      address_line
    ),
    address_line = if_else(address_line == "x600 pitt st wilkinsburg",
      "x600 pitt st pittsburgh 15221",
      address_line
    ),
    address_line = if_else(address_line == "x1705 maple st homestead",
      "x1705 maple st homestead 15120",
      address_line
    ),
    address_line = if_else(
      address_line == "x1315 marshall selma st norristown 19401",
      "x1315 marshall st norristown 19401", address_line
    ),
    address_line = if_else(address_line == "x1833 laketon rd wilkinsburg",
      "x1833 laketon rd wilkinsburg 15221", address_line
    ),
    address_line = if_else(name == "west homestead vfd",
      "x447 w 8th ave w homestead pa 15120",
      address_line
    ),
    address_line = if_else(address_line == "x1228 brinkerton rd greensburg 156010",
      "x1228 brinkerton rd greensburg 15601", address_line
    ),
    address_line = if_else(address_line == "x12 fire rd eldersville 15036",
      "x12 fire rd eldersville 15021", address_line
    ),
    address_line = if_else(address_line == "x4628 upper rd gowen city 17828",
      "x4628 upper rd gowen city 17872", address_line
    ),
    address_line = if_else(address_line == "x900 packer ave philadelphia 1914",
      "x900 packer ave philadelphia 19148", address_line
    ),
    address_line = if_else(address_line == "x339 jamestown rd greenville 16152",
      "x339 e jamestown rd greenville 16125", address_line
    ),
    address_line = if_else(address_line == "  clermont 16722",
      "x14183 wilcox clermont rd wilcox 15870", address_line
    ),
    address_line = if_else(address_line == "x5177 nuangola rd nuangola 18637",
      "x5177 nuangola rd nuangola 18707", address_line
    ),
    address_line = if_else(address_line == "x109 german erie 16597",
      "x109 german erie 16507", address_line
    )
  ) %>%
  filter(!zipcode %in% odd_zips) %>%
  mutate(zipcode = as.character(substr(
    address_line, nchar(address_line) - n_last + 1, nchar(address_line)
  ))) %>%
  select(name, address_line, zipcode) %>%
  filter(name != "election hse next to ed green") %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify((zipcode %in% pa_cos$zip) == TRUE) %>%
  verify(n_distinct(address_line) == 5667) %>%
  verify(nrow(.) == 5667 & ncol(.) == 3) %>%
  arrange(address_line)

pa_zips_freq_2016 <- as.data.frame(table(pa_2016_df$zipcode)) %>%
  mutate(
    zipcode = Var1,
    n_pp_2016 = as.numeric(Freq)
  ) %>%
  verify(ncol(.) == 4 & nrow(.) == 1476)

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
    county = as.character(snakecase::to_any_case(county_name,
      case = "big_camel"
    ))
  ) %>%
  select(county, name, house_num, street, street_type, city, zipcode) %>%
  unite("address_line", house_num:zipcode, remove = FALSE, sep = " ") %>%
  drop_na(zipcode) %>%
  mutate(
    address_line = as.character(make_clean_names(address_line,
      sep_out = " ",
      unique_sep = NULL
    )),
    address_line = as.character(sub("_.*", "", address_line)),
    address_line = as.character(gsub(" na ", " ", address_line)),
    name = as.character(gsub(" na ", " ", name)),
    address_line = as.character(gsub("na ", " ", address_line)),
    name = as.character(gsub("na ", "", name)),
    address_line = as.character(gsub(" na", " ", address_line)),
    name = as.character(gsub(" na", "", name)),
    address_line = as.character(gsub(" pitsburgh ", " pittsburgh ", address_line)),
    address_line = as.character(gsub(" altoo ", " altoona ", address_line)),
    address_line = as.character(gsub(" street ", " st ", address_line)),
    address_line = as.character(gsub(" sts ", " st ", address_line)),
    address_line = as.character(gsub(" avenue ", " ave ", address_line)),
    address_line = as.character(gsub(" av ", " ave ", address_line)),
    address_line = as.character(gsub(" road ", " rd ", address_line)),
    address_line = as.character(gsub(" drive ", " dr ", address_line)),
    address_line = as.character(gsub(" boulevard ", " blvd ", address_line)),
    address_line = as.character(gsub(" blv ", " blvd ", address_line)),
    address_line = as.character(gsub(" route ", " rte ", address_line)),
    address_line = as.character(gsub(" rt ", " rte ", address_line)),
    address_line = as.character(gsub(" lane ", " ln ", address_line)),
    address_line = as.character(gsub(" hiway ", " hwy ", address_line)),
    address_line = as.character(gsub(" highway ", " hwy ", address_line)),
    address_line = as.character(gsub(" parkway ", " pkwy ", address_line)),
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
    address_line = as.character(gsub("first ", "1st ", address_line)),
    address_line = as.character(gsub("second ", "2nd ", address_line)),
    address_line = as.character(gsub("third ", "3rd ", address_line)),
    address_line = as.character(gsub("fourth ", "4th ", address_line)),
    address_line = as.character(gsub("fifth ", "5th ", address_line)),
    address_line = as.character(gsub("sixth ", "6th ", address_line)),
    address_line = as.character(gsub("seventh ", "7th ", address_line)),
    address_line = as.character(gsub("eigth ", "8th ", address_line)),
    address_line = as.character(gsub("ninth ", "9th ", address_line)),
    address_line = as.character(gsub("tenth ", "10th ", address_line)),
    address_line = as.character(gsub("01st ", "1st ", address_line)),
    address_line = as.character(gsub("02nd ", "2nd ", address_line)),
    address_line = as.character(gsub("03rd ", "3rd ", address_line)),
    address_line = as.character(gsub("04th ", "4th ", address_line)),
    address_line = as.character(gsub("05th ", "5th ", address_line)),
    address_line = as.character(gsub("06th ", "6th ", address_line)),
    address_line = as.character(gsub("07th ", "7th ", address_line)),
    address_line = as.character(gsub("08th ", "8th ", address_line)),
    address_line = as.character(gsub("09th ", "9th ", address_line)),
    address_line = if_else(
      address_line == "x95 center circle rd west hickory 16370",
      "x95 center circle st west hickory 16370", address_line
    ),
    name = if_else(
      name == "hickory township", "harmony township building", name
    ),
    address_line = if_else(
      address_line == "x9151 old newton rd philadelphia 19115",
      "x9151 old newtown rd philadelphia 19115", address_line
    ),
    address_line = if_else(
      address_line == "x860 colonial manor large bldg rd n huntingdon 15642",
      "x860 colonial manor rd n huntingdon 15642", address_line
    ),
    address_line = if_else(address_line == "x825 poplar ave pittsburgh 15220",
      "x825 poplar st pittsburgh 15220", address_line
    ),
    address_line = if_else(
      address_line == "x81 commonwealth dr west mifflin 15122",
      "x81 commonwealth ave west mifflin 15122", address_line
    ),
    address_line = if_else(
      address_line == "x800 coopertown rd bryn mawr 19010",
      "x800 coopertown rd haverford 19041", address_line
    ),
    address_line = if_else(
      address_line == "x7701 mansfield ave philadelphia 19138",
      "x7701 mansfield ave philadelphia 19150", address_line
    ),
    address_line = if_else(address_line == "x761 47th st philadelphia 19104",
      "x761 47th st philadelphia 19139", address_line
    ),
    address_line = if_else(address_line == "x7600 evans st swissvale",
      "x7600 evans st pittsburgh 15218", address_line
    ),
    address_line = if_else(address_line == "x75 evas st pringle 18704",
      "x75 evans st pringle 18704", address_line
    ),
    address_line = if_else(
      address_line == "x703 stevenson blvd new kensingtn 15068",
      "x703 stevenson blvd new kensington 15068", address_line
    ),
    address_line = if_else(
      address_line == "x700 york mitchell ave lansdale 19446",
      "x700 york ave lansdale 19446", address_line
    ),
    address_line = if_else(
      address_line == "x6841 19th st philadelphia 19137",
      "x6841 19th st philadelphia 19126", address_line
    ),
    address_line = if_else(
      address_line == "x6801 grovers st philadelphia 19142",
      "x6801 grovers ave philadelphia 19142", address_line
    ),
    address_line = if_else(
      address_line == "x661 newberry st 17404",
      "x661 newberry st york 17404", address_line
    ),
    address_line = if_else(
      address_line == "x6501 limekiln pk philadelphia 19138",
      "x6501 limekiln pike philadelphia 19138", address_line
    ),
    address_line = if_else(
      address_line == "x6435 frankstown rd pittsburgh 15206",
      "x6435 frankstown ave pittsburgh 15206", address_line
    ),
    address_line = if_else(
      address_line == "x6001 cedar ave philadelphia 19139",
      "x6001 cedar ave philadelphia 19143", address_line
    ),
    address_line = if_else(
      address_line == "x60 gawaldo rd pittsburgh 15108" |
        address_line == "x60 gawaldo rd coraopolis 15108",
      "x60 gawaldo dr coraopolis 15108", address_line
    ),
    address_line = if_else(address_line == "x590 crane a pittsburgh 15216",
      "x590 crane ave pittsburgh 15216", address_line
    ),
    address_line = if_else(
      address_line == "x5898 lancaster ave philadelphia 19124",
      "x5898 lancaster ave philadelphia 19131", address_line
    ),
    address_line = if_else(
      address_line == "x5801 media st philadelphia 19146",
      "x5801 media st philadelphia 19131", address_line
    ),
    address_line = if_else(
      address_line == "x5800 chester ave philadelphia 19142",
      "x5800 chester ave philadelphia 19143", address_line
    ),
    address_line = if_else(
      address_line == "x5799 hampton st pittsburgh 15201",
      "x5799 hampton st pittsburgh 15206", address_line
    ),
    address_line = if_else(
      address_line == "x551 ravensburg blvd pittsburgh 15025",
      "x551 ravensburg blvd clairton 15025", address_line
    ),
    address_line = if_else(
      address_line == "x5470 mcalevy fort rd huntingdon 16652",
      "x5470 mcalevys fort rd petersburg 16669", address_line
    ),
    address_line = if_else(
      address_line == "x545 ppermastone drive northumberland 17857",
      "x545 permastone drive northumberland 17857", address_line
    ),
    address_line = if_else(
      address_line == "x522 rock run rd pittsburgh 15037",
      "x522 rock run rd elizabeth 15037", address_line
    ),
    address_line = if_else(
      address_line == "x506 bessemer st e pittsburgh 15112",
      "x506 bessemer ave e pittsburgh 15112", address_line
    ),
    address_line = if_else(
      address_line == "x503 speer st n belle vern 15012",
      "x503 speer st belle vernon 15012", address_line
    ),
    address_line = if_else(
      address_line == "x503 main st meadville 16335",
      "x503 main st ext meadville 16335", address_line
    ),
    address_line = if_else(address_line == "x501 butler st springdale 15144",
      "x501 butler rd springdale 15144", address_line
    ),
    address_line = if_else(
      address_line == "x4901 parrish philadelphia 19139",
      "x4901 parrish st philadelphia 19139", address_line
    ),
    address_line = if_else(
      address_line == "x4901 chestnut st philadelphia 19104",
      "x4901 chestnut st philadelphia 19139", address_line
    ),
    address_line = if_else(address_line == "x4565 prestwick dr reading 19601",
      "x4565 prestwick dr reading 19606", address_line
    ),
    address_line = if_else(
      address_line == "x4300 ave of republice philadelphia 19131",
      "x4300 ave of republic philadelphia 19131", address_line
    ),
    address_line = if_else(
      address_line == "x429 berwick hazleton hwy nescopeck 18635",
      "x429 berwick hazelton hwy nescopeck 18635", address_line
    ),
    address_line = if_else(
      address_line == "x423 allegheny st h0llidaysburg 16648",
      "x423 allegheny st hollidaysburg 16648", address_line
    ),
    address_line = if_else(address_line == "x419 pearson rd lititz 17543",
      "x419 pierson rd lititz 17543", address_line
    ),
    address_line = if_else(address_line == "x416 lincoln ave pittsburgh 15209",
      "x416 lincoln ave millvale 15209", address_line
    ),
    address_line = if_else(
      address_line == "x415 brennan ave loyalhanna 15661",
      "x415 brennan ave latrobe 15650", address_line
    ),
    address_line = if_else(
      address_line == "x414 railroad st pittsburgh 15028" |
        address_line == "x414 railroad st coulters 15028",
      "x414 railroad st white oak 15131", address_line
    ),
    address_line = if_else(
      address_line == "x400 sproul rd springfield 19064",
      "x400 sproul rd springield 19064", address_line
    ),
    address_line = if_else(address_line == "x40 prospect ave pitsburgh 15205",
      "x40 prospect ave pittsburgh 15205", address_line
    ),
    address_line = if_else(
      address_line == "x4 westtown thornton rd thornton 19373",
      "x4 westtown rd thornton 19373", address_line
    ),
    address_line = if_else(address_line == "x3856 10 th st erie 16505",
      "x3856 10th st erie 16505", address_line
    ),
    address_line = if_else(
      address_line == "x3501 midvale ave philadelphia 19125" |
        address_line == "x3501 midvale ave philadelphia 19121",
      "x3501 midvale ave philadelphia 19129", address_line
    ),
    address_line = if_else(
      address_line == "x3436 windchester rd allentown 18104",
      "x3436 winchester rd allentown 18104", address_line
    ),
    address_line = if_else(
      name == "kerr elementary school" &
        address_line != "x341 kittanning pike pittsburgh 15215",
      "x341 kittanning pike pittsburgh 15215", address_line
    ),
    address_line = if_else(address_line == "x338 28th st philadelphia 19103",
      "x338 26th st philadelphia 19103", address_line
    ),
    address_line = if_else(
      address_line == "x3351 millers run rd mcdonald 15057",
      "x3351 millers run rd cecil 15321", address_line
    ),
    address_line = if_else(
      address_line == "x3344 churchview ave ext ave pittsburgh 15227",
      "x3344 churchview ave pittsburgh 15227", address_line
    ),
    address_line = if_else(
      address_line == "x330 hill top rd hummelstown 17036",
      "x330 hilltop rd hummelstown 17036", address_line
    ),
    address_line = if_else(address_line == "x33 lonsdale st pittsburgh 15214",
      "x33 lonsdale st pittsburgh 15212", address_line
    ),
    name = if_else(name == "reserve township volunteer fire department",
      "reserve township police department", name
    ),
    address_line = if_else(address_line == "x3260 quenshukeny rd linden 17744",
      "x3260 queneshukeny rd", address_line
    ),
    address_line = if_else(address_line == "x326 napoleon st johnstown 15905",
      "x326 napoleon st johnstown 15901", address_line
    ),
    address_line = if_else(address_line == "x325 7th st philadelphia 19107",
      "x325 7th st philadelphia 19106", address_line
    ),
    address_line = if_else(address_line == "x3245 oakland rd bethlehem 18017",
      "x3245 oakland rd bethlehem 18020", address_line
    ),
    address_line = if_else(address_line == "x320 hawkins ave braddock 15104",
      "x320 hawkins ave rankin 15104", address_line
    ),
    address_line = if_else(
      address_line == "x306 st james ln n alexandria 15670",
      "x306 st james ln new alexandria 15670", address_line
    ),
    address_line = if_else(
      address_line == "x3010 route 212 springtown 18055",
      "x3010 route 212 springtown 18081", address_line
    ),
    address_line = if_else(
      address_line == "x2940 sheraden blvd pittsburgh 15205",
      "x2940 sheraden blvd pittsburgh 15204", address_line
    ),
    address_line = if_else(
      address_line == "x281 mountain st mountain top 18707",
      "x281 mountain blvd mountain top 18707", address_line
    ),
    address_line = if_else(
      address_line == "x2604 4th philadelphia 19148",
      "x2604 4th st philadelphia 19148", address_line
    ),
    address_line = if_else(
      name == "x31 15 cione recreation center" |
        name == "x31 18 cione recreation center",
      "x2600 aramingo ave philadelphia 19125", address_line
    ),
    address_line = if_else(
      address_line == "x26 patterson st r lansford 18232",
      "x26 patterson st lansford 18232", address_line
    ),
    address_line = if_else(
      address_line == "x2565 susquehanna ave abington 19001",
      "x2565 susquehanna rd abington 19001", address_line
    ),
    address_line = if_else(
      address_line == "x2551 murray rd huntingdon vly 19006",
      "x2551 murray ave huntingdon vly 19006", address_line
    ),
    address_line = if_else(
      address_line == "x2501 63rd st philadelphia 19153" |
        address_line == "x2501 63rd st philadelphia 19104",
      "x2501 63rd st philadelphia 19142", address_line
    ),
    address_line = if_else(
      address_line == "x243 thiorne hill rd shickshinny 18655",
      "x243 thorne hill rd shickshinny 18655", address_line
    ),
    address_line = if_else(
      address_line == "x2425 new falls rd newportville 19056",
      "x2425 new falls rd levittown 19054", address_line
    ),
    address_line = if_else(
      address_line == "x241 maple ave pittsburgh 15221",
      "x241 maple ave pittsburgh 15218", address_line
    ),
    address_line = if_else(
      address_line == "x2409 shady ave pittsburgh 15207",
      "x2409 shady ave pittsburgh 15217", address_line
    ),
    address_line = if_else(
      address_line == "x230 righters mill rd rd gladwyne 19035",
      "x230 righters mill rd gladwyne 19035", address_line
    ),
    address_line = if_else(
      address_line == "x227 meetinghouse rd horsham 19044",
      "x227 meeting house rd horsham 19044", address_line
    ),
    address_line = if_else(
      address_line == "x2250 center ave pittsburgh 15219",
      "x2250 centre ave pittsburgh 15219", address_line
    ),
    address_line = if_else(
      name == "ross township public works building" &
        address_line != "x225 cemetery ln pittsburgh 15237",
      "x225 cemetery ln pittsburgh 15237", address_line
    ),
    address_line = if_else(
      address_line == "x2201 16th st philadelphia 19144",
      "x2201 16th st philadelphia 19145", address_line
    ),
    address_line = if_else(address_line == "x2200 haines rd levittown 19054",
      "x2200 haines rd levittown 19055", address_line
    ),
    address_line = if_else(address_line == "x2021 20 st erie 16510",
      "x2021 20th st erie 16510", address_line
    ),
    address_line = if_else(address_line == "x201 lysle st mckeesport 15132",
      "201 lysle blvd mckeesport 15132", address_line
    ),
    address_line = if_else(address_line == "x201 18th st homestead 15120",
      "x201 18th ave homestead 15120", address_line
    ),
    address_line = if_else(
      address_line == "x2001 wylie ave pittsburgh 15213",
      "x2001 wylie ave pittsburgh 15219", address_line
    ),
    address_line = if_else(
      address_line == "x1919 ontario ave philadelphia 19140",
      "x1919 ontario st philadelphia 19140", address_line
    ),
    address_line = if_else(
      address_line == "x1832 howard st philadelphia 19132",
      "x1832 howard st philadelphia 19122", address_line
    ),
    address_line = if_else(
      name == "munhall number 5 volunteer fire department",
      "x1817 whitaker st munhall 15120", address_line
    ),
    address_line = if_else(
      address_line == "x1801 22nd st philadelphia 19146",
      "x1801 22nd st philadelphia 19145", address_line
    ),
    address_line = if_else(
      name == "shaler area middle school",
      "x1800 mt royal blvd glenshaw 15116", address_line
    ),
    address_line = if_else(
      address_line == "x1800 7th st rd new kensingtn 15068",
      "x1800 7th st rd new kensington 15068", address_line
    ),
    address_line = if_else(
      address_line == "x1796 lower heckman rd mckeesport 15131",
      "x1796 lower heckman rd white oak 15131", address_line
    ),
    address_line = if_else(
      address_line == "x17920 main st corry 16407",
      "x17920 main st elgin 16413", address_line
    ),
    address_line = if_else(address_line == "x1761 26 st erie 16502",
      "x1761 26th st erie 16508", address_line
    ),
    address_line = if_else(
      address_line == "x175 green la philadelphia 19127" |
        address_line == "x175 green la philadelphia 19128",
      "x175 green ln philadelphia 19127", address_line
    ),
    address_line = if_else(
      address_line == "x17074 crogan pike shirleysburg 17260",
      "x17074 croghan pike shirleysburg 17260", address_line
    ),
    address_line = if_else(name == "deer lakes middle school",
      "x17 e union rd tarentum 15024", address_line
    ),
    address_line = if_else(
      address_line == "x1621 54th philadelphia 19131",
      "x1621 54th st philadelphia 19131", address_line
    ),
    address_line = if_else(
      address_line == "x1612 manhattan st pittsburgh 15212",
      "x1612 manhattan st pittsburgh 15233", address_line
    ),
    address_line = if_else(
      address_line == "x1610 chelten ave philadelphia 19123" |
        address_line == "x1610 chelten ave philadelphia 19126",
      "x1610 chelten ave philadelphia 19141", address_line
    ),
    address_line = if_else(
      address_line == "x1606 sullivan easton 18040",
      "x1606 sullivan trail easton 18040", address_line
    ),
    address_line = if_else(name == "eisenhower middle school",
      "x1601 markley st norristown 19401", address_line
    ),
    address_line = if_else(name == "hanover area high school",
      "x1600 sans souci pkwy hanover twnshp 18706", address_line
    ),
    address_line = if_else(
      address_line == "x1500 boyce rd upper st clair 15241",
      "x1500 boyce rd pittsburgh 15241", address_line
    ),
    address_line = if_else(
      address_line == "x150 wadham ave plymouth 18651",
      "x150 wadham st plymouth 18651", address_line
    ),
    address_line = if_else(
      address_line == "x1445 lincoln hwy coatesville 19320",
      "x1445 linc hwy coatesville 19320", address_line
    ),
    address_line = if_else(
      address_line == "x1414 beaver rd osborne 15143",
      "x1414 beaver st sewickley 15143", address_line
    ),
    address_line = if_else(name == "north versailles community center",
      "x1401 greensburg ave n versailles 15137", address_line
    ),
    address_line = if_else(
      address_line == "x139 muncipal st e freedom 16637",
      "x139 municipal st e freedom 16637", address_line
    ),
    address_line = if_else(name == "langeloth community center",
      "x1375 langeloth rd burgettstown 15021", address_line
    ),
    address_line = if_else(
      address_line == "x136 community bldg rd leechburg 15656",
      "x136 community building rd leechburg 15656", address_line
    ),
    address_line = if_else(
      address_line == "carpenter and miller st schaefferstown 17088",
      "carpenter miller st schaefferstown 17088", address_line
    ),
    address_line = if_else(
      address_line == "echo lake 652 laurel dr tobyhan18466",
      "echo lake 652 laurel dr tobyhanna 18466", address_line
    ),
    address_line = if_else(
      address_line == "x982 chestnut st ext derry 15627",
      "x982 chestnut st derry 15627", address_line
    ),
    address_line = if_else(
      address_line == "x965 burtner rd pittsburgh 15065" |
        address_line == "x965 burtner rd tro heights 15065",
      "x965 burtner rd natrona heights 15065", address_line
    ),
    address_line = if_else(
      address_line == "x95 circle center rd w hickory 16370",
      "x965 burtner rd natrona heights 15065", address_line
    ),
    address_line = if_else(
      address_line == "xx95 circle center rd w hickory 16370" |
        address_line == "x95 center circle rd w hickory 16370",
      "x95 center circle st w hickory 16370", address_line
    ),
    address_line = if_else(address_line == "x927 freeport rd tarentum 15030",
      "x927 freeport rd creighton 15030", address_line
    ),
    address_line = if_else(address_line == "x915 linc ave w chester 19380",
      "x915 lincoln ave w chester 19380", address_line
    ),
    address_line = if_else(
      address_line == "x81 commonwealth dr w mifflin 15122",
      "x81 commonwealth ave w mifflin 15122", address_line
    ),
    address_line = if_else(
      address_line == "x800 mac dade blvd collingdale 19023",
      "x800 macdade blvd collingdale 19023", address_line
    ),
    address_line = if_else(address_line == "x80 jones st shickshinny 18655",
      "x80 jones st wilkes barre 18702", address_line
    ),
    address_line = if_else(address_line == "x7911 wm penn hwy cresson",
      "x7911 wm penn hwy cresson 16630", address_line
    ),
    address_line = if_else(address_line == "x200 04 3rd ave altoona 16602",
      "x200 3rd ave altoona 16602", address_line
    ),
    address_line = if_else(
      address_line == "x1030 cochrans mill rd s park 15236",
      "x1030 cochrans mill rd pittsburgh 15236", address_line
    ),
    address_line = if_else(
      address_line == "x751 sugar run rd altoona 16602",
      "x751 sugar run rd altoona 16601", address_line
    ),
    address_line = if_else(
      address_line == "x125 parkview st pittsburgh 15210",
      "x125 parkfield st pittsburgh 15210", address_line
    ),
    address_line = if_else(
      name == "cheltenham school admin building" &
        address_line != "x2000 ashbourne rd elkins park 19027",
      "x2000 ashbourne rd elkins park 19027", address_line
    ),
    address_line = if_else(
      address_line == "x1 coebourn rd brookhaven 19015",
      "x1 coebourn blvd brookhaven 19015", address_line
    ),
    address_line = if_else(
      address_line == "x370 carvertown rd shavertown 18708",
      "x370 carverton rd shavertown 18708", address_line
    ),
    address_line = if_else(
      address_line == "x8448 jonestown rd grantville 17028",
      "x8848 jonestown rd grantville 17028", address_line
    ),
    address_line = if_else(
      address_line == "x539 chicora st mckeesport 15035",
      "x539 chicora st e mc keesport 15035", address_line
    ),
    address_line = if_else(
      address_line == "x326 poleon st johnstown 15905",
      "x326 napoleon st johnstown 15901", address_line
    ),
    address_line = if_else(
      address_line == "x799 barclay ave pittsburgh 15221",
      "x700 barclay ave pittsburgh 15221", address_line
    ),
    address_line = if_else(
      address_line == "x3 keystone commons white haven 18661",
      "x3 keystone cmns white haven 18661", address_line
    ),
    address_line = if_else(
      address_line == "x542 otterman st greensburg 15601",
      "x542 w otterman st greensburg 15601", address_line
    ),
    address_line = if_else(
      address_line == "x2340 tioga st pittsburgh 15208",
      "x7430 tioga st pittsburgh 15208", address_line
    ),
    address_line = if_else(
      address_line == "x1111 hewitt st h0llidaysburg 16648",
      "x1000 hewitt st hollidaysburg 16648", address_line
    ),
    address_line = if_else(
      address_line == "x11 russell la warren 16365",
      "x11 russell st warren 16365", address_line
    ),
    address_line = if_else(
      address_line == "x32 firston st duquesne 15110",
      "x32 1st st duquesne 15110", address_line
    ),
    address_line = if_else(
      address_line == "x45 mahanoy ave hometown 18252",
      "x45 mahanoy ave tamaqua 18252", address_line
    ),
    address_line = if_else(
      address_line == "x1037 state rte 56 rd apollo 15613",
      "x1037 state rte 56 apollo 15613", address_line
    ),
    address_line = if_else(name == "martin l mattei middle school" &
      address_line != "x120 new st pittston 18640",
    "x120 new st pittston 18640", address_line
    ),
    address_line = if_else(
      name == "mckeesport public safety building" &
        address_line != "x201 lysle blvd mckeesport 15132",
      "x201 lysle blvd mckeesport 15132", address_line
    ),
    address_line = if_else(address_line == "x10th st jim thorpe 18229",
      "x101 10th st jim thorpe 18229", address_line
    ),
    address_line = if_else(address_line == "x223 clever rd pittsburgh 15136",
      "x225 clever rd mckees rocks 15136", address_line
    ),
    address_line = if_else(
      address_line == "x940 beaver grade rd coraopolis 15108",
      "x904 beaver grade rd coraopolis 15108", address_line
    ),
    address_line = if_else(name == "mother cabrini church hall",
      "x214 n shamokin st shamokin 17872", address_line
    ),
    address_line = if_else(address_line == "x1500 lincoln ave scranton 18504",
      "x1500 n lincoln ave scranton 18504",
      address_line
    ),
    address_line = if_else(address_line == "x2375 levans rd coplay 18036",
      "x2375 levans rd coplay 18037", address_line
    ),
    address_line = if_else(address_line == "x1504 bedford st johnstown",
      "x1504 bedford st johnstown 15902", address_line
    ),
    address_line = if_else(address_line == "x608 main st lilly",
      "x608 main st lilly 15938", address_line
    ),
    address_line = if_else(name == "penn hills library",
      "x1037 stotler rd pittsburgh 15235", address_line
    ),
    address_line = if_else(
      address_line == "x2875 perrysville ave pittsburgh 15214",
      "x3875 perrysville ave pittsburgh 15214", address_line
    ),
    address_line = if_else(name == "pioneer hose company",
      "x124 morgan st brackenridge 15014", address_line
    ),
    address_line = if_else(name == "roslyn elementary school",
      "x2565 susquehanna rd abington 19001",
      address_line
    ),
    address_line = if_else(
      address_line == "x50 canal st middletown 17057",
      "x50 canal st royalton 17057", address_line
    ),
    address_line = if_else(
      address_line == "x1231 meetinghouse rd jenkintown 19046",
      "x1231 meeting house rd jenkintown 19046", address_line
    ),
    address_line = if_else(name == "sacred heart church",
      "x154 orchard ave emsworth 15202", address_line
    ),
    address_line = if_else(
      address_line == "x1220 bridge st honesdale 18431",
      "x1200 bridge st honesdale 18431", address_line
    ),
    address_line = if_else(
      address_line == "x545 ppermastone dr northumberland 17857",
      "x545 permastone dr northumberland 17857", address_line
    ),
    address_line = if_else(name == "shrewsbury township municipal building",
      "x11505 susquehanna trail s glen rock trl 17327",
      address_line
    ),
    address_line = if_else(name == "springtown fire company",
      "x3010 rte 212 coopersburg 18055", address_line
    ),
    address_line = if_else(address_line == "x7911 wm penn hwy cresson",
      "x7911 wm penn hwy cresson 16630", address_line
    ),
    address_line = if_else(address_line == "x33 lewin la pittsburgh 15235",
      "x33 lewin ln pittsburgh 15235", address_line
    ),
    address_line = if_else(
      address_line == "x1315 marshall selma st norristown 19401",
      "x1315 marshall st norristown 19401", address_line
    ),
    address_line = if_else(
      address_line == "x11605 dickens dr irwin 15642",
      "x11605 dickens dr n huntingdon 15642", address_line
    ),
    address_line = if_else(name == "the bible chapel fellowship hall",
      "x200 state st belle vernon 15012",
      address_line
    ),
    address_line = if_else(address_line == "x524 franklin ave aliquippa 15001",
      "x464 franklin ave aliquippa 15001",
      address_line
    ),
    address_line = if_else(address_line == "x4770 drexelbrook dr drexel hill 19026",
      "x4700 drexelbrook dr drexel hill 19026",
      address_line
    ),
    address_line = if_else(address_line == "pine st whitehall 18052",
      "x2301 pine st whitehall 18052", address_line
    ),
    address_line = if_else(
      address_line == "x110 vaneer ave greensburg 15601",
      "x110 vannear ave greensburg 15601", address_line
    ),
    address_line = if_else(
      name == "west homestead volunteer fire department",
      "x447 w 8th ave w homestead pa 15120", address_line
    ),
    address_line = if_else(
      address_line == "x395 perry hwy pittsburgh 15229",
      "x398 perry hwy pittsburgh 15229", address_line
    ),
    address_line = if_else(
      address_line == "x1400 weston way w chester 19380",
      "x1402 weston way w chester 19380", address_line
    ),
    address_line = if_else(
      address_line == "x110 peffer rd pittsburgh 15145",
      "x110 peffer rd turtle creek 15145", address_line
    ),
    address_line = if_else(
      address_line == "x7600 evans st swissvale 15218",
      "x7600 evans st pittsburgh 15218", address_line
    ),
    address_line = if_else(
      address_line == "x28 memorial st exeter 18643",
      "x20 memorial st exeter 18643", address_line
    ),
    address_line = if_else(address_line == "x1228 brinkerton rd greensburg 156010",
      "x1228 brinkerton rd greensburg 15601", address_line
    ),
    address_line = if_else(address_line == "x12 fire rd eldersville 15036",
      "x12 fire rd eldersville 15021", address_line
    ),
    address_line = if_else(address_line == "x4628 upper rd gowen city 17828",
      "x4628 upper rd gowen city 17872", address_line
    ),
    address_line = if_else(address_line == "x3387 lafayette rd bakers summit 16614",
      "x3387 lafayette rd roaring spring 16673", address_line
    ),
    address_line = if_else(address_line == "x339 jamestown rd greenville 16152",
      "x339 e jamestown rd greenville 16125", address_line
    ),
    address_line = if_else(address_line == "x3493 stull rd noxen 186369",
      "x3493 stull rd noxen 18636", address_line
    ),
    address_line = if_else(address_line == "x109 german erie 16597",
      "x109 german erie 16507", address_line
    ),
    address_line = if_else(address_line == "x3260 queneshukeny rd",
      "x3260 queneshukeny rd linden 17744", address_line
    ),
    address_line = if_else(address_line == "x17 16 widener school" |
      address_line == "x1450 olney ave philadelphia 19141",
    "x1450 w olney ave philadelphia 19141", address_line
    )
  ) %>%
  filter(!zipcode %in% odd_zips) %>%
  mutate(zipcode = as.character(substr(
    address_line, nchar(address_line) - n_last + 1, nchar(address_line)
  ))) %>%
  select(name, address_line, zipcode) %>%
  filter(address_line != "x285 pine run church rd apollo 15613") %>%
  distinct(address_line, .keep_all = TRUE) %>%
  verify((zipcode %in% pa_cos$zip) == TRUE) %>%
  verify(n_distinct(address_line) == 5061) %>%
  verify(nrow(.) == 5061 & ncol(.) == 3) %>%
  arrange(address_line)

# The one address removed in line 2593 is a random empty house

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
