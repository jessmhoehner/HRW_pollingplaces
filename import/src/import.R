#!/usr/bin/env Rscript --vanilla
#
# Authors:     JR
# Maintainers: JR
# Copyright:   2020, HRW, GPL v3 or later
# ============================================
# HRW_pollingplaces/import/src/import.R
#
pacman::p_load(
  "here", "tidyverse", "janitor",
  "assertr", "tidycensus", "textreadr"
)

# AZ and SC zipcode and county data obtained here
# https://www.unitedstateszipcodes.org/az/#zips-list and
# https://www.unitedstateszipcodes.org/sc/#zips-list

inputs <- list(
  az_2016 = here("import/input/vip_az_2016primary/polling_location.txt"),
  az_2020 = here("import/input/vip_az_2020primary/polling_location.txt"),
  az_2020_maricopa = here("import/input/vip_az_maricopa_2020primary/polling_location.txt"),

  sc_2016 = here("import/input/vip_sc_2016primary/polling_location.txt"),
  sc_2020 = here("import/input/vip_sc_2020primary/polling_location.txt"),

  zip_counties = here("import/input/zips/zip_code_database.csv"),

  pa_2016 = here("import/input/penn/CopyofPollingPlacesList_20160425.csv"),
  pa_2020 = here("import/input/penn/PollingPlaceList20200601(1).csv")
)
outputs <- list(
  VIPinlist_imp = here("clean/input/VIPdata_imported.rds"),

  pa_2016_imp = here("clean/input/pa2016_imported.rds"),
  pa_2016_2_imp = here("clean/input/pa2016_2_imported.rds"),
  pa_2020_imp = here("clean/input/pa2020_imported.rds"),

  census_imp = here("clean/input/census_imported.rds"),
  counnzip_azscpa_imp = here("clean/input/counzip_azscpa_imported.rds")
)

# import VIP data
## creates a list of VIP files as connections
inputslist <- list(
  inputs$az_2016, inputs$az_2020, inputs$az_2020_maricopa,
  inputs$sc_2016, inputs$sc_2020
)

names(inputslist) <- c(
  "az_2016", "az_2020", "az_2020_maricopa",
  "sc_2016", "sc_2020"
)

stopifnot(length(inputslist) == 5)

inlist <- lapply(inputslist, function(x) {
  expected_cols <- c("id", "name", "address_line")

  x_df <- read_csv(x,
    col_names = TRUE, na = "NA",
    col_types = cols_only(
      id = "c",
      name = "c", address_line = "c"
    )
  ) %>%
    clean_names() %>%
    verify(colnames(.) == expected_cols) %>%
    verify(ncol(.) == 3)
})

stopifnot(length(inlist) == 5)
saveRDS(inlist, outputs$VIPinlist_imp)


expected_colspa2016 <- c(
  "county_name", "precinct_code", "precinct_name",
  "description", "house_num", "prefix_direction_desc",
  "street", "street_type_desc", "suffix_direction_desc",
  "city", "state_desc", "postal_code", "line2", "comment",
  "day_phone"
)

pa2016_df <- read_csv(inputs$pa_2016,
  col_names = TRUE,
  na = "NULL", col_types =
    c(
      Comment = "c", HouseNum = "c",
      PostalCode = "n", PrecinctCode = "c"
    )
) %>%
  clean_names() %>%
  verify(colnames(.) == expected_colspa2016) %>%
  verify(ncol(.) == 15 & nrow(.) == 9155) %>%
  saveRDS(outputs$pa_2016_imp)

expected_colspa2020 <- c(
  "county_name", "precinct_code", "precinct_name",
  "precinct_split_code", "description", "house_num",
  "prefix_direction", "street", "street_type",
  "suffix_direction", "city", "state", "postal_code",
  "line2", "comment", "day_phone", "handicap_accessible"
)

pa2020_df <- read_csv(inputs$pa_2020,
  col_names = TRUE,
  na = "NULL", col_types =
    c(
      SuffixDirection = "c",
      Comment = "c", HouseNum = "c", PostalCode = "n"
    )
) %>%
  clean_names() %>%
  verify(colnames(.) == expected_colspa2020) %>%
  verify(ncol(.) == 17 & nrow(.) == 9234) %>%
  saveRDS(outputs$pa_2020_imp)

# zips in SC, AZ, and PA
zc <- read_csv(inputs$zip_counties,
  col_names = TRUE, na = "",
  col_types = cols_only(
    zip = "n",
    state = "c",
    county = "c"
  )
) %>%
  clean_names() %>%
  filter(state == "AZ" | state == "SC" | state == "PA") %>%
  filter(is.na(county) != TRUE) %>%
  verify(ncol(.) == 3 & nrow(.) == 3318) %>%
  verify(min(zip) == 15001 & max(zip) == 86556)

# import census data for SC and AZ ending in 2018
# data come from 2014-2018 5 year ACS
expected_cols3 <- c("geoid", "name", "variable", "estimate", "moe")

demo_income_1418 <- get_acs(
  geography = "zcta",
  variables = c(
    total = "B03002_001",
    total_nhl = "B03002_002",
    nhl_white = "B03002_003",
    nhl_black = "B03002_004",
    nhl_ai_an = "B03002_005",
    nhl_asian = "B03002_006",
    nhl_nhi_pi = "B03002_007",
    nhl_sor = "B03002_008",
    nhl_tom = "B03002_009",
    total_hl = "B03002_012",
    med_income_byhh = "B19019_001"
  ),
  year = 2018,
  geometry = FALSE
) %>%
  clean_names() %>%
  verify(colnames(.) == expected_cols3) %>%
  select(geoid, name, variable, estimate) %>%
  filter(geoid %in% zc$zip) %>%
  verify(ncol(.) == 4 & nrow(.) == 28897) %>%
  saveRDS(outputs$census_imp)

zc <- zc %>%
  saveRDS(outputs$counnzip_azscpa_imp)

# done.