# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License: 2020, HRW, GPL v3 or later
# 
# HRW_pollingplaces/import/Makefile
#----------------------------------------------------
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

import_data = \
				$(HERE)/clean/input/VIPdata_imported.rds \
				$(HERE)/clean/input/covid_az_imported.rds \
				$(HERE)/clean/input/covid_sc_imported.rds \
				$(HERE)/clean/input/census_imported.rds \
				$(HERE)/clean/input/counzip_azscpa_imported.rds \
				$(HERE)/clean/input/covid_pa_imported.rds \
				$(HERE)/clean/input/pa2016_imported.rds \
				$(HERE)/clean/input/pa2020_imported.rds
				
.PHONY:	all clean

all: $(import_data)

clean:
				-rm -r $(HERE)/clean/input/*
		
$(import_data): \
				src/import.R \
				$(HERE)/import/input/vip_az_2016primary/polling_location.txt \
				$(HERE)/import/input/vip_az_2020primary/polling_location.txt \
				$(HERE)/import/input/vip_az_maricopa_2020primary/polling_location.txt \
				$(HERE)/import/input/vip_sc_2016primary/polling_location.txt \
				$(HERE)/import/input/vip_sc_2020primary/polling_location.txt \
				$(HERE)/import/input/covid/COVID19CONFIRMED_BYZIP_excel.csv \
				$(HERE)/import/input/zips/zip_code_database.csv \
				$(HERE)/import/input/penn/PollingPlaceList20161104.csv \
				$(HERE)/import/input/penn/PollingPlaceList20200601(1).csv
				
				
				
			Rscript --vanilla $<
			
# done.