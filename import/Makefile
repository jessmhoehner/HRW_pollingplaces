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
				$(HERE)/write/input/covid_az_imported.rds \
				$(HERE)/clean/input/census_imported.rds \
				$(HERE)/write/input/census_geometry_imported.rds

				
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
				$(HERE)/import/input/covid/COVID19CONFIRMED_BYZIP_excel.csv
			Rscript --vanilla $<
			
# done.