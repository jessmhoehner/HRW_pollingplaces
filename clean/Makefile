# -*- coding: utf-8 -*- 
#
#Author: JR
#Maintainer: JR
#License: 2020, HRW, GPL v3 or later
# 
# HRW_pollingplaces/clean/Makefile
#----------------------------------------------------
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

clean_data = \
				 $(HERE)/write/input/az_2016_clean.rds \
				 $(HERE)/write/input/az_2020_clean.rds \
				 $(HERE)/write/input/az_demo_clean.rds \
				 $(HERE)/write/input/az_places.rds \
				 $(HERE)/write/input/sc_2016_clean.rds \
				 $(HERE)/write/input/sc_2020_clean.rds \
				 $(HERE)/write/input/sc_demo_clean.rds \
				 $(HERE)/write/input/sc_places.rds \
				 $(HERE)/write/input/pa_2016_clean.rds \
				 $(HERE)/write/input/pa_2020_clean.rds \
				 $(HERE)/write/input/pa_demo_clean.rds \
				 $(HERE)/write/input/pa_places.rds
				 
.PHONY:	all clean

all: $(clean_data)

clean:
				-rm -r $(HERE)/write/input/*
		
$(clean_data): \
				src/clean.R \
				$(HERE)/clean/input/VIPdata_imported.rds \
				$(HERE)/clean/input/pa2016_imported.rds \
				$(HERE)/clean/input/pa2020_imported.rds \
				$(HERE)/clean/input/census_imported.rds \
				$(HERE)/clean/input/counzip_azscpa_imported.rds
			Rscript --vanilla $<
			
# done.