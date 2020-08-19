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
				 $(HERE)/write/input/az_2016_freq_clean.rds \
				 $(HERE)/write/input/az_2020_clean.rds \
				 $(HERE)/write/input/az_2020_freq_clean.rds \
				 $(HERE)/write/input/az_demo_clean.rds \
				 $(HERE)/write/input/sc_2016_clean.rds \
				 $(HERE)/write/input/sc_2016_freq_clean.rds \
				 $(HERE)/write/input/sc_2020_clean.rds \
				 $(HERE)/write/input/sc_2020_freq_clean.rds \
				 $(HERE)/write/input/sc_demo_clean.rds
				 
.PHONY:	all clean

all: $(clean_data)

clean:
				-rm -r $(HERE)/write/input/*
		
$(clean_data): \
				src/clean.R \
				$(HERE)/clean/input/VIPdata_imported.rds \
				$(HERE)/clean/input/census_imported.rds
			Rscript --vanilla $<
			
# done.