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
				# add files to be added to write task here # 

				
.PHONY:	all clean

all: $(clean_data)

clean:
				-rm -r $(HERE)/write/input/*
		
$(clean_data): \
				src/clean.R \
				$(HERE)/write/input/VIPdata_imported.rds \
				$(HERE)/write/input/covid_imported.rds \
				$(HERE)/write/input/census_imported.rds
			Rscript --vanilla $<
			
# done.