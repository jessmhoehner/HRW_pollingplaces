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
				$(HERE)/clean/input/az_2016_imported.rds \
				$(HERE)/clean/input/az_2020_imported.rds \
				$(HERE)/clean/input/az_2020_maricopa_imported.rds \
				$(HERE)/clean/input/sc_2016_imported.rds \
				$(HERE)/clean/input/sc_2020_imported.rds 
				
.PHONY:	all clean

all: $(import_data)

clean:
				-rm -r $(HERE)/clean/input/*
		
$(import_data): \
				src/import.R \
				$(HERE)/input/vip_az_2016primary/polling_location.txt \
				$(HERE)/input/vip_az_2020primary/polling_location.txt \
				$(HERE)/input/vip_az_maricopa_2020primary/polling_location.txt \
				$(HERE)/input/vip_sc_2016primary/polling_location.txt \
				$(HERE)/input/vip_az_2020primary/polling_location.txt
			Rscript --vanilla $<
			
# done.