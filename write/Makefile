# -*- coding: utf-8 -*-
#
# Author: JR
# Maintainer(s): JR
# License: (c) HRW 2020, GPL v3 or newer
#
# HRW_pollingplaces/write/Makefile
#----------------------------------------------------
#

HERE := $(shell git rev-parse --show-toplevel)

.PHONY: all clean

all: output/write.pdf

clean:
	rm $(HERE)/write/output/*

output/write.pdf: \
		src/write.Rmd \
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
	Rscript -e "rmarkdown::render('$<')"
	mv src/*.pdf $(HERE)/write/output/

# done.
