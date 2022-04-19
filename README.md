# HRW_pollingplaces 
Project structure for analysis related to this project with HRW "What Democracy Looks Like
Protecting Voting Rights in the US during the Covid-19 Pandemic"
https://www.hrw.org/report/2020/09/22/what-democracy-looks/protecting-voting-rights-us-during-covid-19-pandemic
<br>
  1. import
      - input/covid data, vip data, zipcode data
      - src/script to read data into clean task
      - Makefile
  2. clean
      - input/data brought in from import task
      - src/script to clean data
	  -Makefile
  3. write
      - input/clean data
	  - output/report
      - src/ script to generate report
	  -Makefile
  4. docs
	- project related files not used by scripts
