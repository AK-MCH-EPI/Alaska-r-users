#### Meta ######################################################################
#
# Project: R - users group
# Date: 1/18/2022
# Author: Jared Parrish
# Notes: Saving Rmarkdown output 
#
#

### set-up #####################################################################

library(jsonlite)
library(rmarkdown)
library(dplyr)


options(scipen=999) # remove scientific notation

#Connect to API, Read in data, and clean a bit

url <- "https://data.alaska.gov/resource/7vre-awpz.json"
t_dat <- jsonlite::fromJSON(url)

### Look at data ###############################################################

t_dat <- t_dat %>% mutate_at(c("year","birth_rate","lower_confidence_limit","upper_confidence_limit"), as.numeric)
head(t_dat)

## Make a table of county fips codes
cnty <- t_dat %>% group_by(county_fips_code) %>%
                  top_n(1) %>%
                  select(county,county_fips_code)

## Make a function to run .Rmd file for a specified county fips code

## NOTE: you'll need to update the paths to run

render_report <- function(cnty_fips,dta){
  t_dat <- dta 
  county <- t_dat %>% filter (county_fips_code == {{cnty_fips}}) %>% 
                      select (county) 
  cnty <- first(county$county)
  
  rmarkdown::render("R:\\UTRS\\OPERATIONS\\TRAININGS\\R Users Group\\CodeArchive\\SavingRmarkdown\\SavingRmarkdwn.Rmd", 
                    output_file = paste0(cnty,"_TeenBirthRate.html"),
                    params = list(cnty_fips = (cnty_fips)),
                    output_dir = "R:\\UTRS\\OPERATIONS\\TRAININGS\\R Users Group\\CodeArchive\\SavingRmarkdown\\Reports\\")
}

#run report by specified county - could create a function or apply as well...
render_report(cnty_fips = 20, dta = t_dat)     
render_report(cnty_fips = 13, dta = t_dat) 
render_report(cnty_fips = 50, dta = t_dat) 
