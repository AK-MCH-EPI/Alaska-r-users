##### META #####################################################
#
# Date: 01/19/2021
# Author: Chris Barnett
# Project: AK R-users group
# Topic(s): Fun with Functions: The Cs() and unquote()
#
################################################################


library(dplyr)
library(stringr)
library(tidyverse)
library(Hmisc)

# List of ALaska cities without quotes (throws an error)
ak_cities <- c(Anchorage, Fairbanks, Juneau, Sitka, Ketchikan, Wasilla, 
               Kenai, Kodiak, Bethel, Palmer, Homer, Dutch Harbor, 
               UtqiaÄvik, Soldotna, Valdez, Nome, Seward, Wrangell, 
               Dillingham, Cordova, North Pole, Delta Junction)

# List of Alaska cities with quotes
ak_cities_a <- c("Anchorage", "Fairbanks", "Juneau", "Sitka", "Ketchikan", 
               "Wasilla", "Kenai", "Kodiak", "Bethel", "Palmer", "Homer", 
               "Dutch Harbor", "UtqiaÄ¡vik", "Soldotna", "Valdez", 
               "Nome", "Seward", "Wrangell", "Dillingham", "Cordova", 
               "North Pole", "Delta Junction")

# Adding quotations with Cs() function from the Hmisc package
ak_cities_q <- Cs(Anchorage, Fairbanks, Juneau, Sitka, Ketchikan, Wasilla, 
                  Kenai, Kodiak, Bethel, Palmer, Homer, Dutch_Harbor, 
                  UtqiaÄvik, Soldotna, Valdez, Nome, Seward, Wrangell, 
                  Dillingham, Cordova, North_Pole, Delta_Junction)

# Comparing ak_cities_a and ak_cities_q
ak_cities_a
ak_cities_q


# Removing quotes from ak_cities_q with noquote() base function
ak_city_names <- noquote(ak_cities_q)
ak_city_names

# Other interesting notes discussed include using Ctrl + Alt and mouse clicks to
# create multiple cursors that help with inserting multiple commas.

