#### Meta ################################################################################
#
# Project: R - users group
# Date: 9/21/2021
# Author: Jared Parrish
# Notes: Moving averages
#
#


### set-up ###############################################################################

library(dplyr) # data wrangling
library(zoo) #rolling mean, max, medians, sums

### Make a simple dataset #######################################################################
set.seed(2)
dat <- data.frame(
       year = 2010:2019,
       dt.cause1 = sample(c(1:10),10, replace = T)
)

# x year moving average method 1

# inputs: x = a univarite or multivariate time series
#         filter = a vector 
#         method = convolution = moving average or recursive = autoregression
#         sides = use 1 for past values (right), 2 for centered.

dat <- dat %>%
       mutate(MVage2 = stats::filter(dt.cause1,rep(1/2,2),method = "convolution", sides=1),
              MVage3 = stats::filter(dt.cause1,rep(1/3,3),method = "convolution", sides=1),
              MVage4 = stats::filter(dt.cause1,rep(1/4,4),method = "convolution", sides=1),
              MVage3.ct = stats::filter(dt.cause1,rep(1/3,3),method = "convolution", sides=2))
dat

## missing values example

dat_m <- dat %>%
         select(year,dt.cause1) %>%
         mutate(dt.cause1na = ifelse(year == 2015, NA, dt.cause1),
                MVage3m = stats::filter(dt.cause1na,rep(1/3,3),method = "convolution", sides=1))

  # Missing values are allowed in x but not in filter

## using zoo package

#inputs: x = object
#        k = rolling window length
#        fill = filling values for data range
#        align = right, left, centered
# note: rollmean, rollmax, roll median, roll sum

dat <- dat %>%
       mutate(MVage3_zoo = zoo::rollmean(dt.cause1, 3, align = 'right', fill = NA),
              MVage3_zoo.lt = zoo::rollmean(dt.cause1, 3, align = 'left', fill = NA),
              MVage3_zoo.ct = zoo::rollmean(dt.cause1, 3, align = 'center', fill = NA),
              MVage3_zoo.mx = zoo::rollmax(dt.cause1, 3, align = 'right', fill = NA))
dat
