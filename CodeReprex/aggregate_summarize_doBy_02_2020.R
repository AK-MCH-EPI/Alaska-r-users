#### Meta ################################################################################
#
# Project: R - users group
# Date: 2/16/2021
# Author: Jared Parrish
# Notes: Three functions for summarizing data by a group
# Dataset: infert
#

### set-up ###############################################################################

library(Hmisc)
library(doBy)
library(dplyr)

### Review dataset #######################################################################


dplyr::glimpse(infert)

Hmisc::describe(infert)

### Three functions for describing data by a group #######################################

### stats package - with base install

aggregate(infert$age, by = list(Induced = infert$induced), FUN = mean)
# or
aggregate(age ~ induced, data = infert, mean)

# add in another strata
aggregate(infert$age, by = list(Induced = infert$induced,
                                Case = infert$case), FUN = mean)
# or
aggregate(age ~ induced + case, data = infert, mean)


### dplyr package

infert %>% group_by(induced) %>%
           summarise(mean_age = mean(age))

infert %>% group_by(induced, case) %>%
           summarise(age = mean(age))

# return multiple summary statistics

infert %>% group_by(induced) %>%
           summarise(age_mean = mean(age),
                     age_median = median(age),
                     age_var = var(age),
                     cnt = length(age),
                     cnt1 = n(), #another way to count by group
                     age_min = min(age),
                     age_max = max(age))

#doBy

doBy::summaryBy(age ~ induced, data = infert, FUN = c(mean))

doBy::summaryBy(age ~ induced + case, data = infert, FUN = c(mean))

# return multiple summary statistics
doBy::summaryBy(age ~ induced, data = infert, FUN = c(mean, median, var, length,
                                                      min, max))

#<<< END >>>


