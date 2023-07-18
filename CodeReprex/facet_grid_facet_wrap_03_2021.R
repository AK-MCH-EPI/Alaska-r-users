#### Meta ################################################################################
#
# Project: R - users group
# Date: 3/16/21
# Author: Rachel Gallegos
# Notes: Faceting GGPLOT graphs
# Dataset: esoph
#

### set-up ###############################################################################

library(tidyverse)

### Review dataset #######################################################################

esoph <- esoph

head(esoph)

summary(esoph)


### Visualizing in GGPLOT #######################################

esoph %>% # Can put dataset in ggplot brackets or pipe to ggplot
  ggplot() +
  geom_point(aes(x = alcgp, y = ncases/(ncases+ncontrols)))

# geom_jitter - adds random noise to data to prevent overlap in non-continuous data
esoph %>% 
  ggplot() +
  geom_jitter(aes(x = alcgp, y = ncases/(ncases+ncontrols)))

# Add variable grouping by color
esoph %>% 
  ggplot() +
  geom_jitter(aes(x = alcgp, y = ncases/(ncases+ncontrols), color = agegp))

### Facets: Divide plots into subplots ########################

# Subplots divided by one or more discrete variables

# facet_wrap() - wrap facets into rectangular layout
ggplot(esoph) +
  geom_jitter(aes(x = agegp, y = ncases/ (ncases + ncontrols))) + 
  facet_wrap( ~ alcgp)

# facet_grid() - rows based on alcgp
ggplot(esoph) +
  geom_jitter(aes(x = agegp, y = ncases/ (ncases + ncontrols))) + 
  facet_grid(alcgp ~ .)

# facet_grid() - columns based on alcgp
ggplot(esoph) +
  geom_jitter(aes(x = agegp, y = ncases/ (ncases + ncontrols))) + 
  facet_grid(. ~ alcgp)

# Adding tobgp
ggplot(esoph) +
  geom_jitter(aes(x = agegp, y = ncases/ (ncases + ncontrols), color = tobgp)) + 
  facet_grid(. ~ alcgp)

# Using two discrete variables
ggplot(esoph) +
  geom_point(aes(x = alcgp, y = ncases / (ncases + ncontrols), color = tobgp)) + 
  facet_grid(agegp ~ tobgp) # Could also use facet_wrap with two variables

# Can use scales = "free" in facet_grid()/facet_wrap() if you want facets to not share same axis

#<<< END >>>


