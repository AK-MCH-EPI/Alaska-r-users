##### META #####################################################
#
# Date: 12/10/2020
# Author: Jared Parrish
# Project: AK R-users group
# Topic(s): Avoiding duplicated columns when merging in r &
#           the contains() function.
#
################################################################

library(dplyr)

set.seed(2020)
dat1 <- data.frame(ID = seq(1,20),
                   Event = sample(x = c("Yes","No"), size = 20, replace = TRUE),
                   Dist.norm = rnorm(n = 20, mean = 5, sd = 1.5),
                   Var1 = rep(c("T1","T2"), 10),
                   Var2 = sample(x = c("Slow","Fast","UNK"), size = 20, replace = TRUE),
                   var3 = sample(x = c("Young","Old"), size = 20, replace = TRUE),
                   Ind1 = sample (x = c(0,1), size = 20, replace = TRUE),
                   Ind2 = sample (x = c(0,1), size = 20, replace = TRUE),
                   Ind3 = sample (x = c(0,1), size = 20, replace = TRUE)
                   )
set.seed(2020)
dat2 <- data.frame(ID = seq(1,20),
                   Event = sample(x = c("Yes","No"), size = 20, replace = TRUE),
                   Dist.pois = rpois(n = 20, lambda = 1.5),
                   Var1 = rep(c("T1","T2"), 10))

# Data process 1: merge datasets but don't duplicate columns

# issue
m_dat1 <- merge(dat1, dat2, by.x = "ID", by.y = "ID", all.x = TRUE)
View(m_dat1)

# can solve multiple ways

# specify the column index to include
m_dat2 <- merge (dat1, dat2[,c(1,3)], by = "ID", all.x = TRUE)
View(m_dat2)

# specify the column index to exclude
m_dat2a <- merge (dat1, dat2[,-c(2,4)], by = "ID", all.x = TRUE)
View(m_dat2a)

# can specify the variable names to include
m_dat2b <- merge (dat1, dat2[,c("ID","Dist.pois")], by = "ID", all.x = TRUE)
View(m_dat2b)

# with lots of columns this could be a pain, one way to address this

dnames <- names(dat2[,-1]); dnames

dat1s <- dat1[setdiff(names(dat1), dnames)]; dat1s

m_dat3 <- merge (dat1s, dat2, by = "ID")
View(m_dat3)


# Data process 2: Want to recode all indicator variables as 1 = "Y", 0 = "N"

dat3 <- m_dat3 %>%
        mutate_at(vars(contains('Ind')), ~(ifelse(. == 1, "Y", 
                                          ifelse ( . == 0, "N", NA))))

### END example ###                