#### Meta ######################################################################
#
# Project: R - users group
# Date: 10/19/2021
# Author: Jared Parrish
# Notes: Custom functions
#
#

#### Start Code ################################################################
library(dplyr)
library(ggplot2)

## Basic Syntax for writing functions in R
#  FunctionName <- function(arguments){
#                  function_body
#                  return()
#  }
#

## Question: what are some reasons we would build or use functions?
#  - logically break our code into simpler parts
#  - Reduce redundant code
#  - Reduce error
#  - others...?

# For SAS users think of Functions as Macros


#### 1st basic example #########################################################

pow <- function(x,y) {
  #function to print x raised to the power y
  result <- x^y
  print(paste(x,"raised to the power",y, "is",result))
}

#use basic function
pow(8,2)
pow(2,8)

#### 2nd basic example #########################################################

div_check <- function(x){
  if(x%%3==0){
    result <- "The number IS divisible by three"
  }else {
    result <- "The number IS NOT divisible by three"
  }
  return(result)
}

div_check(21)
div_check(100)

#### 3rd basic example #########################################################

data("USArrests")
head(USArrests,10)

# Goal calculate the max or min by arrest type, specify the state, 
# return values, and plot

state_disc <- function(dataset, col, stat = c(max,min)){
  dat <- dataset
  t.dat <- dat %>% filter({{col}} == stat({{col}}))
  
  ST <- rownames(t.dat)
  
  tsdat <- as.data.frame(t(t.dat))
    tsdat$arrest.type <- rownames(tsdat)
    names(tsdat)[1:2] <- c("State","ArrestType")
  
    s.plot <- ggplot(tsdat, aes(x = ArrestType, y = State, fill = State)) +
              geom_bar(stat = "identity")
    
    s.list <- list(state = ST, data = tsdat, plot = s.plot)
    
  return(s.list)
}

# use function.
state_disc(dataset = USArrests, col = Murder, stat = max)
state_disc(dataset = USArrests, col = Murder, stat = min)

state_disc(dataset = USArrests, col = Rape, stat = max)
state_disc(dataset = USArrests, col = Rape, stat = min)

state_disc(dataset = USArrests, col = Assault, stat = max)
state_disc(dataset = USArrests, col = Assault, stat = min)

state_disc(dataset = USArrests, col = UrbanPop, stat = max)
state_disc(dataset = USArrests, col = UrbanPop, stat = min)


## END EXAMPLE