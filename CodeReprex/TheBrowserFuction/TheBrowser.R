#### Meta ----------------------------------------------------------------------
# Example from survival and survey package
# Applying Dr. Lumley's (author of survey package) answer to the memory issue with calculating
# the SE with svykm().
# 
# This demonstrates how to troubleshoot a function using the browser() object.

#### Data set up ---------------------------------------------------------------
library(survey)
library(dplyr)
library(survival)
library(ggplot2)
#Shut off scientific notation
options(scipen=999)

#load/make data
data(pbc, package="survival")                         #Load data
pbc$status <- with(pbc,ifelse(status == 2, 1,0))      #make outcome dicot.
pbc$randomized <- with(pbc, !is.na(trt) & trt>0)      #Create new variable
biasmodel<-glm(randomized~age*edema,data=pbc)         #Fit model 
pbc$randprob<-fitted(biasmodel)                       #Create random probabilities for psudo oversampling example

dpbc<-svydesign(id=~1, prob=~randprob, strata=~edema, data=subset(pbc,randomized)) # set survey design
dpbc<-update(dpbc, died=status==1) # update design based on default

#Import CI functions
source("R:\\SCANlink\\ALCANLink\\ALCANLinkCode\\SvyIncProp_Function.R")
View(svyIncProp)

glimpse(subset(pbc,randomized))

Tempdat <- svyIncProp(SurveyDesign = dpbc,
                      event = "status",
                      eventime = "time",
                      nreps = 100)

## plot intervals
ggplot(data = Tempdat) +
  geom_ribbon(aes(x=time,ymin=empirical.lowerCI,ymax=empirical.upperCI),fill = "red", alpha=0.2)+ #shifts up towards tail
  geom_ribbon(aes(x=time,ymin=Ft.lowerCI,ymax=Ft.upperCI),fill = "green",alpha=0.2)+ #shifts up towards tail, close to rp1 above
  
  geom_step(aes(y=Ft.orig,x=time),size=1.0) +
  geom_step(aes(y=Ft.repl,x=time,),size = 1, color = "green",alpha=0.9)+
  geom_step(aes(y=empirical.median,x=time,),size = 1, color = "red",alpha=0.9)+
  geom_step(aes(y=empirical.mean,x=time,),size = 1, color = "purple",alpha=0.9)+
  
  theme_minimal(base_size = 15) +
  theme(legend.position = "top",legend.title = element_blank(),
        text=element_text(size=15))+
  ylab("Incidence Proportion F(t)")+
  xlab("Age in days") +
  labs(title = "Original vs. cred. int.")




