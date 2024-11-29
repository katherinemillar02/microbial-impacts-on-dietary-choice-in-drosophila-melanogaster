#### Packages ####
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
##################
## Reading the data in
lifespan_adultstraits <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx")


## adding a sex section 
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")

## ANALYSIS 

# Model 1 
#### Poisson GLMM ###
glmm.p.adulttraits.lifespan <- glmmTMB(days_alive ~ Conditioning * Sex + (1|vial), family = poisson, data = lifespan_adultstraits)


## DHARMa residuals check 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.adulttraits.lifespan, plot = T)
# some errors with the plot, negative tests - 


# performance checks - easystats 

# Checking for overdispersion 
check_overdispersion(glmm.p.adulttraits.lifespan)
# nO overdispersion

# Checking for zeroinflation
check_zeroinflation(glmm.p.adulttraits.lifespan) 
# There is no zero-inflation 

# Testing model with interaction effect
glmm.p.adulttraits.lifespan <- glmmTMB(days_alive ~ Conditioning * Sex + (1|vial), family = poisson, data = lifespan_adultstraits)

# testing significance of interaction effect
drop1(glmm.p.adulttraits.lifespan, test  = "Chisq")

# new model without interaction effect
glmm.p.adulttraits.lifespan.2 <- glmmTMB(days_alive ~ Conditioning + Sex + (1|vial), family = poisson, data = lifespan_adultstraits)

# data analysis
summary(glmm.p.adulttraits.lifespan.2 )
 # no differences in days alive between treatments
 # differences in sex 
 # but censor not considered
