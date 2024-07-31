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

#### Reading data in: ####
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")

# Model 1 
#### Poisson GLMM ###
glmm.p.MFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1|vial), family = poisson, data = bodyweight_MFE)

## qq plot from the model
residuals <- residuals(glmm.p.MFE.weight)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos

# performance checks 
check_overdispersion(glmm.p.MFE.weight)
# overdispersion
check_zeroinflation(glmm.p.MFE.weight) 
# No zero-inflation 


#### Chosen model: Poisson GLMM ####

drop1(glmm.p.MFE.weight, test = "Chisq")
# No interaction effect between treatment and sex

# If you want to change the intercept...
bodyweight_2$sex <- as.factor(bodyweight_2$sex)
bodyweight_2$sex <- relevel(bodyweight_2$sex, ref = "male")

# Final model: 
glmm.p.MFE.weight.2 <- glmmTMB(weight_mg ~ treatment + sex + (1|vial), family = poisson, data = bodyweight_MFE)



#### DATA ANALYSIS ####
summary(glmm.p.MFE.weight.2)
# Significant difference between Conditioned and Unconditioned!! 

## Table for model. 
tab_model(glmm.p.MFE.weight.2)


