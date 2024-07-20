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

# Model 1 
glmm.p.UMFE.weight <- glmmTMB(weight_mg ~ treatment * sex, family = poisson, data = bodyweight_2)

## qq plot from the model
residuals <- residuals(glmm.p.UMFE.weight)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos

# performance checks 
check_overdispersion(glmm.p.UMFE.weight)
 # No overdispersion
check_zeroinflation(glmm.p.UMFE.weight) 
 # No zero-inflation 


#### Chosen model: Poisson GLMM ####

drop1(glmm.p.UMFE.weight, test = "Chisq")
 # No interaction effect



# Final model: 
glmm.p.UMFE.weight.2 <- glmmTMB(weight_mg ~ treatment + sex, family = poisson, data = bodyweight_2)

bodyweight_2$sex <- as.factor(bodyweight_2$sex)
bodyweight_2$sex <- relevel(bodyweight_2$sex, ref = "male")

#### DATA ANALYSIS ####
summary(glmm.p.UMFE.weight.2)
 # Significant difference between Conditioned and Unconditioned!! 
 
tab_model(glmm.p.UMFE.weight.2)
bodyweight_2$sex <- as.factor(bodyweight_2$sex)
bodyweight_2$sex <- relevel(bodyweight_2$sex, ref = "male")

