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

#### Reading and cleaning data ####

## Reading data in using read excel 
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")

## Multiplying the values by 1000, for better analysis and to get the plots to work... for consistency too 
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000



#### 1. Preliminary Data Analysis #### 


# Model 1 
#### Poisson GLMM ###
glmm.p.MFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1|vial), family = poisson, data = bodyweight_MFE)

## DHARMa residuals check 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.MFE.weight, plot = T)s
 # The model is not great with either qq or homogeneity tests 


# performance checks - easystats 

# Checking for overdispersion 
check_overdispersion(glmm.p.MFE.weight)
# There is overdispersion

# Checking for zeroinflation
check_zeroinflation(glmm.p.MFE.weight) 
# There is no zero-inflation 



## As there is overdispersion, trying a Negative Binomial GLM 

# Model 2
#### Negative Binomial GLM #### 
glm.nb.MFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                               
                               , data = bodyweight_MFE)


## DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.weight, plot = T)
 # Model is better, but still get significant result for homogeneity


## Using this model for now 

#### 2. Data analysis with chosen model ####

## Testing for a two-way interaction effect 
glm.nb.MFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                            
                            , data = bodyweight_MFE)

## Looking for significance in the two-way interaction effect using drop1()
drop1(glm.nb.MFE.weight, test = "Chisq")
 # No significance 

## Using the model without the two-way interaction effect 
glm.nb.MFE.weight.2 <- glm.nb(weight_mg ~ treatment + sex  
                            
                            , data = bodyweight_MFE)


##### Data analysis for write-up ####

# Basic analysis 
summary(glm.nb.MFE.weight.2)

## Looking for confidence intervals 
exp(confint(glm.nb.MFE.weight.2))


## Real values for write-up
emmeans::emmeans(glm.nb.MFE.weight.2, specs =  ~ treatment + sex , type = "response")


## Table of model for write-up
tab_model(glm.nb.MFE.weight.2, CSS = list(css.table = '+font-family: Arial;'))





emmeans::emmeans(glm.nb.MFE.weight.2, specs = ~ sex + treatment, type = "response")


