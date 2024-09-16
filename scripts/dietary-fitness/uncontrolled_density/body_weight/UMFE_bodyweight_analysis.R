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
bodyweight_2 <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")

 # Multiplying by 1000 to get consistent with visualisation
bodyweight_2$weight_mg <- bodyweight_2$weight_mg * 1000







# Model 1 
#### Poisson GLMM ###
glmm.p.UMFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1| sample_number), family = poisson, data = bodyweight_2)

## Assumptions checks 

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.p.UMFE.weight, plot = T)
 # Pretty bad, most of the tests are significant 


# performance checks - easystats 

## Overdispersion test 
check_overdispersion(glmm.p.UMFE.weight)
 # Overdispersion detected

# Zeroinflation test
check_zeroinflation(glmm.p.UMFE.weight) 
 # No zero-inflation 







# Model 2 
#### Negative Binomial GLM ####
glm.nb.UMFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                            
                            , data = bodyweight_2)


# Assumption checks 

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.nb.UMFE.weight, plot = T)
 # The tests seem good, assumptions seem good 
 # This seems like the better model 


# Doing an AIC test for comparison 
AIC(glmm.p.UMFE.weight, glm.nb.UMFE.weight)
 # Negative Binomial GLM is a lot better





# Model 2 chosen

# Using the chosen model with a two-way interaction
glm.nb.UMFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                             
                             , data = bodyweight_2)



# Checking for significance in the two-way interaction
drop1(glm.nb.UMFE.weight, test = "Chisq")
  # No significant interaction


# Dropping the two-way interaction from the model
glm.nb.UMFE.weight.2  <- glm.nb(weight_mg ~ treatment + sex  
                             
                             , data = bodyweight_2)





#### Data analysis for write-up #### 

# Basic analysis 
summary(glm.nb.UMFE.weight.2)


# Confidence intervals 
exp(confint(glm.nb.UMFE.weight.2))


# Real values for write-up
emmeans::emmeans(glm.nb.UMFE.weight, specs = ~ sex + treatment, type = "response")

 
## Table of model for write-up
tab_model(glmm.p.UMFE.weight.2, CSS = list(css.table = '+font-family: Arial;'))


