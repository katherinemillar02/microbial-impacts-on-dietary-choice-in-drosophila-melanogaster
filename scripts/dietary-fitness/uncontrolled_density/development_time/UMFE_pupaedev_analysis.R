
#### Chapter 4 ####

 # Uncontrolled Density

#### Packages 📦📦📦📦 ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)


#### Reading the data in, making total data set: ####

# Reading excel file in 
pupae_fitness_UMFE <- read_excel("data/fitness_development/puape_data.xlsx")

## Working out the total sum of pupae: 
total_pupae <- pupae_fitness_UMFE %>% 
  group_by(treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = TRUE))




# Model 1 
#### Poisson GLMM ####
glmm.p.UMFE.pupae <- glmmTMB(pupae
                             ~ treatment * `time (hours)`
                             
                             + (1| vial), family = poisson, data = pupae_fitness_UMFE)

## Assumption Checking
# DHARMa checks 
plot(simulateResiduals(glmm.p.UMFE.pupae)) 
  ## This looks quite bad, qq doesn't line up, residuals are everywhere.
  # Significant tests

# Performance checks
check_zeroinflation(glmm.p.UMFE.pupae)
  ## There is zero inflation
check_overdispersion(glmm.p.UMFE.pupae) 
  ## There is over dispersion 




# Model 2 
#### Negative Binomial GLM ####
glm.nb.UMFE.pupae <- glm.nb(pupae ~ 
                              
                         treatment * `time (hours)`  
                         
                       , data = pupae_fitness_UMFE)



# DHARMa checks 
plot(simulateResiduals(glm.nb.UMFE.pupae)) 
  ## qq plot is better, but residuals are still dodgy.


# Performance checks: 
check_zeroinflation(glm.nb.UMFE.pupae) 
  ## There is zero inflation

check_overdispersion(glm.nb.UMFE.pupae) 
  ## There is NO over dispersion 



# There is no longer overdispersion, 
# but there is still zero inflation, so testing zero-inflation models 




# Model 3 
#### Zero-Inflated Poisson ####


zi.p.UMFE.pupae <- zeroinfl(pupae ~ 
                              
                              treatment * `time (hours)`  
                            
                            , data = pupae_fitness_UMFE)


# Two-way interaction found
drop1(zi.p.UMFE.pupae, test = "Chisq")


# Final model #

#### DATA ANALYSIS ####
summary(zi.p.UMFE.pupae)


## Table 
tab_model(zi.p.UMFE.pupae)


