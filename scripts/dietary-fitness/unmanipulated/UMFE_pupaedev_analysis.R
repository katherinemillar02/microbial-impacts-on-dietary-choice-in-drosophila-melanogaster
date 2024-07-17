# Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


#### Reading the data in: ####
pupae_fitness_UMFE <- read_excel("data/fitness_development/puape_data.xlsx")


# Model 1 
#### Poisson GLMm ####
glmm.p.UMFE.pupae <- glmmTMB(pupae
                             ~ treatment * `time (hours)`
                             
                             + (1| vial), family = poisson, data = pupae_fitness_UMFE)

## Assumption Checking
# DHARMa checks 
plot(simulateResiduals(glmm.p.UMFE.pupae)) 
 ## This looks quite bad, qq doesn't line up, residuals are everywhere.

# Performance checks
check_zeroinflation(glmm.p.UMFE.pupae)
  ## There is zero inflation
check_overdispersion(glmm.p.UMFE.pupae) 
  ## There is over dispersion 




# Model 2 
#### Negative Binomial GLM ####
glm.nb.UMFE.pupae <- glm.nb(pupae ~ 
                              
                         treatment * `time (hours)`  
                         
                       + (1| vial), data = pupae_fitness_UMFE)



# DHARMa checks 
plot(simulateResiduals(glm.nb.UMFE.pupae)) 
  ## qq plot is better, but residuals are still dodgy.


# Performance checks: 
check_zeroinflation(glm.nb.UMFE.pupae) 
  ## There is zero inflation

check_overdispersion(glm.nb.UMFE.pupae) 
  ## There is NO over dispersion 



# There is still zero inflation, so testing zero-inflation models 




# Model 3 
#### Zero-Inflated Poisson ####
zi.p.UMFE.pupae <- zeroinfl(pupae