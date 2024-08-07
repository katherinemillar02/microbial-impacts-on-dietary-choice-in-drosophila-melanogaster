# Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)


#### Reading the data in: ####
pupae_fitness_UMFE <- read_excel("data/fitness_development/pupae_data.xlsx")


## Code which will remove count
pupae_fitness_tidy_filled <- pupae_fitness_UMFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae))

pupae_fitness_UMFE_2 <- uncount(pupae_fitness_tidy_filled, pupae)


# Model 1 
#### Poisson GLMM ####
glmm.p.UMFE.pupae <- glmmTMB( time_hours
                             ~ treatment 
                             
                             + (1| vial) + (1| time_hours), family = poisson, data = pupae_fitness_UMFE_2)

## Assumption Checking
# DHARMa checks 
plot(simulateResiduals(glmm.p.UMFE.pupae)) 
## This looks quite bad, qq doesn't line up, residuals are everywhere.

# Performance checks
check_zeroinflation(glmm.p.UMFE.pupae)
## No zero inflation
check_overdispersion(glmm.p.UMFE.pupae) 
## No overdispersion 




# Model 2 
#### Negative Binomial GLM ####
glm.nb.UMFE.pupae <- glm.nb(time_hours ~ 
                              
                              treatment  
                            
                            , data = pupae_fitness_UMFE_2)



# DHARMa checks 
plot(simulateResiduals(glm.nb.UMFE.pupae)) 
## qq plot is better, but residuals are still dodgy.


# Performance checks: 
check_zeroinflation(glm.nb.UMFE.pupae) 
## No zero inflation

check_overdispersion(glm.nb.UMFE.pupae) 
##  NO over dispersion 





AIC(glmm.p.UMFE.pupae, glm.nb.UMFE.pupae)




# chosen model: 
glmm.p.UMFE.pupae <- glmmTMB( time_hours
                              ~ treatment 
                              
                              + (1| vial) , family = poisson, data = pupae_fitness_UMFE_2)




# Final model #

#### DATA ANALYSIS ####
summary(glmm.p.UMFE.pupae)

tab_model(glmm.p.UMFE.pupae)


exp(confint(glmm.p.UMFE.pupae))

