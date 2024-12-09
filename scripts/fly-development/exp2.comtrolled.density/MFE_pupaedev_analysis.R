#### MFE Pupae Analysis ####


#### Packages ####
# Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)



## Reading pupae data in
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")


#### Testing Models ####


# Model 1 
glmm.p.pupae <- glmmTMB(pupae ~ treatment * time_hours + (1| vial), family = poisson, data = pupae_fitness_MFE)

#### ASSUMPTION CHECKS 
# DHARMa checks 
plot(simulateResiduals(glmm.p.pupae)) 
 ## Does not look great

## Performance checks
check_zeroinflation(glmm.p.pupae) ## There is zero inflation
check_overdispersion(glmm.p.pupae) ## There is over dispersion 



# Model 2
glm.nb_pupae <- glm.nb(pupae ~ 
                         treatment * time_hours  
                       
                       , data = pupae_fitness_MFE)



#### ASSUMPTION CHECKS 
# DHARMa checks 
plot(simulateResiduals(glm.nb_pupae)) 
## qq looks a lot better

## Performance checks
check_zeroinflation(glm.nb_pupae)
 ## No zeroinflation 
check_overdispersion(glm.nb_pupae) 
 ## No overdispersion 


# Zero Inflation 
# Model 3 
#### Zero-Inflated Poisson ####
zi.p.MFE.pupae <- zeroinfl(pupae ~ 
                             
                             treatment * time_hours  
                           
                           , data = pupae_fitness_MFE)

## Interaction effects: 
drop1(zi.p.MFE.pupae, test = "Chisq")

#### DATA ANALYSIS ####
summary(zi.p.MFE.pupae) 



## Comparing models:
AIC(glmm.p.pupae,glm.nb_pupae,zi.p.MFE.pupae)

## Comparing the models: 
AIC(glmm.p.pupae,glm.nb_pupae)
## Negative Binomial GLM a lot better




## Using Negative Binomial GLM

# First check: for, two-way interaction. 
glm.nb_pupae <- glm.nb(pupae ~ 
                         treatment * time_hours  
                       
                       , data = pupae_fitness_MFE)


## Using drop1 to see if the two-way interaction is significant. 
drop1(glm.nb_pupae, test = "F")
  # two-way interaction is not significant 





## Removing the 2-way interaction 
# Final model?: 
glm.nb_pupae.2 <- glm.nb(pupae ~ 
                         treatment + time_hours  
                       
                       , data = pupae_fitness_MFE)


## Data analysis
summary(glm.nb_pupae.2)

# Generating a table 
tab_model(glm.nb_pupae.2)












