## Larvae - Pupae 


#### Packages #### 
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)




## Reading pupae data in
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

#### Pupae data check. 
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = TRUE))


## Changing it to a dataframe
total_pupae <- as.data.frame(total_pupae)



## working out survivability code 
survivability_pupae <- total_pupae %>%
  mutate(fixed_total = 63, 
         survivability = (total_pupae / fixed_total) * 100)





#### Data Analysis #### 

# Model 1 
#### Poisson GLMM ####
glmm.p.pupaesurvive.MFE <- glmmTMB(survivability ~ 
                                     
                                     treatment +
                                     
                                     + (1|vial) + (1|id),
                                   
                                   family = poisson, data = survivability_pupae)


## Assumption checking:

# DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.pupaesurvive.MFE , plot = T)
## qq doesn't look too bad



# easystats checks 
check_overdispersion(glmm.p.pupaesurvive.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.pupaesurvive.MFE)
## NO zero inflation 






# Model 2 
#### Negative Binomial GLM ####
glm.nb.MFE.pupae <- glm.nb(survivability ~
                             
                             treatment,
                           
                           data = survivability_pupae)



## Assumption checking:

# DHARMa: 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.pupae, plot = T)
## Assumptions aren't great, new model maybe? 



# easystats
check_overdispersion(glm.nb.MFE.pupae)
# No Overderdispersion detected 

check_zeroinflation(glm.nb.MFE.pupae)
## Zero inflation 


## There is still zeroinflation, so trying zero inflation models... 
