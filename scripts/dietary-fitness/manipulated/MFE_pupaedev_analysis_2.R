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




pupae_fitness_MFE_filled <- pupae_fitness_MFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae))


pupae_fitness_MFE_2 <- uncount(pupae_fitness_MFE_filled, pupae)





#### Testing Models ####


# Model 1 
glmm.p.pupae <- glmmTMB(time_hours ~ treatment  + (1| vial), family = poisson, 
                        data = pupae_fitness_MFE_2)

#### ASSUMPTION CHECKS 
# DHARMa checks 
plot(simulateResiduals(glmm.p.pupae)) 
## Does not look great

## Performance checks
check_zeroinflation(glmm.p.pupae) ## There NO is zero inflation
check_overdispersion(glmm.p.pupae) ## There is over dispersion 



# Model 2
glm.nb_pupae <- glm.nb(time_hours ~ 
                         treatment   
                       
                       , data = pupae_fitness_MFE_2)



#### ASSUMPTION CHECKS 
# DHARMa checks 
plot(simulateResiduals(glm.nb_pupae)) 
## qq looks a lot better

## Performance checks
check_zeroinflation(glm.nb_pupae)
## No zeroinflation 
check_overdispersion(glm.nb_pupae) 
## No overdispersion 






## Comparing models:
AIC(glmm.p.pupae,glm.nb_pupae)
# Negative Binomial GLM is lower






## Using Negative Binomial GLM

# First check: for, two-way interaction. 
glm.nb_pupae <- glm.nb(time_hours  ~ 
                         treatment  
                       
                       , data = pupae_fitness_MFE_2)





## Data analysis
summary(glm.nb_pupae)

# Generating a table 
tab_model(glm.nb_pupae)












