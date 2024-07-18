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



## 
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

AIC(glmm.p.pupae,glm.nb_pupae)
## NB GLM a lot better





glm.nb_pupae <- glm.nb(pupae ~ 
                         treatment * time_hours  
                       
                       , data = pupae_fitness_MFE)

drop1(glm.nb_pupae, test = "F")

glm.nb_pupae.2 <- glm.nb(pupae ~ 
                         treatment + time_hours  
                       
                       , data = pupae_fitness_MFE)

summary(glm.nb_pupae.2)

#  No interaction effect... 


#### Nothing seems to be significant? 





# zero inflation 
# Model 3 
#### Zero-Inflated Poisson ####
zi.p.MFE.pupae <- zeroinfl(pupae ~ 
                              
                              treatment * time_hours  
                            
                            , data = pupae_fitness_MFE)


drop1(zi.p.MFE.pupae, test = "Chisq")
summary(zi.p.MFE.pupae)



AIC(glmm.p.pupae,glm.nb_pupae,zi.p.MFE.pupae)





