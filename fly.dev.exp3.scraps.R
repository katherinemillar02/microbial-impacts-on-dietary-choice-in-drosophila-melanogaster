#### scraps for fly development 3 
## Uploading Packages using source
library(ggpubr)
source("packages.R")


#### 1. Preliminary Data Analysis ####
## Testing Models

# Model 1 
#### Poisson GLM ####
glm.p.adulttraits.fly <- glm(time_hours ~
                               treatment * sex  , 
                             
                             family = poisson, data = fly.dev.3.tidy)




## Assumption checks 

# DHARMa assumption checks 
plot(simulateResiduals(glm.p.adulttraits.fly)) 
# Significant tests and assumptions are really quite bad 

# easystats assumption checks
performance::check_model(glm.p.adulttraits.fly, check = c("outliers")) 
# No outliers?
performance::check_model(glm.p.adulttraits.fly, check = c("homogeneity")) 
# The line is somewhat straight?

## Checking for overdispersion 
check_overdispersion(glm.p.adulttraits.fly) 
## No overdispersion detected






## Assumption checks 



# Model 2
#### Poisson GLMM ####
glmm.p.adulttraits.fly <- glmmTMB(time_hours ~ 
                                    
                                    treatment * sex
                                  
                                  + (1|sex/vial) ,
                                  
                                  family = poisson, data = fly.dev.3.tidy)



# DHARMa assumption checks 
plot(simulateResiduals(glmm.p.adulttraits.fly)) 
# Assumptions are pretty bad 

check_overdispersion(glmm.p.adulttraits.fly)
# No overdispersion detected




# Comparing models
AIC(glm.p.adulttraits.fly, glmm.p.adulttraits.fly)
# poisson bad but what else>

)


## Assumption checks 

# DHARMa assumption checks 
plot(simulateResiduals(glm.p.MFE.fly.adulttraits)) 
# Significant tests and assumptions are really quite bad 

# easystats assumption checks
performance::check_model(glm.p.MFE.fly.adulttraits, check = c("outliers")) 
# No outliers?
performance::check_model(glm.p.MFE.fly.adulttraits, check = c("homogeneity")) 
# The line is somewhat straight?

## Checking for overdispersion 
check_overdispersion(glm.p.MFE.fly.adulttraits) 
## No overdispersion detected






## Assumption checks 



# Model 2
#### Poisson GLMM ####
glmm.p.adulttraits.fly <- glmmTMB(time_hours ~ 
                                    
                                    treatment * sex
                                  
                                  + (1|sex/vial) ,
                                  
                                  family = poisson, data = fly_fitness_tidy_adulttraits_2)



# DHARMa assumption checks 
plot(simulateResiduals(glmm.p.adulttraits.fly)) 
# Assumptions are pretty bad 

check_overdispersion(glmm.p.adulttraits.fly)
# No overdispersion detected




# Comparing models
AIC(glm.p.adulttraits.fly, glmm.p.adulttraits.fly)
# poisson bad but what else>

