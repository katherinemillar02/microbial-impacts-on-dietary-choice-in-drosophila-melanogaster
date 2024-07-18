# Overall Fly Emergence 
# Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


#### DATA ANALYSIS



#### Reading data in ####
fly_fitness <- read_excel("data/fitness_development/fly_data.xlsx")


## Separating the data into female and male columns 
fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 
     

# as df
fly_fitness_tidy <- as.data.frame(fly_fitness_tidy)







#### Testing Models ####

# Model 1 
#### Poisson GLM ####
glm.p.UMFE.fly <- glm(count ~
                         treatment * sex * time_hours , 
                       
                       family = poisson, data = fly_fitness_tidy)


#### ASSUMPTION CHECKS 
performance::check_model(glm.p.UMFE.fly, check = c("qq")) # doesn't show 
performance::check_model(glm.p.UMFE.fly, check = c("outliers")) # weird? 
performance::check_model(glm.p.UMFE.fly, check = c("homogeneity")) # think this looks alright 

## Checking for overdispersion 
check_overdispersion(glm.p.UMFE.fly) 
 ## overdispersion detected

## Checking for zeroinflation
check_zeroinflation(glm.p.UMFE.fly) 
## there is zero inflation 









# Model 2 
#### Negative Binomial GLM ####
glm.nb.UMFE.fly <- glm.nb(count ~
                       
                       treatment * sex * time_hours,
                     
                     data = fly_fitness_tidy)


#### ASSUMPTION CHECKS 
performance::check_model(glm.nb.UMFE.fly, check = c("qq")) # does not show. 
performance::check_model(glm.nb.UMFE.fly, check = c("outliers")) 
performance::check_model(glm.nb.UMFE.fly, check = c("homogeneity")) # looks a bit worse than glm poisson 


## Checking for overdispersion 
check_overdispersion(glm.nb.UMFE.fly)
 ## Underdispersion detected









# Model 3
#### Poisson GLMM ####
glmm.p.UMFE.fly <- glmmTMB(count ~ 
                             
                             treatment * sex * time_hours
                           
                           + (1|sex/vial) + (1|time_hours),
                           
                           family = poisson, data = fly_fitness_tidy)


## qq plot from the model
residuals <- residuals(glmm.p.UMFE.fly)
qqnorm(residuals)
qqline(residuals, col = 2)

## Performance
check_zeroinflation(glmm.p.UMFE.fly)
check_overdispersion(glmm.p.UMFE.fly)





# Model 4 
#### Zero-Inflated Poisson ####
zi.p.UMFE.fly <- zeroinfl(count ~ 
                              
                              treatment * time_hours * sex  
                            
                            , data = fly_fitness_tidy)







# Comparing models
AIC(glm.p.UMFE.fly,glm.nb.UMFE.fly, glmm.p.UMFE.fly, zi.p.UMFE.fly)






#### Chosen model: Poisson GLMM ####
glmm.p.UMFE.fly <- glmmTMB(count ~ 
                             
                             treatment * sex * time_hours
                           
                           + (1|vial/sex) + (1|time_hours),
                           
                           family = poisson, data = fly_fitness_tidy)





# Using drop1 to look for significance in a 3-way interaction
drop1(glmm.p.UMFE.fly, test = "Chisq")
 # 3-way interaction effect




#### DATA ANALYSIS ####
summary(glmm.p.UMFE.fly)


