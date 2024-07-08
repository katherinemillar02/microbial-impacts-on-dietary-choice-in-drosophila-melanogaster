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

## Reading data in ##
fly_fitness <- read_excel("data//fitness_development/fly_data.xlsx")

############

## Separating the data into female and male columns 
fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 


## Starting with a basic model 

## Basic glm poisson 
glm_poisson_fly <- glm(count ~ treatment * sex, family = poisson, data = fly_fitness_tidy)


#### ASSUMPTION CHECKS 
performance::check_model(glm_poisson_fly, check = c("qq")) # doesn't show 
performance::check_model(glm_poisson_fly, check = c("outliers")) # weird? 
performance::check_model(glm_poisson_fly, check = c("homogeneity")) # think this looks alright 

## Checking for overdispersion 
check_overdispersion(glm_poisson_fly) ## overdispersion detected

## Checking for zeroinflation
check_zeroinflation(glm_poisson_fly) ## there is zero inflation 


## Trying a negative binomial model
glm.nb_fly <- glm.nb(count ~ treatment * sex, fly_fitness_tidy)


#### ASSUMPTION CHECKS 
performance::check_model(glm.nb_fly, check = c("qq")) # does not show. 
performance::check_model(glm.nb_fly, check = c("outliers")) 
performance::check_model(glm.nb_fly, check = c("homogeneity")) # looks a bit worse than glm poisson 

## Checking for overdispersion 
check_overdispersion(glm.nb_fly) ## overdispersion NO LONGER detected


## Trying a mixed model 
glm_mm_fly <- glmmTMB(count ~ treatment * sex + (1|sex/vial) + (1|time_hours), family = poisson, data = fly_fitness_tidy)


## qq plot from the model
residuals <- residuals(glm_mm_fly)
qqnorm(residuals)
qqline(residuals, col = 2)


## testing for an interaction effect 
drop1(glm_mm_fly, test = "Chi") ## no interaction effect

# model without interaction
glm_mm_fly <- glmmTMB(count ~ treatment + sex + (1|sex/vial) + (1|time_hours), family = poisson, data = fly_fitness_tidy)

summary(glm_mm_fly)







## Flies 

fly_model <- glmmTMB(count ~ treatment * time_hours + sex + (1| vial), family = poisson, data =  fly_fitness_tidy)

summary(fly_model)

## Assumption checks 

# DHARMa checks 
plot(simulateResiduals(fly_model)) ## doesn't look too bad? 

check_zeroinflation(fly_model) ## There is zero inflation

check_overdispersion(fly_model) ## There is over dispersion 

### Doing negative binomial 

glm.nb_fly <- glm.nb(count ~ 
                       treatment : time_hours : sex 
                     + treatment : time_hours 
                     + treatment : sex
                     + time_hours : sex 
                     + treatment + time_hours + sex 
                     + (1| vial), data = fly_fitness_tidy)


drop1(glm.nb_fly, test = "F")

glm.nb_fly.2 <- glm.nb(count ~ 
                      
                      treatment : time_hours 
                     + treatment : sex
                     + time_hours : sex 
                     + treatment + time_hours + sex 
                     + (1| vial), data = fly_fitness_tidy)

drop1(glm.nb_fly.2, test = "F")


glm.nb_fly.3 <- glm.nb(count ~ 
                         
                         treatment : time_hours 
                       
                       + time_hours : sex 
                       + treatment + time_hours + sex 
                       + (1| vial), data = fly_fitness_tidy)

drop1(glm.nb_fly.3, test = "F")



summary(glm.nb_fly.3)


# DHARMa checks 
plot(simulateResiduals(glm.nb_fly)) ## looks quite okay

# easystats checks 
check_zeroinflation(glm.nb_fly) ## No zero inflation

check_overdispersion(glm.nb_fly) ## There is UNDER dispersion 

## Doing zero inflation model 

zi.p_fly <- zeroinfl(count ~ treatment * time_hours * sex | treatment * time_hours * sex, dist = "poisson", link = "logit", data = fly_fitness_tidy)

## cannot do DHARMa checks 


AIC(fly_model,glm.nb_fly, zi.p_fly)

## Best is glm.nb_fly by quite a bit, - but use anyway? 

zi.p_fly <- zeroinfl(count ~ treatment * time_hours * sex | treatment * time_hours * sex, dist = "poisson", link = "logit", data = fly_fitness_tidy)

zi.p_fly.2 <- zeroinfl(count ~ 
                         treatment : time_hours : sex 
                       + treatment : time_hours 
                       + treatment : sex
                       + time_hours : sex 
                       + treatment + time_hours + sex 
            
                       |   treatment : time_hours : sex 
                       + treatment : time_hours 
                       + treatment : sex
                       + time_hours : sex 
                       + treatment + time_hours + sex 
                       , dist = "poisson", link = "logit", data = fly_fitness_tidy)


drop1(zi.p_fly.2, test = "F")

zi.p_fly_2 <- zeroinfl(count ~ treatment + time_hours + sex | treatment + time_hours + sex, dist = "poisson", link = "logit", data = fly_fitness_tidy)

summary(zi.p_fly_2)
