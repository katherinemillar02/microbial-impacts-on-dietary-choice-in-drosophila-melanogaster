


## Reading data in using read excel 
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")

## Multiplying the values by 1000, for better analysis and to get the plots to work... for consistency too 
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000



#### 1. Preliminary Data Analysis #### 


# Model 1 
#### Poisson GLMM ###
glmm.p.MFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1|vial), family = poisson, data = bodyweight_MFE)

## DHARMa residuals check 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.MFE.weight, plot = T)
# The model is not great with either qq or homogeneity tests 


# performance checks - easystats 

# Checking for overdispersion 
check_overdispersion(glmm.p.MFE.weight)
# There is overdispersion

# Checking for zeroinflation
check_zeroinflation(glmm.p.MFE.weight) 
# There is no zero-inflation 



## As there is overdispersion, trying a Negative Binomial GLM 

# Model 2
#### Negative Binomial GLM #### 
glm.nb.MFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                            
                            , data = bodyweight_MFE)


## DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.weight, plot = T)
# Model is better, but still get significant result for homogeneity


## Using this model for now 

#### 2. Data analysis with chosen model ####



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


#### Reading data in ####
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Separating the data into female and male columns 
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 


# as df
fly_fitness_tidy_MFE <- as.data.frame(fly_fitness_tidy_MFE)







## RUN ABOVE THIS !! 








#### side test
# Changing the intercept to see if the values actually change 
fly_fitness_tidy_MFE$sex <- as.factor(fly_fitness_tidy_MFE$sex)
fly_fitness_tidy_MFE$sex <- relevel(fly_fitness_tidy_MFE$sex, ref = "males")

# Model with the changed intercepts 
glmm.p.MFE.fly.3 <- glmmTMB(count ~ 
                              
                              sex * time_hours +
                              treatment * time_hours +
                              
                              
                              + (1|vial/sex) + (1|time_hours),
                            
                            family = poisson, data = fly_fitness_tidy_MFE)



drop1(glmm.p.MFE.fly.31, test = "Chisq")

tab_model(glmm.p.MFE.fly.3)

summary(glmm.p.MFE.fly.3)







#### 1. Preliminary Data Analysis ####
## Testing Models

# Model 1 
#### Poisson GLM ####
glm.p.MFE.fly <- glm(time_hours ~
                       treatment * sex  , 
                     
                     family = poisson, data = fly_fitness_tidy_MFE_2)


## Assumption checks 

# DHARMa assumption checks 
plot(simulateResiduals(glm.p.MFE.fly)) 
# Significant tests and assumptions are really quite bad 

# easystats assumption checks
performance::check_model(glm.p.MFE.fly, check = c("outliers")) 
# No outliers?
performance::check_model(glm.p.MFE.fly, check = c("homogeneity")) 
# The line is somewhat straight?

## Checking for overdispersion 
check_overdispersion(glm.p.MFE.fly) 
## Overdispersion detected






# Model 2 
#### Negative Binomial GLM ####
glm.nb.MFE.fly <- glm.nb(time_hours ~
                           
                           treatment * sex ,
                         
                         data = fly_fitness_tidy_MFE_2)


## Assumption checks 

# DHARMa assumption checks 
plot(simulateResiduals(glm.nb.MFE.fly)) 
# Less significant tests, assumptions are looking a lot better 


# performance - easystats 
performance::check_model(glm.nb.MFE.fly, check = c("outliers")) 
# No outliers?


performance::check_model(glm.nb.MFE.fly, check = c("homogeneity"))
# looks a bit weird, but line is somewhat straight


## Checking for overdispersion 
check_overdispersion(glm.nb.MFE.fly)
## No overdispersion detected 





# Model 3
#### Poisson GLMM ####
glmm.p.MFE.fly <- glmmTMB(time_hours ~ 
                            
                            treatment * sex
                          
                          + (1|sex/vial) ,
                          
                          family = poisson, data = fly_fitness_tidy_MFE_2)



# DHARMa assumption checks 
plot(simulateResiduals(glmm.p.MFE.fly)) 
# Assumptions are pretty bad 

check_overdispersion(glmm.p.MFE.fly)
# overdispersion detected




# Comparing models
AIC(glm.p.MFE.fly,glm.nb.MFE.fly, glmm.p.MFE.fly)
# NegBin the lowest by a little bit




#### Negative Binomial GLM ####
glm.nb.MFE.fly <- glm.nb(time_hours ~
                           
                           treatment * sex ,
                         
                         data = fly_fitness_tidy_MFE_2)





# Using drop1 to look for significance in a 3-way interaction
drop1(glm.nb.MFE.fly, test = "Chisq")
# No 2-way interaction effect






#### TESTING MODELS ####


# Model 1
#### Poisson GLMM ####
glmm.p.flysurvive.MFE <- glmmTMB(survivability ~ 
                                   
                                   treatment +
                                   
                                   (1|id) + (1|vial) ,
                                 
                                 family = poisson, data = fly_survivability)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.flysurvive.MFE , plot = T)
## Assumptions aren't great, new model maybe?
## qq doesn't really match at all... 



check_overdispersion(glmm.p.flysurvive.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.flysurvive.MFE)
## zero inflation 






# Model 2 
# Negative Binomial GLM
glm.nb.flysurvive.MFE <- glm.nb(survivability ~
                                  
                                  treatment,
                                
                                data = fly_survivability)




## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.flysurvive.MFE, plot = T)
## Assumptions aren't great, new model maybe? 
## qq still is not matching up great 


check_overdispersion(glm.nb.flysurvive.MFE)
# overderdispersion detected 

check_zeroinflation(glm.nb.flysurvive.MFE)
## zero inflation 







## Model 3 
glm.p.flysurvive.MFE <- glm(survivability ~
                              treatment, 
                            family = poisson, data = fly_survivability)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.p.flysurvive.MFE, plot = T)
## Assumptions aren't great, new model maybe? 
## qq still is not matching up great 


check_overdispersion(glm.p.flysurvive.MFE)
# Overderdispersion detected 

check_zeroinflation(glm.p.flysurvive.MFE)
## zero inflation 












#### Poisson GLMM ###
glmm.p.MFE.weight <- glmmTMB(weight_mg ~ treatment * sex + (1|vial), family = poisson, data = bodyweight_MFE)

## DHARMa residuals check 
simulationOutput <- simulateResiduals(fittedModel = glmm.p.MFE.weight, plot = T)
# The model is not great with either qq or homogeneity tests 


# performance checks - easystats 

# Checking for overdispersion 
check_overdispersion(glmm.p.MFE.weight)
# There is overdispersion

# Checking for zeroinflation
check_zeroinflation(glmm.p.MFE.weight) 
# There is no zero-inflation 



## As there is overdispersion, trying a Negative Binomial GLM 

# Model 2
#### Negative Binomial GLM #### 
glm.nb.MFE.weight <- glm.nb(weight_mg ~ treatment * sex  
                            
                            , data = bodyweight_MFE)


## DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.weight, plot = T)


######################## playing around with larvae - fly survivability data

## means 
conditioned_survivability <- fly_survivability %>%
  filter(treatment == "conditioned") %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(conditioned_survivability$mean_survivability)
 ## 19.6712


unconditioned_survivability <- fly_survivability %>%
  filter(treatment == "unconditioned") %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(unconditioned_survivability$mean_survivability)
 ##  17.3545




## medians
conditioned_survivability <- fly_survivability %>%
  filter(treatment == "conditioned") %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(conditioned_survivability$median_survivability)
## 23.80952


unconditioned_survivability <- fly_survivability %>%
  filter(treatment == "unconditioned") %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(unconditioned_survivability$median_survivability)
##  21.42857





## zeros removed 

## means 
conditioned_survivability <- fly_survivability %>%
  filter(treatment == "conditioned", , survivability != 0) %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(conditioned_survivability$mean_survivability)
#  22.94974


unconditioned_survivability <- fly_survivability %>%
  filter(treatment == "unconditioned" , survivability != 0) %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(unconditioned_survivability$mean_survivability)
##   22.6363





## medians
conditioned_survivability <- fly_survivability %>%
  filter(treatment == "conditioned", survivability != 0) %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(conditioned_survivability$median_survivability)
##  23.80952


unconditioned_survivability <- fly_survivability %>%
  filter(treatment == "unconditioned", survivability != 0) %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(unconditioned_survivability$median_survivability)
##   25.39683





######################## playing around with pupae - fly survivability data

## means 
conditioned_survivability <- survivability_between %>%
  filter(treatment == "conditioned") %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(conditioned_survivability$mean_survivability)
## 29.54995


unconditioned_survivability <- survivability_between %>%
  filter(treatment == "unconditioned") %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(unconditioned_survivability$mean_survivability)
## 25.74572




## medians
conditioned_survivability <- survivability_between %>%
  filter(treatment == "conditioned") %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(conditioned_survivability$median_survivability)
## 34.16667


unconditioned_survivability <- survivability_between %>%
  filter(treatment == "unconditioned") %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(unconditioned_survivability$median_survivability)
## 26.53061





## zeros removed 

## means 
conditioned_survivability <- survivability_between %>%
  filter(treatment == "conditioned", , survivability != 0) %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(conditioned_survivability$mean_survivability)
# 34.47495


unconditioned_survivability <- survivability_between %>%
  filter(treatment == "unconditioned" , survivability != 0) %>%
  summarise(mean_survivability = mean(survivability, na.rm = TRUE))

mean(unconditioned_survivability$mean_survivability)
##   33.58138





## medians
conditioned_survivability <- survivability_between %>%
  filter(treatment == "conditioned", survivability != 0) %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(conditioned_survivability$median_survivability)
##   36.14983


unconditioned_survivability <- survivability_between %>%
  filter(treatment == "unconditioned", survivability != 0) %>%
  summarise(median_survivability = median(survivability, na.rm = TRUE))

median(unconditioned_survivability$median_survivability)
## 39.02439





