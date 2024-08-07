#### Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)


## Larve to Fly Survivability: 

#### Reading data in: ####
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 



#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  group_by(id, vial, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) 


## making it a dataframe 
overallflies_MFE <- as.data.frame(overallflies_MFE)


## making a surviability data frame 
fly_survivability <- overallflies_MFE %>%
  mutate(fixed_total = 63,  # Add a fixed total of 63
         survivability = (total_count / fixed_total)*100 )





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
# Overderdispersion detected 

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





# Model 4 
#### Zero-Inflated Poisson ####

## changing to integer to use a zeroinfl model
fly_survivability$survivability <- as.integer(fly_survivability$survivability)

# Model
zi.p.flysurvive.MFE <- zeroinfl(survivability ~ 
                           
                           treatment 
                         
                         , data = fly_survivability) 


## assumption checks
residuals <- residuals(zi.p.flysurvive.MFE)

# Create QQ plot
qqnorm(residuals, main="QQ Plot of Residuals")
qqline(residuals)


# I think it looks better? 

AIC(zi.p.flysurvive.MFE, glm.p.flysurvive.MFE, glm.nb.flysurvive.MFE, glmm.p.flysurvive.MFE)

 ## zeroinfl has best assumptions



# Using Model
zi.p.flysurvive.MFE <- zeroinfl(survivability ~ 
                                  
                                  treatment 
                                
                                , data = fly_survivability) 


summary(zi.p.flysurvive.MFE)
## no survivability difference 

## table: 
tab_model(zi.p.flysurvive.MFE)




################################################################################





#### Reading data in: ####
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 



#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  group_by(id, vial, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) 

overallflies_MFE <- as.data.frame(overallflies_MFE)

fly_survivability <- overallflies_MFE %>%
  mutate(fixed_total = 63,  # Add a fixed total of 63
         survivability = (total_count / fixed_total)*100 )


#### TESTING MODELS ####


# Model 1
#### Poisson GLMM ####
glmm.p.total.MFE <- glmmTMB(survivability ~ 
                              
                              treatment +
                              
                              (1|id) + (1|vial) ,
                            
                            family = poisson, data = fly_survivability)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.total.MFE , plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glmm.p.total.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.total.MFE)
## zero inflation 






# Model 2 
# Negative Binomial GLM
glm.nb.MFE.flies <- glm.nb(survivability ~
                             
                             treatment,
                           
                           data = fly_survivability)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.flies, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glm.nb.MFE.flies)
# Overderdispersion detected 

check_zeroinflation(glm.nb.MFE.flies)
## zero inflation 







#### Poisson GLMM ####
glmm.p.flysurvive.MFE <- glmmTMB(survivability ~ 
                                   
                                   treatment +
                                   
                                   + (1|vial) + (1|id),
                                 
                                 family = poisson, data = fly_survivability)

summary(glmm.p.flysurvive.MFE)
## no survivability difference 

exp(confint(glmm.p.flysurvive.MFE))


## table: 
tab_model(glmm.p.flysurvive.MFE)



## Reading pupae data in
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

#### Pupae data check. 
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = TRUE))


## Changing it to a dataframe
total_pupae <- as.data.frame(total_pupae)

survivability_pupae <- total_pupae %>%
  mutate(fixed_total = 63,  # Add a fixed total of 63
         survivability = (total_pupae / fixed_total) * 100)


glmm.p.pupaesurvive.MFE <- glmmTMB(survivability ~ 
                                   
                                   treatment +
                                   
                                   + (1|vial) + (1|id),
                                 
                                 family = poisson, data = survivability_pupae)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.pupaesurvive.MFE , plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glmm.p.pupaesurvive.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.pupaesurvive.MFE)
## NO zero inflation 



# Negative Binomial GLM
glm.nb.MFE.pupae <- glm.nb(survivability ~
                             
                             treatment,
                           
                           data = survivability_pupae)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.pupae, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glm.nb.MFE.pupae)
# No Overderdispersion detected 

check_zeroinflation(glm.nb.MFE.pupae)
## zero inflation 


AIC(glm.nb.MFE.pupae, glmm.p.pupaesurvive.MFE)


glm.nb.MFE.pupae <- glm.nb(survivability ~
                             
                             treatment,
                           
                           data = survivability_pupae)

summary(glm.nb.MFE.pupae)
exp(confint(glm.nb.MFE.pupae))






tab_model(glm.nb.MFE.pupae)


both <- overallflies_MFE %>%
  left_join(total_pupae, by = c("id", "vial", "treatment"))
both



survivability_between <- both %>%
  mutate(survivability = (total_count / total_pupae) * 100)


glmm.p.bothsurvive.MFE <- glmmTMB(survivability ~ 
                                     
                                     treatment +
                                     
                                     + (1|vial) + (1|id),
                                   
                                   family = poisson, data = survivability_between)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.bothsurvive.MFE , plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glmm.p.bothsurvive.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.bothsurvive.MFE)
##  zero inflation 




# Negative Binomial GLM
glm.nb.MFE.both <- glm.nb(survivability ~
                             
                             treatment,
                           
                           data = survivability_between)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.both, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glm.nb.MFE.both)
# No Overderdispersion detected 

check_zeroinflation(glm.nb.MFE.both)
## zero inflation 

AIC(glmm.p.bothsurvive.MFE, glm.nb.MFE.both)


glmm.p.bothsurvive.MFE <- glmmTMB(survivability ~ 
                                    
                                    treatment +
                                    
                                    + (1|vial) + (1|id),
                                  
                                  family = poisson, data = survivability_between)


summary(glmm.p.bothsurvive.MFE)

exp(confint(glmm.p.bothsurvive.MFE))

