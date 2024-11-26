
      #### Chapter 3 - Development ####

#### Packages 📦📦📦📦 ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)


#### Reading data in, and cleaning ####

## Reading data in with readexcel! 
fly_fitness <- read_excel("data/fitness_development/fly_data.xlsx")



# Separating the data into "female" and "male" columns 
fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 
# As df
fly_fitness_tidy <- as.data.frame(fly_fitness_tidy)


## This code will turn NA into 0 - so uncount can be used 
fly_fitness_tidy  <- fly_fitness_tidy %>%
  mutate(count = ifelse(is.na(count), 0, count))

## This code will uncount my data, to make it appropriate for the following models... 
fly_fitness_tidy_2  <- uncount(fly_fitness_tidy, count)




#### Testing Models ####



# Model 1 
#### Poisson GLM ####
glm.p.UMFE.fly <- glm(time_hours ~
                        treatment * sex  , 
                      
                      family = poisson, data = fly_fitness_tidy_2)


#### ASSUMPTION CHECKS: 

# easystats / performance 
performance::check_model(glm.p.UMFE.fly, check = c("outliers")) # weird? 
performance::check_model(glm.p.UMFE.fly, check = c("homogeneity")) # think this looks alright 


## Checking for overdispersion 
check_overdispersion(glm.p.UMFE.fly) 
## Overdispersion detected

# DHARMa checks 
plot(simulateResiduals(glm.p.UMFE.fly)) 
 # Seems to be significant tests for everything 








# Model 2 
 # As there is overdispersion, trying a Negative Binomial GLM
#### Negative Binomial GLM ####
glm.nb.UMFE.fly <- glm.nb( time_hours ~
                            
                            treatment * sex ,
                          
                          data = fly_fitness_tidy_2)


#### Assumption Checks: 

# performance - easystats 
performance::check_model(glm.nb.UMFE.fly, check = c("outliers")) 
performance::check_model(glm.nb.UMFE.fly, check = c("homogeneity")) # looks a bit worse than glm poisson 


## Checking for overdispersion 
check_overdispersion(glm.nb.UMFE.fly)
 ## No overdispersion detected 


# DHARMa checks 
plot(simulateResiduals(glm.nb.UMFE.fly)) 
 ## qq seems tp line up well, but there are still significant tests 










# Model 3
# As assumptions are not great, trying a mixed model 
#### Poisson GLMM ####
glmm.p.UMFE.fly <- glmmTMB( time_hours ~ 
                             
                             treatment * sex 
                           
                           + (1|vial),
                           
                           family = poisson, data = fly_fitness_tidy_2)



# DHARMa checks 
plot(simulateResiduals(glmm.p.UMFE.fly)) 
 ## Assumptions now look even worse 

check_overdispersion(glmm.p.UMFE.fly)









# Comparing models
AIC(glm.p.UMFE.fly,glm.nb.UMFE.fly, glmm.p.UMFE.fly)
 # Models are not great - but best is Negative Binomial GLM
 

# chosen model is Poisson GLMM? 
glmm.p.UMFE.fly <- glmmTMB( time_hours ~ 
                              
                              treatment * sex 
                            
                            + (1|vial) ,
                            
                            family = poisson, data = fly_fitness_tidy_2)




## Looking for significance of two-way interaction
drop1(glmm.p.UMFE.fly, test = "Chisq")
  # The two-way interaction is significant
  

#### DATA ANALYSIS ####

# Basic analysis 
summary(glmm.p.UMFE.fly) 

# Values for write-up 
emmeans::emmeans(glmm.p.UMFE.fly, specs = ~ sex + treatment, type = "response")

# Confidence intervals 
exp(confint(glmm.p.UMFE.fly))

## Table to display data
tab_model(glmm.p.UMFE.fly, CSS = list(css.table = '+font-family: Arial;'))



