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
library(sjPlot)


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



## Code which will remove count
fly_fitness_tidy_filled <- fly_fitness_tidy %>%
  mutate(count = ifelse(is.na(count), 0, count))

   fly_fitness_tidy_2 <- uncount(fly_fitness_tidy_filled, count)




#### Testing Models ####

# Model 1 
#### Poisson GLM ####
glm.p.UMFE.fly <- glm(time_hours ~
                        treatment * sex  , 
                      
                      family = poisson, data = fly_fitness_tidy_2)


#### ASSUMPTION CHECKS 
performance::check_model(glm.p.UMFE.fly, check = c("qq")) # doesn't show 
performance::check_model(glm.p.UMFE.fly, check = c("outliers")) # weird? 
performance::check_model(glm.p.UMFE.fly, check = c("homogeneity")) # think this looks alright 

## Checking for overdispersion 
check_overdispersion(glm.p.UMFE.fly) 
## overdispersion detected

## Checking for zeroinflation
check_zeroinflation(glm.p.UMFE.fly) 
## NO zero inflation 









# Model 2 
#### Negative Binomial GLM ####
glm.nb.UMFE.fly <- glm.nb( time_hours ~
                            
                            treatment * sex ,
                          
                          data = fly_fitness_tidy_2)


#### ASSUMPTION CHECKS 
performance::check_model(glm.nb.UMFE.fly, check = c("qq")) # does not show. 
performance::check_model(glm.nb.UMFE.fly, check = c("outliers")) 
performance::check_model(glm.nb.UMFE.fly, check = c("homogeneity")) # looks a bit worse than glm poisson 


## Checking for overdispersion 
check_overdispersion(glm.nb.UMFE.fly)
## no OVERDISPERSION detected









# Model 3
#### Poisson GLMM ####
glmm.p.UMFE.fly <- glmmTMB( time_hours ~ 
                             
                             treatment * sex 
                           
                           + (1|sex/vial) + (1|time_hours),
                           
                           family = poisson, data = fly_fitness_tidy_2)


## qq plot from the model
residuals <- residuals(glmm.p.UMFE.fly)
qqnorm(residuals)
qqline(residuals, col = 2)
  ##  looks in line?

## Performance
check_zeroinflation(glmm.p.UMFE.fly)
# No zero inflation
check_overdispersion(glmm.p.UMFE.fly)
# No overdisperion




# Model 4 
#### Zero-Inflated Poisson ####
zi.p.UMFE.fly <- zeroinfl(  time_hours ~ 
                            
                            treatment * sex  
                          
                          , data = fly_fitness_tidy_2)







# Comparing models
AIC(glm.p.UMFE.fly,glm.nb.UMFE.fly, glmm.p.UMFE.fly)



# chosen model
glmm.p.UMFE.fly <- glmmTMB( time_hours ~ 
                              
                              treatment * sex 
                            
                            + (1|sex/vial) ,
                            
                            family = poisson, data = fly_fitness_tidy_2)



drop1(glmm.p.UMFE.fly, test = "Chisq")

#### DATA ANALYSIS ####
summary(glmm.p.UMFE.fly) 


emmeans::emmeans(glmm.p.UMFE.fly, specs = ~ sex + treatment, type = "response")

exp(confint(glmm.p.UMFE.fly))

## Table to display data
tab_model(glmm.p.UMFE.fly, CSS = list(css.table = '+font-family: Arial;'))


exp(confint(glmm.p.UMFE.fly))
