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






fly_fitness_tidy_MFE_filled <- fly_fitness_tidy_MFE %>%
  mutate(count = ifelse(is.na(count), 0, count))


fly_fitness_tidy_MFE_2 <- uncount(fly_fitness_tidy_MFE_filled, count)






#### Testing Models ####

# Model 1 
#### Poisson GLM ####
glm.p.MFE.fly <- glm(time_hours ~
                       treatment * sex  , 
                     
                     family = poisson, data = fly_fitness_tidy_MFE_2)


#### ASSUMPTION CHECKS 
performance::check_model(glm.p.MFE.fly, check = c("qq")) 
# doesn't show 
performance::check_model(glm.p.MFE.fly, check = c("outliers")) 
# weird? 
performance::check_model(glm.p.MFE.fly, check = c("homogeneity")) 
# really quite bad 

## Checking for overdispersion 
check_overdispersion(glm.p.MFE.fly) 
## overdispersion detected

## Checking for zeroinflation
check_zeroinflation(glm.p.MFE.fly) 
## there is no zero inflation 









# Model 2 
#### Negative Binomial GLM ####
glm.nb.MFE.fly <- glm.nb(time_hours ~
                           
                           treatment * sex ,
                         
                         data = fly_fitness_tidy_MFE_2)


#### ASSUMPTION CHECKS 
performance::check_model(glm.nb.MFE.fly, check = c("qq")) 
# does not show. 
performance::check_model(glm.nb.MFE.fly, check = c("outliers")) 

performance::check_model(glm.nb.MFE.fly, check = c("homogeneity"))
# looks a bit weird


## Checking for overdispersion 
check_overdispersion(glm.nb.MFE.fly)
## No overdispersion detected 

## qq plot from the model
residuals <- residuals(glm.nb.MFE.fly)
qqnorm(residuals)
qqline(residuals, col = 2)
## qq residuals look pretty good







# Model 3
#### Poisson GLMM ####
glmm.p.MFE.fly <- glmmTMB(time_hours ~ 
                            
                            treatment * sex
                          
                          + (1|sex/vial) ,
                          
                          family = poisson, data = fly_fitness_tidy_MFE_2)


## qq plot from the model
residuals <- residuals(glmm.p.MFE.fly)
qqnorm(residuals)
qqline(residuals, col = 2)
## qq residuals look pretty good. 

## Performance
check_zeroinflation(glmm.p.MFE.fly)
# there is no zero inflation
check_overdispersion(glmm.p.MFE.fly)
# overdispersion detected











# Comparing models
AIC(glm.p.MFE.fly,glm.nb.MFE.fly, glmm.p.MFE.fly)






#### Chosen model: Poisson GLMM ####
# Model 2 
#### Negative Binomial GLM ####
glm.nb.MFE.fly <- glm.nb(time_hours ~
                           
                           treatment * sex ,
                         
                         data = fly_fitness_tidy_MFE_2)





# Using drop1 to look for significance in a 3-way interaction
drop1(glm.nb.MFE.fly, test = "Chisq")
# No 2-way interaction effect


#### Chosen model: Poisson GLMM ####
glm.nb.MFE.fly.2 <- glm.nb(time_hours ~
                           
                           treatment + sex ,
                         
                         data = fly_fitness_tidy_MFE_2)




## Data analysis of the chosen model
summary(glm.nb.MFE.fly.2)

exp(confint(glm.nb.MFE.fly.2))

tab_model(glm.nb.MFE.fly.2)
