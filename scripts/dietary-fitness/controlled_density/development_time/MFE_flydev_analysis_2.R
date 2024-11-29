
# Chapter 3 - Development 


# Overall Fly Emergence 

#### Packages 📦📦📦📦 ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


#### Reading data in, sorting data ####

# Using readexcel to get the dataframe 
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Separating the data into female and male columns 
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 


# As DF
#fly_fitness_tidy_MFE <- as.data.frame(fly_fitness_tidy_MFE)


## Cleaning the data into a tidier format, for correct data analysis

# Making NAs 0, so this code works 
fly_fitness_tidy_MFE_filled <- fly_fitness_tidy_MFE %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Changing count to uncount, so the format is better 
fly_fitness_tidy_MFE_2 <- uncount(fly_fitness_tidy_MFE_filled, count)




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


#### Chosen model: NegBin ####
glm.nb.MFE.fly.2 <- glm.nb(time_hours ~
                           
                           treatment + sex ,
                         
                         data = fly_fitness_tidy_MFE_2)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glm.nb.MFE.fly.2)

# Real values for write-up
emmeans::emmeans(glm.nb.MFE.fly.2, specs = ~ sex + treatment, type = "response")

# Confidence intervals
exp(confint(glm.nb.MFE.fly.2))

# Table for write-up 
tab_model(glm.nb.MFE.fly.2, CSS = list(css.table = '+font-family: Arial;'))
