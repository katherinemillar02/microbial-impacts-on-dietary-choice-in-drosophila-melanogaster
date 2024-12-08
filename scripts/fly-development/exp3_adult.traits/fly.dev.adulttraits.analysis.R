
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
fly_fitness_adulttraits <- read_excel("data/fitness_development/adulttraits_flydev.xlsx")

## Separating the data into female and male columns 
fly_fitness_tidy_adulttraits <- tidyr::pivot_longer(data = fly_fitness_adulttraits ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 


# As DF
fly_fitness_tidy_adulttraits <- as.data.frame(fly_fitness_tidy_adulttraits)


## Cleaning the data into a tidier format, for correct data analysis

# Making NAs 0, so this code works 
fly_fitness_tidy_adulttraits_filled <- fly_fitness_tidy_adulttraits %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Changing count to uncount, so the format is better 
fly_fitness_tidy_adulttraits_2 <- uncount(fly_fitness_tidy_adulttraits_filled, count)




#### 1. Preliminary Data Analysis ####
## Testing Models

# Model 1 
#### Poisson GLM ####
glm.p.adulttraits.fly <- glm(time_hours ~
                       treatment * sex  , 
                     
                     family = poisson, data = fly_fitness_tidy_adulttraits_2)


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



glmm.p.adulttraits.fly <- glmmTMB(time_hours ~ 
                                    
                                    treatment * sex
                                  
                                  + (1|sex/vial) ,
                                  
                                  family = poisson, data = fly_fitness_tidy_adulttraits_2)




# Using drop1 to look for significance in a 3-way interaction
drop1(glmm.p.adulttraits.fly, test = "Chisq")
# No 2-way interaction effect


#### Chosen model: NegBin ####
glm.nb.adulttraits.fly.2 <- glm.nb(time_hours ~
                             
                             treatment + sex ,
                           
                           data = fly_fitness_tidy_adulttraits_2)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glm.nb.adulttraits.fly.2)

# Real values for write-up
emmeans::emmeans(glm.nb.MFE.fly.2, specs = ~ sex + treatment, type = "response")

# Confidence intervals
exp(confint(glm.nb.adulttraits.fly.2))

# Table for write-up 
tab_model(glm.nb.adulttraits.fly.2, CSS = list(css.table = '+font-family: Arial;'))




## 
summary_data <- fly_fitness_adulttraits %>%
  group_by(treatment, time_hours, vial) %>%
  summarise(total_females = sum(females),
            total_males = sum(males))
