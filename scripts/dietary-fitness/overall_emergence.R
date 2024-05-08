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

## Reading data in 
fly_fitness <- read_excel("data//fitness_development/fly_data.xlsx")

############

## Separating the data into female and male columns 
fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 


fly_emergence_sex <- fly_fitness_tidy %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, sex, sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                levels = c("conditioned females", "unconditioned females",
                                           "conditioned males", "unconditioned males")))



## Starting with a basic model 

## Basic glm poisson 
glm_poisson_fly <- glm(total_count ~ treatment * sex, family = poisson, data = fly_emergence_sex)


#### ASSUMPTION CHECKS 
performance::check_model(glm_poisson_fly, check = c("qq")) # doesn't show 
performance::check_model(glm_poisson_fly, check = c("outliers")) # weird? 
performance::check_model(glm_poisson_fly, check = c("homogeneity")) # a bit bumpy

## Checking for overdispersion 
check_overdispersion(glm_poisson_fly) ## overdispersion detected

## Checking for zeroinflation
check_zeroinflation(glm_poisson_fly) ## there is zero inflation 


## Trying a negative binomial model
glm.nb_fly <- glm.nb(total_count ~ treatment * sex, fly_emergence_sex)


#### ASSUMPTION CHECKS 
performance::check_model(glm.nb_fly, check = c("qq")) # does not show. 
performance::check_model(glm.nb_fly, check = c("outliers")) 
performance::check_model(glm.nb_fly, check = c("homogeneity")) # still slopey

## Checking for overdispersion 
check_overdispersion(glm.nb_fly) ## overdispersion still detected




# model without interaction
glm_mm_fly <- glmmTMB(total_count ~ treatment * sex + (1|sex/vial), family = poisson, data = fly_emergence_sex)

## qq plot from the model
residuals <- residuals(glm_mm_fly)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos


## performance easystats
performance::check_model(glm_mm_fly, check = c("homogeneity")) # bit slopish

## Checking AIC of different models so far 
AIC(glm_poisson_fly, glm.nb_fly, glm_mm_fly)
 # glm nb - lowest AIC 


## using glm.nb_fly so far
glm.nb_fly <- glm.nb(total_count ~ treatment * sex, fly_emergence_sex)

## checking for interaction effect 
drop1(glm.nb_fly, test = "F") ## no interaction effect

## model without sex being an interaction effect
glm.nb_fly_2 <- glm.nb(total_count ~ treatment + sex, fly_emergence_sex)

## looking at data analysis 
## no diff between males and females
## significant difference between conditioned and unconditioned
summary(glm.nb_fly_2)
