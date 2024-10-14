#### MFE Pupae Analysis ####


#### Packages 📦📦📦📦 ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


#### Reading, cleaning and organising the data ####

## Reading pupae data in with read excel
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")

## This code does something with 0s - not sure if it needed... 
pupae_fitness_MFE <- pupae_fitness_MFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae))

## The uncount code will simply clean the data
pupae_fitness_MFE_2 <- uncount(pupae_fitness_MFE, pupae)


## I now use this uncounted dataset, which works best for time. 

#### Preliminary Data Analysis #### 


#### 1. Testing Models ####


# Model 1 
#### Poisson GLMM #### 
glmm.p.pupae <- glmmTMB(time_hours ~ treatment  + (1| vial), family = poisson, 
                        data = pupae_fitness_MFE_2)



#### ASSUMPTION CHECKS:  
# DHARMa checks 
plot(simulateResiduals(glmm.p.pupae)) 
  ## Does not look great
  # DHARMa assumptions shows this model is poor for both qq and homogeneity 
  # tests are significant for both 


## Performance checks - easystats 

# Looking for zeroinflation
check_zeroinflation(glmm.p.pupae) 
## There NO is zero inflation, indicating issues may be coming from overdispersion 

# Looking for overdispersion 
check_overdispersion(glmm.p.pupae) 
 ## There is overdispersion, indicating a negative binomial model might be good to test 






# Model 2
#### Negative Binomial GLM ####
glm.nb_pupae <- glm.nb(time_hours ~ 
                         treatment   
                       
                       , data = pupae_fitness_MFE_2)



#### ASSUMPTION CHECKS 
# DHARMa checks 
plot(simulateResiduals(glm.nb_pupae)) 
## qq looks a lot better, BUT there are still significant P values

## Performance checks
check_zeroinflation(glm.nb_pupae)
## No zeroinflation 
check_overdispersion(glm.nb_pupae) 
## No overdispersion 




## In this situation, I do not know what to do? 




## Comparing models:
AIC(glmm.p.pupae,glm.nb_pupae)
 # Negative Binomial GLM is lower, but only slightly
 # It is still not a great model... need to research into what models could be better






## Using Negative Binomial GLM (FOR NOW)... 
glm.nb_pupae <- glm.nb(time_hours  ~ 
                         treatment  
                       
                       , data = pupae_fitness_MFE_2)



#### 2. Data analysis with chosen model ####

# Basic analysis 
summary(glm.nb_pupae)

# Confidence intervals
exp(confint(glm.nb_pupae))

# Getting real values for write-up 
emmeans::emmeans(glm.nb_pupae, specs =  ~ treatment, type = "response")


# Generating a table 
tab_model(glm.nb_pupae, CSS = list(css.table = '+font-family: Arial;'))








