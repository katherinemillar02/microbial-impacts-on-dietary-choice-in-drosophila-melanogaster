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


#### Reading the data in: ####

## Read data file in using excel: 
pupae_fitness_UMFE <- read_excel("data/fitness_development/pupae_data.xlsx")

## Making NAs 0
pupae_fitness_tidy_filled <- pupae_fitness_UMFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae)) 
  ## If I have NAs in my data, it makes the count 0 - this is to make the next code work

## Code which will remove count, making the data frame needed for the following models 
pupae_fitness_UMFE_2 <- uncount(pupae_fitness_tidy_filled, pupae)

 
#### Preliminary Data Analysis

# Model 1 
#### Poisson GLMM ####
glmm.p.UMFE.pupae <- glmmTMB( time_hours
                             ~ treatment 
                             
                             + (1| vial) + (1| time_hours), family = poisson, data = pupae_fitness_UMFE_2)

## Assumption Checking
# DHARMa checks 
plot(simulateResiduals(glmm.p.UMFE.pupae)) 
## This looks quite bad, qq doesn't line up, residuals are everywhere.
## Also lots of significant tests. 


# Performance checks - easystats
check_zeroinflation(glmm.p.UMFE.pupae)
## No zero inflation
check_overdispersion(glmm.p.UMFE.pupae) 
## There no overdispersion 



 
# Model might be ok, but assumptions do not look great... 


# Model 2 
#### Negative Binomial GLM ####
glm.nb.UMFE.pupae <- glm.nb(time_hours ~ 
                              
                              treatment  
                            
                            , data = pupae_fitness_UMFE_2)



# DHARMa checks 
plot(simulateResiduals(glm.nb.UMFE.pupae)) 
## qq plot is better, but residuals are still dodgy.
## There is still the same significant tests


AIC(glmm.p.UMFE.pupae, glm.nb.UMFE.pupae)
 # For now... 




# Chosen model: 
glmm.p.UMFE.pupae <- glmmTMB( time_hours
                              ~ treatment 
                              
                              + (1| vial) , family = poisson, data = pupae_fitness_UMFE_2)


#### DATA ANALYSIS ####

# Basic analysis
summary(glmm.p.UMFE.pupae)

# Table for write-up
tab_model(glmm.p.UMFE.pupae, CSS = list(css.table = '+font-family: Arial;'))

# Confidence intervals for write-up
exp(confint(glmm.p.UMFE.pupae))

# Real values for write-up
emmeans::emmeans(glmm.p.UMFE.pupae, specs = ~ treatment, type = "response")



