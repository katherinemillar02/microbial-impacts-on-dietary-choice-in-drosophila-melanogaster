#### Packages ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
####################



#### Reading data in ####
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

#### Making a dataframe of total pupae, not over time 
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = FALSE))




#### Data Analysis ####


# Model 1 - Poisson GLMM
glmm.p.MFE.totalpupae <- glmmTMB(total_pupae ~ 
                              
                              treatment 
                              
                              
                              + (1|vial) + (1|id),
                            
                            family = poisson, data = total_pupae)


# Assumption checks 

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.p.MFE.totalpupae, plot = T)
 # Model looks sort of okay



# easyststas checks
check_overdispersion(glmm.p.MFE.totalpupae)
 # There is overdispersion 

check_zeroinflation(glmm.p.MFE.totalpupae)
 # No zeroinflation 



# Model 2 
# Negative Binomial GLM #
glm.nb.MFE.totalpupae <- glm.nb(total_pupae ~ 
                                  
                                  treatment,
                          
                          data = total_pupae)


## Assumption checks 
# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.totalpupae, plot = T)


# easyststas checks
check_overdispersion(glm.nb.MFE.totalpupae)
# No overdispersion 


# Comparing models 
AIC(glmm.p.MFE.totalpupae, glm.nb.MFE.totalpupae)
 # Negative Binomial GLM











#### Final Data analysis ####

# Simple analysus
summary(glmm.p.MFE.totalpupae)

# Confidence intervals 
exp(confint(glmm.p.MFE.totalpupae))


# getting values for the write-up
emmeans::emmeans(glmm.p.MFE.totalpupae, specs = ~ treatment, type = "response")


# Using a table to view the data
tab_model(glmm.p.MFE.totalpupae, CSS = list(css.table = '+font-family: Arial;'))
# looking okay

