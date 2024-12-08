#### Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


#### Reading data in: ####
pupae_fitness_UMFE <- read_excel("data/fitness_development/pupae_data.xlsx")



#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallpupae_UMFE <- pupae_fitness_UMFE %>%
  group_by(vial, treatment) %>%
  summarise(total_count = sum(pupae, na.rm = FALSE)) %>%
  ungroup() 




#### TESTING MODELS ####


# Model 1
#### Poisson GLMM ####
glmm.p.totalpupae.UMFE <- glmmTMB(total_count ~ 
                               
                                treatment + (1|vial), 
                             
                             
                             
                             family = poisson, data = overallpupae_UMFE)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.totalpupae.UMFE, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glmm.p.totalpupae.UMFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.totalpupae.UMFE)
## NO zero inflation 


# GLM NB 
glm.nb.UMFE.totalpupae <- glm.nb(total_count ~
                              
                              treatment,
                            
                            data = overallpupae_UMFE)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.UMFE.totalpupae, plot = T)
## Assumptions are a lot better


check_overdispersion(glm.nb.UMFE.totalpupae)
# No overdispersion detected 






# Comparing the two models: 
AIC(glmm.p.totalpupae.UMFE, glm.nb.UMFE.totalpupae)
  # Negative Binomial GLM is better






#### DATA ANALYSIS ####

# Basic analysis
summary(glm.nb.UMFE.totalpupae)


# confidence intervals
exp(confint(glm.nb.UMFE.totalpupae))
    

## Getting numbers for the write-up
emmeans::emmeans(glm.nb.UMFE.totalpupae, specs =   ~
                   
                   treatment , type = "response")


# Table for the write-up
tab_model(glm.nb.UMFE.totalpupae, CSS = list(css.table = '+font-family: Arial;'))










# #### other versions 
# 
# ## just incase the data wanted to be looked at in other ways. 
# 
# 
# #### fly emergence overall 
# #### EMERGENCE NOT BY SEX
# fly_emergence_overall <- fly_fitness_tidy_UMFE %>%
#   filter(sex %in% c("females", "males")) %>%
#   group_by(vial, treatment) %>%
#   summarise(overall_emergence = sum(count, na.rm = FALSE)) %>%
#   ungroup() %>%
#   mutate(sex_treatment = paste(treatment, "overall", sep = " ")) %>%
#   mutate(sex_treatment = factor(sex_treatment,
#                                 levels = c("conditioned overall", "unconditioned overall")))
# 
# 
# 
# 
# ## An overall code of emergence per time
# ## THIS COMBINES THE VIALS
# emergence_per_time <- fly_fitness %>%
#   group_by(treatment, time_hours) %>%
#   summarize(total_females = sum(females, na.rm = TRUE),
#             total_males = sum(males, na.rm = TRUE)) %>%
#   ungroup()







