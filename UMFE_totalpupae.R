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
  summarise(total_count = sum(pupae, na.rm = TRUE)) %>%
  ungroup() 




#### TESTING MODELS ####


# Model 1
#### Poisson GLMM ####
glmm.p.total.UMFE <- glmmTMB(total_count ~ 
                               
                               sex * treatment, 
                             
                             
                             
                             family = poisson, data = overallflies_UMFE)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.total.UMFE, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glmm.p.total.UMFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.total.UMFE)
## zero inflation 


# GLM NB 
glm.nb.UMFE.flies <- glm.nb(total_count ~
                              
                              sex * treatment,
                            
                            data = overallflies_UMFE)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.UMFE.flies, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glm.nb.UMFE.flies)
# Underdispersion detected 

check_zeroinflation(glm.nb.UMFE.flies)
## zero inflation 


AIC(glmm.p.total.UMFE, glm.nb.UMFE.flies)

#### Chosen model; nb GLM ####

glm.nb.UMFE.flies <- glm.nb(total_count ~
                              
                              sex * treatment,
                            
                            data = overallflies_UMFE)


## Looking for interaction effect 
drop1(glm.nb.UMFE.flies, test = "Chisq")
## No 2-way interaction effect found.




## Model without interaction effect: 
glm.nb.UMFE.flies.2 <- glm.nb(total_count ~
                                
                                sex + treatment,
                              
                              data = overallflies_UMFE)





#### DATA ANALYSIS ####
summary(glm.nb.UMFE.flies.2)

# Table
tab_model(glmm.p.total.UMFE.2)










#### other versions 

## just incase the data wanted to be looked at in other ways. 


#### fly emergence overall 
#### EMERGENCE NOT BY SEX
fly_emergence_overall <- fly_fitness_tidy_UMFE %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, treatment) %>%
  summarise(overall_emergence = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, "overall", sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                levels = c("conditioned overall", "unconditioned overall")))




## An overall code of emergence per time
## THIS COMBINES THE VIALS
emergence_per_time <- fly_fitness %>%
  group_by(treatment, time_hours) %>%
  summarize(total_females = sum(females, na.rm = TRUE),
            total_males = sum(males, na.rm = TRUE)) %>%
  ungroup()







