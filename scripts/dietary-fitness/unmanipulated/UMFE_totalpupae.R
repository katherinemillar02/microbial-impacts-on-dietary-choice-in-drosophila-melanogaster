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
glmm.p.totalpupae.UMFE <- glmmTMB(total_count ~ 
                               
                                treatment + (1|vial), 
                             
                             
                             
                             family = poisson, data = overallpupae_UMFE)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.totalpupae.UMFE, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glmm.p.totalpupae.UMFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.totalpupae.UMFE)
## zero inflation 


# GLM NB 
glm.nb.UMFE.totalpupae <- glm.nb(total_count ~
                              
                              treatment,
                            
                            data = overallpupae_UMFE)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.UMFE.totalpupae, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glm.nb.UMFE.totalpupae)
# overdispersion detected 

check_zeroinflation(glm.nb.UMFE.totalpupae)
## zero inflation 



 # none of these models are great
zi.p.UMFE.totalpupae <- zeroinfl(total_count ~
                                   
                                   treatment, dist = "poisson",
                                 
                                 data = overallpupae_UMFE)


residuals <- residuals(zi.p.UMFE.totalpupae)
qqnorm(residuals)
qqline(residuals, col = 2) # qq looks pretty goos


AIC(glmm.p.totalpupae.UMFE, glm.nb.UMFE.totalpupae, zi.p.UMFE.totalpupae)
# zeroinflation model way better


#### Chosen model; nb GLM ####

zi.p.UMFE.totalpupae <- zeroinfl(total_count ~
                                   
                                   treatment, dist = "poisson",
                                 
                                 data = overallpupae_UMFE)





#### DATA ANALYSIS ####
summary(zi.p.UMFE.totalpupae)

exp(confint(zi.p.UMFE.totalpupae))

# Table
tab_model(zi.p.UMFE.totalpupae, CSS = list(css.table = '+font-family: Arial;'))










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







