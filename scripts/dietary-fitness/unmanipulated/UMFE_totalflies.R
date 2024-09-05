#### Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)


#### Reading, cleaning and organising data ####
fly_fitness_UMFE <- read_excel("data/fitness_development/fly_data.xlsx")


## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_UMFE <- tidyr::pivot_longer(data = fly_fitness_UMFE ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 



#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallflies_UMFE <- fly_fitness_tidy_UMFE %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment) %>%
  summarise(total_count = sum(count, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, sex, sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                levels = c("conditioned females", "unconditioned females",
                                           "conditioned males", "unconditioned males")))






#### Testing models ####


# Model 1
#### Poisson GLMM ####
glmm.p.total.UMFE <- glmmTMB(total_count ~ 
                               
                               sex * treatment
                             
                             + (1|vial ),
                             
                             
                             family = poisson, data = overallflies_UMFE)


#### Assumption checking: ####

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glmm.p.total.UMFE, plot = T)
 ## Assumptions aren't great, new model maybe? 
 ## qq doesn't line up too well

# easystats checks
check_overdispersion(glmm.p.total.UMFE)
 # Overdispersion detected 

check_zeroinflation(glmm.p.total.UMFE)
 ## No zero inflation 




# Model 1
#### Negative Binomial GLM ####
glm.nb.UMFE.flies <- glm.nb(total_count ~
                            
                            sex * treatment,
                          
                          data = overallflies_UMFE)



## Assumption checking:

# DHARMa:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.UMFE.flies, plot = T)
## Assumptions seem to be a lot better

# easystats
check_overdispersion(glm.nb.UMFE.flies)
# No overdispersion detected



# Comparing the two models with AIC
AIC(glmm.p.total.UMFE, glm.nb.UMFE.flies)
  # Negative Binomial GLM has lower AIC


#### Chosen model; Negative Binomial GLM ####

# Testing two-way interaction effect
glm.nb.UMFE.flies <- glm.nb(total_count ~
                              
                              sex * treatment,
                            
                            data = overallflies_UMFE)




## Looking for interaction effect 
drop1(glm.nb.UMFE.flies, test = "Chisq")
  ## No 2-way interaction effect found




# Removing the two-way interaction effect 
glm.nb.UMFE.flies.2 <- glm.nb(total_count ~
                              
                              sex + treatment,
                            
                            data = overallflies_UMFE)






#### DATA ANALYSIS ####

# For basic analysis
summary(glm.nb.UMFE.flies.2)

# condidence intervals 
exp(confint(glm.nb.UMFE.flies.2))

# getting values for the write-up
emmeans::emmeans(glm.nb.UMFE.flies.2, specs = ~ sex + treatment, type = "response")

# getting a table for the write up
tab_model(glm.nb.UMFE.flies.2, CSS = list(css.table = '+font-family: Arial;'))














# 
# ## trying a zero inflation model 
# 
# 
# # Fit a zero-inflated Poisson GLMM
# model <- glmmTMB(
#   total_count ~ sex * treatment + (1 | vial), 
#   ziformula = ~ sex * treatment,               
#   family = poisson(),                         
#   data = overallflies_UMFE  )                  
# 
# 
# simulationOutput <- simulateResiduals(fittedModel = glm.zi.nb.UMFE.flies, plot = T)
# 
# 
# # Fit a zero-inflated negative binomial GLMM
# glm.zi.nb.UMFE.flies <- glmmTMB(
#   total_count ~ sex * treatment + (1 | vial),  
#   ziformula = ~ sex * treatment,               
#   family = nbinom2(),                          
#   data = overallflies_UMFE_1
# )
# 
# 
# simulationOutput <- simulateResiduals(fittedModel = glm.zi.nb.UMFE.flies, plot = T)
# 
# 
# AIC(glmm.p.total.UMFE,glm.nb.UMFE.flies, glm.zi.nb.UMFE.flies )
# 
# drop1(glm.zi.nb.UMFE.flies, test = "Chisq")
# ## no sig 
# 
# 
# glm.zi.nb.UMFE.flies.2 <- glmmTMB(
#   total_count ~ sex + treatment + (1 | vial),  
#   ziformula = ~ sex + treatment,               
#   family = nbinom2(),                          
#   data = overallflies_UMFE
# )
# 
# 
# summary(glm.zi.nb.UMFE.flies.2)
# 
# exp(confint(glm.zi.nb.UMFE.flies.2))
# 
# emmeans::emmeans(glm.zi.nb.UMFE.flies.2, specs = ~ sex + treatment, type = "response")
# 
# 
# 
# 
# 
# tab_model(glm.zi.nb.UMFE.flies.2, CSS = list(css.table = '+font-family: Arial;'))
# 
# 
# 
# simulationOutput <- simulateResiduals(fittedModel = model2, plot = T)
# 
# 
# 
# AIC(glmm.p.total.UMFE, glm.nb.UMFE.flies, model, model2)
# 
# 
# check_zeroinflation(model2)
# 
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
#   summarise(overall_emergence = sum(count, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(sex_treatment = paste(treatment, "overall", sep = " ")) %>%
#   mutate(sex_treatment = factor(sex_treatment,
#                                 levels = c("conditioned ove