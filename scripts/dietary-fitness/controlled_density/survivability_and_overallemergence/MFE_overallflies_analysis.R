
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
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                             cols = c( females, males),
                                             names_to = "sex",
                                             values_to = "count") 



#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment, id) %>%
  summarise(total_count = sum(count, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, sex, sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                levels = c("conditioned females", "unconditioned females",
                                           "conditioned males", "unconditioned males")))



#### TESTING MODELS ####


# Model 1
#### Poisson GLMM ####
glmm.p.total.MFE <- glmmTMB(total_count ~ 
                               
                               sex * treatment +
                             
                              (1|id) + (1|vial) ,
                             
                             family = poisson, data = overallflies_MFE)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.total.MFE , plot = T)
 ## Assumptions aren't great, new model maybe? 



check_overdispersion(glmm.p.total.MFE)
# overdispersion detected 

check_zeroinflation(glmm.p.total.MFE)
## zero inflation 




# Model 2 
# Negative Binomial GLM
glm.nb.MFE.flies <- glm.nb(total_count ~
                              
                              sex * treatment,
                            
                            data = overallflies_MFE)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.flies, plot = T)
## Assumptions aren't great, new model maybe? 


check_overdispersion(glm.nb.MFE.flies)
# Overderdispersion detected 

check_zeroinflation(glm.nb.MFE.flies)
## zero inflation 



## The issue seems to be zeroinflation 



# Trying zeroinflation models 



# Model 3
 # Negative Binomial GLM # 
glm.zi.nb.MFE.flies <- glmmTMB(
  total_count ~ sex * treatment + (1 | vial) + (1 | id),  
  ziformula = ~ sex * treatment,               
  family = nbinom2(),                          
  data = overallflies_MFE
)


## Assumption checking 
# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glm.zi.nb.MFE.flies, plot = T)
 # Model seems ok


check_overdispersion(glm.zi.nb.MFE.flies)
 # No overdispersion 




# Model 4 
 # Poisson Zero Inflated
glm.zi.p.MFE.flies <- glmmTMB(
  total_count ~ sex * treatment + (1 | vial) + (1 | id),  
  ziformula = ~ sex * treatment,               
  family = poisson(),                          
  data = overallflies_MFE
)


## Assumption checks

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.zi.p.MFE.flies, plot = T)
 
# easystats 
check_overdispersion(glm.zi.p.MFE.flies)
 # No overdispersion even with poisson models


# Comparing models
AIC(glmm.p.total.MFE, glm.zi.nb.MFE.flies, glm.zi.p.MFE.flies)
 # Poisson slightly better 




#### Final model: Zero Inflated Poisson ####
glm.zi.p.MFE.flies <- glmmTMB(
  total_count ~ sex * treatment + (1 | vial) + (1 | id),  
  ziformula = ~ sex * treatment,               
  family = poisson(),                          
  data = overallflies_MFE
)

# Testing interaction effect
drop1(glm.zi.p.MFE.flies, test = "Chisq")
 ## interaction effect not needed


# Model without interaction effect
glm.zi.p.MFE.flies.2 <- glmmTMB(
  total_count ~ sex + treatment + (1 | vial) + (1 | id),  
  ziformula = ~ sex + treatment,               
  family = poisson(),                          
  data = overallflies_MFE
)



#### DATA ANALYSIS ####

# Basic analysis
summary(glm.zi.p.MFE.flies.2)

# Confidence intervals
exp(confint(glmm.p.total.MFE.2))

# Values for analysis write-up
emmeans::emmeans(glm.zi.p.MFE.flies.2, specs = ~ sex + treatment, type = "response")

# Table for write-up
tab_model(glm.zi.p.MFE.flies.2, CSS = list(css.table = '+font-family: Arial;'))



