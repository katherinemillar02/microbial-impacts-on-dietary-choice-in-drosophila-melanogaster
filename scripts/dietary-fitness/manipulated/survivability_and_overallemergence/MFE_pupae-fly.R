#### Packages #### 
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)



#### Reading data in: ####

## Fly data
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")

## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 
## Summing up total flies
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  group_by(id, vial, treatment) %>%
  summarise(total_count = sum(count, na.rm = FALSE)) 

## making it a dataframe 
overallflies_MFE <- as.data.frame(overallflies_MFE)





## Pupae data
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

#### Summing up total pupae
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = FALSE))

## Changing it to a dataframe
total_pupae <- as.data.frame(total_pupae)



#### Combining data frames ####


## Calculating the data frames together, making a variable on the left of total pupae
flies_and_pupae <- overallflies_MFE %>%
  left_join(total_pupae, by = c("id", "vial", "treatment"))



## Calculating the survivability percentages of flies to pupae 
survivability_between <- flies_and_pupae %>%
  mutate(survivability = (total_count / total_pupae) * 100)



#### Model testing ####

# Model 1
# Poisson GLMM #
glmm.p.bothsurvive.MFE <- glmmTMB(survivability ~ 
                                    
                                    treatment +
                                    
                                    + (1|vial) + (1|id),
                                  
                                  family = poisson, data = survivability_between)


## Assumption checking:

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.p.bothsurvive.MFE , plot = T)
## Assumptions aren't great, new model maybe? ## red values

# easystats
check_overdispersion(glmm.p.bothsurvive.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.bothsurvive.MFE)
##  zero inflation 





# Model 2 
 # Negative Binomial GLM #
glm.nb.MFE.both <- glm.nb(survivability ~
                            
                            treatment,
                          
                          data = survivability_between)



## Assumption checking:

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.nb.MFE.both, plot = T)
## Assumptions aren't  eeven worse 



# easystats 
check_overdispersion(glm.nb.MFE.both)
# No overderdispersion detected 

check_zeroinflation(glm.nb.MFE.both)
## Zero inflation 



## There no overdispersion but there is still zeroinflation, trying zeroinflated models 



# Model 3
 # Poisson Zero Inflated #
glm.zi.p.MFE.surviveboth <- glmmTMB(
  survivability ~  treatment + (1 | vial) + (1 | id),  
  ziformula = ~ treatment,               
  family = poisson(),                          
  data = survivability_between
)

## Assumption checks 

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.zi.p.MFE.surviveboth, plot = T)
# Assumptions are a lot better
# qq doesn't line up great, but the tests are okay - small sample size could be why the qq does not line up great


# easystats
check_overdispersion(glm.zi.p.MFE.surviveboth)
 # no overdispersion 





# Model 4
 # Poisson Negative Binomial #
glm.zi.nb.MFE.surviveboth <- glmmTMB(
  survivability ~  treatment + (1 | vial),
  ziformula =  ~ treatment,
  family = nbinom2(),
  data = survivability_between
)

## Assumption checks

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.zi.nb.MFE.surviveboth, plot = T)
 # This one looks a lot better



# Comparing models: 
AIC(glmm.p.bothsurvive.MFE, glm.nb.MFE.both, glm.zi.p.MFE.surviveboth, glm.zi.nb.MFE.surviveboth)
 ## zeroinflation models are way better,  Poisson Negative Binomial is the best 





## Final chosen model:  Poisson Negative Binomial
glm.zi.nb.MFE.surviveboth <- glmmTMB(
  survivability ~  treatment + (1 | vial),
  ziformula =  ~ treatment,
  family = nbinom2(),
  data = survivability_between
)


#### Data Analysis ####

# Basic analysis
summary(glm.zi.nb.MFE.surviveboth)

# condidence intervals 
exp(confint(glm.zi.nb.MFE.surviveboth))

# getting values for the write-up
emmeans::emmeans(glm.zi.nb.MFE.surviveboth, specs = ~ treatment, type = "response")

# getting a table for the write up
tab_model(glm.zi.nb.MFE.surviveboth, CSS = list(css.table = '+font-family: Arial;'))
