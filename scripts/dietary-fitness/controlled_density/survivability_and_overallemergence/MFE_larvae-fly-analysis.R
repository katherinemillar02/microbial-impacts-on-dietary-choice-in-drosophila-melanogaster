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


## Larve to Fly Survivability: 

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
  group_by(id, vial, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) 


## making it a dataframe 
overallflies_MFE <- as.data.frame(overallflies_MFE)


## making a surviability data frame 
fly_survivability <- overallflies_MFE %>%
  mutate(fixed_total = 63,  
         survivability = (total_count / fixed_total)*100 )





#### TESTING MODELS ####


# Model 1
#### Poisson GLMM ####
glmm.p.flysurvive.MFE <- glmmTMB(survivability ~ 
                              
                              treatment +
                              
                              (1|id) + (1|vial) ,
                            
                            family = poisson, data = fly_survivability)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.flysurvive.MFE , plot = T)
## Assumptions aren't great, new model maybe?
## qq doesn't really match at all... 



check_overdispersion(glmm.p.flysurvive.MFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.flysurvive.MFE)
## zero inflation 






# Model 2 
# Negative Binomial GLM
glm.nb.flysurvive.MFE <- glm.nb(survivability ~
                             
                             treatment,
                           
                           data = fly_survivability)




## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.flysurvive.MFE, plot = T)
## Assumptions aren't great, new model maybe? 
## qq still is not matching up great 


check_overdispersion(glm.nb.flysurvive.MFE)
# overderdispersion detected 

check_zeroinflation(glm.nb.flysurvive.MFE)
## zero inflation 







## Model 3 
glm.p.flysurvive.MFE <- glm(survivability ~
                            treatment, 
                            family = poisson, data = fly_survivability)



## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.p.flysurvive.MFE, plot = T)
## Assumptions aren't great, new model maybe? 
## qq still is not matching up great 


check_overdispersion(glm.p.flysurvive.MFE)
# Overderdispersion detected 

check_zeroinflation(glm.p.flysurvive.MFE)
## zero inflation 









# Model 4 
glm.zi.p.flysurvive.MFE <- glmmTMB(
  survivability ~ treatment + (1 | vial) + (1 | id),  
  ziformula = ~ treatment,               
  family = poisson(),                          
  data = fly_survivability
)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.zi.p.flysurvive.MFE, plot = T)
## Assumptions aren't great, new model maybe? 
## qq still is not matching up great but looks ok



check_overdispersion(glm.zi.p.flysurvive.MFE)
# No overderdispersion detected 

check_zeroinflation(glm.zi.p.flysurvive.MFE)
## NO zero inflation 


## Comparing models 
AIC(glmm.p.flysurvive.MFE,glm.nb.flysurvive.MFE,glm.p.flysurvive.MFE,glm.zi.p.flysurvive.MFE)
# Zero Inflated Poisson has the lowest AIC 



## Using this model 

# Basic analysis:
summary(glm.zi.p.flysurvive.MFE)


## Getting numbers for the write-up
emmeans::emmeans(glm.zi.p.flysurvive.MFE, specs =  ~ ratio *  density , type = "response")


# Table for write-up
tab_model(glm.zi.p.flysurvive.MFE, CSS = list(css.table = '+font-family: Arial;'))


