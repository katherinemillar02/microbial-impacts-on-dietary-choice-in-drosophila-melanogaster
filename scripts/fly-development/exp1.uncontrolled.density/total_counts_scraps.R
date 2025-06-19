#### Reading in through scripts ####  ####
source("packages.R") # A script containing all the relevant packages
source("fitness.dataread.R")
####  ####  ####  ####  ####  ####  ####


#### Uncontrolled ####
#### Pupae ####

#### Poisson GLMM 
glmm.p.totalpupae.UMFE <- glmmTMB(total_count ~ 
                                treatment 
                                + (1|vial), 
                             family = poisson, data = overallpupae_UMFE)

## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glmm.p.totalpupae.UMFE, plot = T)
## Assumptions aren't great, new model maybe? 

check_overdispersion(glmm.p.totalpupae.UMFE)
# Overdispersion detected 

check_zeroinflation(glmm.p.totalpupae.UMFE)
## NO zero inflation 

#### Negative Binomial GLMM 
glmm.nb.totalpupae.UMFE <- glmmTMB(total_count ~ 
                                    treatment 
                                  + (1|vial), 
                                  family = nbinom2, data = overallpupae_UMFE)


## Assumption checking:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.UMFE.totalpupae, plot = T)
## Assumptions are a lot better

check_overdispersion(glm.nb.UMFE.totalpupae)
# No overdispersion detected 

# Comparing the two models: 
AIC(glmm.p.totalpupae.UMFE, glm.nb.UMFE.totalpupae)
  # Negative Binomial GLM is better


# Basic analysis
summary(glm.nb.UMFE.totalpupae)
# confidence intervals
exp(confint(glm.nb.UMFE.totalpupae))
## Getting numbers for the write-up
emmeans::emmeans(glm.nb.UMFE.totalpupae, specs =   ~  treatment , type = "response")
# Table for the write-up
tab_model(glm.nb.UMFE.totalpupae, CSS = list(css.table = '+font-family: Arial;'))




#### Flies ####

#### Poisson GLMM 
glmm.p.total.UMFE <- glmmTMB(total_count ~ 
                               sex * treatment
                             + (1|vial ),
                             family = poisson, data = overallflies_UMFE)


#### Assumption checking
# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glmm.p.total.UMFE, plot = T)
## no significant results
# easystats checks
check_overdispersion(glmm.p.total.UMFE)
# Overdispersion detected 
check_zeroinflation(glmm.p.total.UMFE)
## No zero inflation 

#### Negative Binomial GLMM 
glmm.nb.totalflies.UMFE <- glmmTMB(total_count ~ 
                                     sex * treatment 
                                   + (1|vial), 
                                   family = nbinom2, data = overallflies_UMFE)

# Assumption checking
# DHARMa:
simulationOutput <- simulateResiduals(fittedModel = glm.nb.UMFE.flies, plot = T)
## Assumptions seem to be a lot better
# easystats
check_overdispersion(glm.nb.UMFE.flies)
# No overdispersion detected
# Comparing the two models with AIC
AIC(glmm.p.total.UMFE, glmm.nb.totalflies.UMFE)
# Negative Binomial GLM has lower AIC


#### Negative Binomial GLM 
glmm.nb.totalflies.UMFE <- glmmTMB(total_count ~ 
                                    sex *  treatment 
                                   + (1|vial), 
                                   family = nbinom2, data = overallflies_UMFE)

## Looking for interaction effect 
drop1(glmm.nb.totalflies.UMFE, test = "Chisq")
## No 2-way interaction effect found

# Removing the two-way interaction effect 
glmm.nb.totalflies.UMFE.2 <- glmmTMB(total_count ~ 
                                     sex +  treatment 
                                   + (1|vial), 
                                   family = nbinom2, data = overallflies_UMFE)

# For basic analysis
summary(glmm.nb.totalflies.UMFE.2)
# condidence intervals 
exp(confint(glmm.nb.totalflies.UMFE.2))
# getting values for the write-up
emmeans::emmeans(glmm.nb.totalflies.UMFE.2, specs = ~ sex + treatment, type = "response")
# getting a table for the write up
tab_model(glmm.nb.totalflies.UMFE.2, CSS = list(css.table = '+font-family: Arial;'))











#### Controlled ####

#### Fly ####

#### Poisson GLM 
glm.p.MFE.fly <- glm(count ~
                       treatment * sex * time_hours , 
                     
                     family = poisson, data = fly_fitness_tidy_MFE)


#### ASSUMPTION CHECKS 
performance::check_model(glm.p.MFE.fly, check = c("qq")) 
# doesn't show 
performance::check_model(glm.p.MFE.fly, check = c("outliers")) 
# weird? 
performance::check_model(glm.p.MFE.fly, check = c("homogeneity")) 
# really quite bad 
## Checking for overdispersion 
check_overdispersion(glm.p.UMFE.fly) 
## overdispersion detected
## Checking for zeroinflation
check_zeroinflation(glm.p.UMFE.fly) 
## there is zero inflation 


#### Negative Binomial GLM 
glm.nb.MFE.fly <- glm.nb(count ~
                           
                           treatment * sex * time_hours,
                         
                         data = fly_fitness_tidy_MFE)


#### ASSUMPTION CHECKS 
performance::check_model(glm.nb.MFE.fly, check = c("qq")) 
# does not show. 
performance::check_model(glm.nb.MFE.fly, check = c("outliers")) 
performance::check_model(glm.nb.MFE.fly, check = c("homogeneity"))
# looks a bit weird
## Checking for overdispersion 
check_overdispersion(glm.nb.MFE.fly)
## No overdispersion detected 


#### Poisson GLMM 
glmm.p.MFE.fly <- glmmTMB(count ~ 
                            
                            treatment * sex * time_hours
                          
                          + (1|sex/vial) + (1|time_hours),
                          
                          family = poisson, data = fly_fitness_tidy_MFE)
#Assumption checks
## qq plot from the model
residuals <- residuals(glmm.p.MFE.fly)
qqnorm(residuals)
qqline(residuals, col = 2)
## qq residuals do not look great. 

## Performance
check_zeroinflation(glmm.p.MFE.fly)
# there is zero inflation
check_overdispersion(glmm.p.MFE.fly)
# overdispersion detected

#### Zero-Inflated Poisson
zi.p.MFE.fly <- zeroinfl(count ~ 
                           
                           treatment * time_hours * sex  
                         
                         , data = fly_fitness_tidy_MFE)

# Comparing models
AIC(glm.p.MFE.fly,glm.nb.MFE.fly, glmm.p.MFE.fly, zi.p.MFE.fly)

#### Chosen model: Poisson GLMM ####
glmm.p.MFE.fly <- glmmTMB(count ~ 
                            
                            treatment * sex * time_hours
                          
                          + (1|vial/sex) + (1|time_hours),
                          
                          family = poisson, data = fly_fitness_tidy_MFE)

# Using drop1 to look for significance in a 3-way interaction
drop1(glmm.p.MFE.fly, test = "Chisq")
# No 3-way interaction effect


#### Poisson GLMM 
glmm.p.MFE.fly.2 <- glmmTMB(count ~ 
                              
                              treatment * sex +
                              treatment * time_hours +
                              sex * time_hours +
                              
                              + (1|vial/sex) + (1|time_hours),
                            
                            family = poisson, data = fly_fitness_tidy_MFE)

# Using drop1 to look for significance in a 3-way interaction
drop1(glmm.p.MFE.fly.2, test = "Chisq")
# 2 - way interaction between sex and time of emergence. 
# 2 - way interaction between treatment and time of emergence. 




## Final model? 
glmm.p.MFE.fly.3 <- glmmTMB(count ~ 
                              
                              sex * time_hours +
                              treatment * time_hours +
                              
                              
                              + (1|vial/sex) + (1|time_hours),
                            
                            family = poisson, data = fly_fitness_tidy_MFE)


# Doublechecking the correct  interaction effects appear 
drop1(glmm.p.MFE.fly.3, test = "Chisq")

## Data analysis of the chosen model
summary(glmm.p.MFE.fly.3)





# Model 1
#### Poisson GLMM
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
















