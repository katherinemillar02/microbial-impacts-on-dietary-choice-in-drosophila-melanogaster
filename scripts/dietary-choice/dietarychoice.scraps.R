

#### Chapter 2  #### 

# This script includes all the analysis I did to get to the final model. To see the scripts containing the chosen models for the write-up, please see:



#### Packages and Data Read in 📦📦📦📦 #### 
library(ggpubr)
source("packages.R")
source("scripts/dietary-choice/dietarychoice.dataread.R")
#### 
-
#Basic model outline: 
 # (fly_numbers 
 # ~ ratio * condition * block 
 #   + (1 | block / plate) + (1 | observation), 
 # data = data)

#Read me: 
#fly_numbers: the number of flies present on a singular diet patch in a dish per observation
#ratio: the protein: carbohydrate ratio of the diet (4:1 or 1:4). 
#condition: whether the fly has previously been conditioned or not by another fly (conditioned or unconditioned).
#observation: one half an hour time point, across the span of about 6 hours (1-11)
#block: a repeat of the experiment (1-4)







#### Male ####

#### Male 4 choice feeding 

#### 1. Preliminary Data Analysis ####

# Model 1 - glmm.m.4choice.2
#### Poisson GLMM ####
glmm.m.4choice <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition * block 
                          
                          + (1 | block / plate) + (1 | observation), 
                          
                          family = poisson, data = combined_m_split)

# Testing without random effects
glmm.m.4choice.noRE <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition * block, 
                          
                          family = poisson, data = combined_m_split)

# LRT  to check relevance of random effects
anova(glmm.m.4choice, glmm.m.4choice.noRE)
 # random effects are significant - probably needed 


# Model to go with 
glmm.m.4choice <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition * block 
                          
                          + (1 | block / plate) + (1 | observation), 
                          
                          family = poisson, data = combined_m_split)

## Assumption checks:

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice, plot = T)
## Assumptions of the model look pretty good
#Uniformity good 
#Dispersion good 
#Outlier good  
#Homogeneity of variance good 



# easystats checks 
check_overdispersion(glmm.m.4choice)
# Overdispersion is detected

check_zeroinflation(glmm.m.4choice) 
# Zeroinflation detected 



#### Model 2
#### Negative Binomial GLMM #### incase overdispersion is a problem 
glmm.nb.m.4choice <- glmmTMB(fly_numbers ~ ratio * condition * block +
                              
                              (1 | block / plate) + (1 | observation),
                            
                            family = nbinom2, data = combined_m_split)


check_overdispersion(glmm.nb.m.4choice)
# Overdispersion no longer detected 

check_zeroinflation(glmm.nb.m.4choice)
# Zeroinflation no longer detected  




# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glmm.nb.m.4choice, plot = T)
## Assumptions of the model look pretty good 
#Uniformity good 
#Dispersion good 
#Outlier good  
#Homogeneity of variance good 




# Zeroinflation Poisson 
## Trying zeroinflation model as there was zeroinflation previously, and overdispersion might not be an issue
glmm.zi.p.m.4choice <- glmmTMB(fly_numbers ~ ratio * condition * block +
                                      (1 | block / plate) + (1 | observation),
                                    ziformula = ~ 1, 
                                    family = poisson, data = combined_m_split)


check_overdispersion(glmm.zi.p.m.4choice)
 # No overdispersion detected 

check_zeroinflation(glmm.zi.p.m.4choice)
# No zeroinflation detected 

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glmm.zi.p.m.4choice, plot = T)
# looks pretty good 
#Uniformity good 
#Dispersion good 
#Outlier good  
#Homogeneity of variance good 




# Final model choice - combo of negBin and zero inflation
glmm.zi.nb.m.4choice <- glmmTMB(fly_numbers ~ ratio * condition * block +
                                 (1 | block / plate) + (1 | observation),
                               ziformula = ~ 1,  
                               family = nbinom2,
                               data = combined_m_split)

      ## cannot actually test as there is a convergence problem ???? 



# Comparing models... 
AIC(glmm.nb.m.4choice, glmm.m.4choice, glmm.zi.p.m.4choice, glmm.zi.nb.m.4choice)
# The mixed model is better, despite there being overdispersion 
# lowest is zeroinfl 





## Using this
# Testing for a 3-way interaction
glm.zi.p.m.4choice <- glmmTMB(fly_numbers ~ ratio * condition * block +
                                (1 | block / plate) + (1 | observation),
                              ziformula = ~ 1, 
                              family = poisson, data = combined_m_split)


# Significance of 3-way interaction
drop1(glm.zi.p.m.4choice, test = "Chisq")
# No 3-way interaction 


# Testing for a 2-way interaction
glm.zi.p.m.4choice.2 <- glmmTMB(fly_numbers 
                            
                            ~ ratio * condition +
                              condition * block +
                              ratio * block
                            
                            + (1 | block / plate) + (1 | observation), 
                            ziformula = ~ 1,
                            
                            family = poisson, data = combined_m_split)

# Significance of 2-way interaction
drop1(glm.zi.p.m.4choice.2, test = "Chisq")
# Significant interaction between condition and block



#### Final model 
glm.zi.p.m.4choice.3 <- glmmTMB(fly_numbers 
                            
                            ~ 
                              condition * block + ratio
                            
                            
                            + (1 | block / plate) + (1 | observation), 
                            ziformula = ~ 1,
                            
                            family = poisson, data = combined_m_split)


## testing if random effects still make a difference 
glm.zi.p.m.4choice.3.noRE <- glmmTMB(fly_numbers 
                                
                                ~ 
                                  condition * block + ratio,
                                
                                
                                
                                ziformula = ~ 1,
                                
                                family = poisson, data = combined_m_split)

# Random Effect Test
anova(glm.zi.p.m.4choice.3, glm.zi.p.m.4choice.3.noRE)
 ## they do 

#### Data Analysis for write-up ####

# Basic analysis 
summary(glm.zi.p.m.4choice.3.noRE)

# Confidence intervals 
exp(confint(glm.zi.p.m.4choice.3.noRE))


# Real values for write-up
emmeans::emmeans(glm.zi.p.m.4choice.3.noRE, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.zi.p.m.4choice.3.noRE, CSS = list(css.table = '+font-family: Arial;')) 











#### Virgin Female #### 

#### Reading, cleaning and binding the data ####

#### Reading in the data
fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx")
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")
fourone_onefour_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx")

## Mutating a block variable 
fourone_onefour_virgin_b1 <- fourone_onefour_virgin_b1  %>% mutate(block = "one")
fourone_onefour_virgin_b2 <- fourone_onefour_virgin_b2  %>% mutate(block = "two")
fourone_onefour_virgin_b3 <- fourone_onefour_virgin_b3  %>% mutate(block = "three")
fourone_onefour_virgin_b4 <- fourone_onefour_virgin_b3  %>% mutate(block = "four")

# Binding the data
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2, fourone_onefour_virgin_b3, fourone_onefour_virgin_b4)

# Making the data long
combined_vf <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Splitting diet up into ratio and condition
combined_vf_split <- combined_vf %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


#### 1. Preliminary Data Analysis

# Model 1
#### Poisson GLMM ####
glmm.vf.4choice <- glmmTMB(fly_numbers 
                           
                           ~ ratio * condition * block 
                           
                           + (1 | block / plate) + (1 | observation), 
                           
                           family = poisson, data = combined_vf_split)




## Assumption checks: 

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glmm.vf.4choice, plot = T)
## Assumptions of the model look pretty good but there is a significant test with dispersion


# easystats checks 
check_overdispersion(glmm.vf.4choice)
# Overdispersion is detected

check_zeroinflation(glmm.vf.4choice)
# Zeroinflation IS NOT detected 






# As there is overdispersion, trying a NegBin GLM 

# Model 2
#### Negative Binomial GLM ####
glmm.nb.vf.4choice <- glmmTMB(fly_numbers ~ ratio * condition * block +
                               
                               (1 | block / plate) + (1 | observation),
                             
                             family = nbinom2, data = combined_vf_split)

## no RE 
glmm.nb.vf.4choice.noRE <- glmmTMB(fly_numbers ~ ratio * condition * block, 
                              
                              family = nbinom2, data = combined_vf_split)



anova(glmm.nb.vf.4choice, glmm.nb.vf.4choice.noRE)
 # Not significant, not worth included 
AIC(glmm.nb.vf.4choice, glmm.nb.vf.4choice.noRE)
 # Not much difference in AIC


simulationOutput <- simulateResiduals(fittedModel = glmm.nb.vf.4choice, plot = T)
 # good 

simulationOutput <- simulateResiduals(fittedModel = glmm.nb.vf.4choice.noRE, plot = T)
 # maybe slightly better 







# For now, including random effects as I don't understand the implications of not including them. 

# easystats checks 
check_overdispersion(glm.nb.m.4choice)
# Overdispersion is NO LONGER detected

check_zeroinflation(glm.nb.m.4choice)
# Zeroinflation IS STILL NOT detected 





# Going to chooose NegBin but doing AIC just in case 
AIC(glmm.vf.4choice, glmm.nb.vf.4choice)
# NegBin AIC only slightly lower 






## Choosing this model: 
# Testing for a 3-way significant effect 
glmm.nb.vf.4choice <- glmmTMB(fly_numbers ~ ratio * condition * block +
                                
                                (1 | block / plate) + (1 | observation),
                              
                              family = nbinom2, data = combined_vf_split)



# Significance of 3-way interaction
drop1(glm.nb.m.4choice, test = "Chisq")
# No significant interaction


## Testing for 2-way interaction effects

glmm.nb.vf.4choice.2 <- glmmTMB(
  fly_numbers ~ ratio * condition + condition * block + ratio * block + 
    (1 | block / plate) + (1 | observation),
  family = nbinom2,
  data = combined_vf_split
)





# Looking for significance in the 2-way interaction effecrs
drop1(glmm.nb.vf.4choice.2, test = "Chisq")
# Only a significant interaction between condition and block 


# Final model 

glmm.nb.vf.4choice.3 <- glmmTMB(  fly_numbers ~ 
  condition * block + ratio  + 
    (1 | block / plate) + (1 | observation),
  family = nbinom2,
  data = combined_vf_split
)


glmm.nb.vf.4choice.3.noRE <- glmmTMB(  fly_numbers ~ 
                                    condition * block + ratio, 
                                   
                                  family = nbinom2,
                                  data = combined_vf_split
)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glmm.nb.vf.4choice.3)

# Confidence intervals 
exp(confint(glmm.nb.vf.4choice.3))


# Real values for write-up
emmeans::emmeans(glmm.nb.vf.4choice.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glmm.nb.vf.4choice.3, CSS = list(css.table = '+font-family: Arial;')) 


# checking random effects again 
anova(glmm.nb.vf.4choice.3, glmm.nb.vf.4choice.3.noRE)



simulationOutput <- simulateResiduals(fittedModel = glmm.nb.vf.4choice.3, plot = T)
simulationOutput <- simulateResiduals(fittedModel = glmm.nb.vf.4choice.3.noRE, plot = T)





#### OvoD1 Female ####

#### Reading, binding and cleaning the data ####

## Reading the data in
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")

# Mutating a block variable 
fourone_onefour_ovod1_b1 <- fourone_onefour_ovod1_b1  %>% mutate(block = "one")
fourone_onefour_ovod1_b2 <- fourone_onefour_ovod1_b2  %>% mutate(block = "two")

# Binding the separate blocks 
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)

## Cleaning the data, adding relevant variables
combined_of <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Splitting the data up 
combined_of_split <- combined_of %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


#### 1. Preliminary Data Analysis 

# Model 1
#### Poisson GLMM ####
glmm.of.4choice <- glmmTMB(fly_numbers ~ 
                             ratio * condition * block 
                           + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_of_split)



## Assumption checks: 

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glmm.of.4choice, plot = T)
## Assumptions of the model look pretty good, but there is a significant test for homogeneity


# easystats checks 
check_overdispersion(glmm.of.4choice)
# Overdispersion is detected

check_zeroinflation(glmm.of.4choice)
# Zeroinflation IS detected 




# As there is overdispersion AND zeroinflation, trying a Negative Binomial GLM

# Model 2

#### Negative Binomial GLM #### 
glm.nb.of.4choice <- glm.nb(fly_numbers ~ 
                              ratio * condition * block,
                            data = combined_of_split)




## Assumption checks: 

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.of.4choice, plot = T)
## Assumptions of the model look pretty good


# easystats checks 
check_overdispersion(glm.nb.of.4choice)
# Overdispersion is NO LONGER detected

check_zeroinflation(glm.nb.of.4choice)
# Zeroinflation IS NO LONGER detected 

# Comparing models 
AIC(glm.nb.of.4choice, glmm.of.4choice)
# Mixed model is better but has zeroinflation and overdispersion 







#### Negative Binomial GLM #### 

# 3-way interaction test 
glm.nb.of.4choice <- glm.nb(fly_numbers ~ 
                              ratio * condition * block,
                            data = combined_of_split)


# Testing for significance in a 3-way interaction
drop1(glm.nb.of.4choice, test = "Chisq")
# No significant 3-way interaction found



# 2-way interaction tests 
glm.nb.of.4choice.2 <- glm.nb(fly_numbers ~ 
                                ratio * condition +
                                block * condition + 
                                ratio * block,
                              data = combined_of_split)


# Testing for significance in 2-way interactions
drop1(glm.nb.of.4choice.2, test = "Chisq")
# 2 way interaction between ratio and condition found 


## Final chosen model 
glm.nb.of.4choice.3 <- glm.nb(fly_numbers ~  
                                ratio * condition + block
                              ,
                              data = combined_of_split)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glm.nb.of.4choice.3)

# Confidence intervals 
exp(confint(glm.nb.of.4choice.3))


# Real values for write-up
emmeans::emmeans(glm.nb.of.4choice.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.nb.of.4choice.3, CSS = list(css.table = '+font-family: Arial;')) 











#### Packages 📦📦📦📦 #### 
library(tidyverse)
library(lmerTest)
library(readxl)
library(DHARMa)
library(glmmTMB)
library(lme4)
library(performance)
library(pscl)
library(MASS)
library(sjPlot)



#### Male #### 
#### Reading, binding, cleaning data 📖 ####

# 4:1 + 1:4 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_1-4_b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_1-4_b2_oviposition.xlsx")
# Mutating a block variable
fourone_onefour_male_oviposition_b1 <- fourone_onefour_male_oviposition_b1 %>% mutate(block = "one")
fourone_onefour_male_oviposition_b2 <- fourone_onefour_male_oviposition_b2%>% mutate(block = "two")
# Binding the data for 4:1/1:4 
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)

## Making the data different dataframes
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Splitting "diet" up
combined_ovi_m_split <- combined_ovi_m %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")








#### 1. Preliminary Data Analysis ####


#### Poisson GLMM ####
comb_m_egg_glm.p <- glm(egg_numbers ~ ratio * condition * block, family = poisson,  combined_ovi_m_split)


## Assumption checks 

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = comb_m_egg_glm.p, plot = T)
# Quite a bad model, all tests are significant 

# easystats / performance checks 
# Testing for overdispersion
check_overdispersion(comb_m_egg_glm.p)
# Overdispersion detected

# Testing for zeroinflation
check_zeroinflation(comb_m_egg_glm.p)
# There is probabale zeroinflation





#### Negative Binomial GLM ####
comb_m_egg_glm.nb <- glm.nb(egg_numbers ~ ratio * condition * block, data =  combined_ovi_m_split)

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = comb_m_egg_glm.nb, plot = T)
# Model looks a lot better, still a positive dispersion tests though

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(comb_m_egg_glm.nb)
# Undersipersion is now detected - what to do? 

# Testing for zeroinflation
check_zeroinflation(comb_m_egg_glm.nb)
# There is now an overfitting of zeros




#### Poisson GLMM ####
comb_m_egg_glmm.p <- glmmTMB(egg_numbers ~ ratio * condition * block + (1|block/plate) , family = poisson, data = combined_ovi_m_split)

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = comb_m_egg_glmm.p, plot = T)
# Quite a bad model, all tests are significant 

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(comb_m_egg_glmm.p)
# Overdispersion detected

# Testing for zeroinflation
check_zeroinflation(comb_m_egg_glmm.p)
# There is an underfitting of zeros


## None of these models are great - may need to go back to these... 

#### Comparing models 
AIC(comb_m_egg_glm.p, comb_m_egg_glm.nb, comb_m_egg_glmm.p)
# NegBin GLM has lowest AIC 



# Using this model 
# Testing for 3-way interaction 
comb_m_egg_glm.nb <- glm.nb(egg_numbers
                            ~ ratio * condition * block, 
                            data =  combined_ovi_m_split)



# Using drop1 to test for the significance of the 3-way interaction 
drop1(comb_m_egg_glm.nb, test = "Chisq")
# No 3-way interaction 


# Testing for 2-way interactions 
comb_m_egg_glm.nb.2 <- glm.nb(egg_numbers
                              ~ ratio * condition + 
                                ratio * block + 
                                condition * block, 
                              data =  combined_ovi_m_split)


# Using drop1 to test for the significance of the 2-way interaction 
drop1(comb_m_egg_glm.nb.2, test = "Chisq")
# No 2-way interactions


# Final model 
comb_m_egg_glm.nb.3 <- glm.nb(egg_numbers
                              ~ ratio + condition + block, 
                              data =  combined_ovi_m_split)



#### Data analysis for write-up #### 

# Basic analysis 
summary(comb_m_egg_glm.nb.3)


# Confidence intervals 
exp(confint(comb_m_egg_glm.nb.3))


# Real values for write-up
emmeans::emmeans(comb_m_egg_glm.nb.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(comb_m_egg_glm.nb.3, CSS = list(css.table = '+font-family: Arial;'))






#### Virgin Female ####
#### Reading, binding, cleaning data 📖 ####

## Reading in the different data-sets
fourone_onefour_oviposition_virgin_b2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx")
fourone_onefour_oviposition_virgin_b3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx")
fourone_onefour_oviposition_virgin_b4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx")
## Mutating a block variable to the data-sets
fourone_onefour_oviposition_virgin_b2 <- fourone_onefour_oviposition_virgin_b2 %>% mutate(block = "two")
fourone_onefour_oviposition_virgin_b3 <- fourone_onefour_oviposition_virgin_b3 %>% mutate(block = "three")
fourone_onefour_oviposition_virgin_b4 <- fourone_onefour_oviposition_virgin_b4 %>% mutate(block = "four")

## Binding the different data-sets
fourone_onefour_oviposition_virgin <- rbind(fourone_onefour_oviposition_virgin_b2, fourone_onefour_oviposition_virgin_b3, fourone_onefour_oviposition_virgin_b4)


## adding some data names
combined_ovi_v <- fourone_onefour_oviposition_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Using separate to split the diet into rartio and condition 
combined_ovi_v_split <- combined_ovi_v %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")








#### 1. Preliminary Data Analysis ####
#### TESTING MODELS ####

#### Poisson GLM ####
comb_v_egg_glm.p <- glm(egg_numbers ~ ratio * condition * block, family = poisson,  combined_ovi_v_split)

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = comb_v_egg_glm.p, plot = T)
# Quite a bad model, all tests are significant 

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(comb_v_egg_glm.p)
# Overdispersion detected

# Testing for zeroinflation
check_zeroinflation(comb_v_egg_glm.p)
# There is an underfitting of zeros




#### Poisson GLMM ####
glm_mm_v_egg <- glmmTMB(egg_numbers
                        ~ ratio * condition * block 
                        + (1| block / plate) , family = poisson, data = combined_ovi_v_split)


# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm_mm_v_egg, plot = T)
# A bit better, but still significant tests

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm_mm_v_egg)
# Overdispersion detected

# Testing for zeroinflation
check_zeroinflation(glm_mm_v_egg)
# There is an underfitting of zeros





#### Negative Binomial GLM ####
glm.nb_v_comb_egg <- glm.nb(egg_numbers
                            ~ ratio * condition * block, 
                            data =  combined_ovi_v_split)



# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.nb_v_comb_egg, plot = T)
# A LOT better,  no significant tests

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm.nb_v_comb_egg)
# Overdispersion NOT detected

# Testing for zeroinflation
check_zeroinflation(glm.nb_v_comb_egg)
# There is an underfitting of zeros - still zeroinflation 


#### Zero inflation tests! ####

# Zero inflated poisson ####
glm.zi.p.v.egg <- glmmTMB(
  egg_numbers   ~ ratio * condition * block + (1| block / plate),  
  ziformula =  ~ ratio * condition * block,               
  family = poisson(),                          
  data = combined_ovi_v_split
)


# Assumption checks 

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.zi.p.v.egg , plot = T)
# Some significant tests

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm.zi.p.v.egg )
# Overdispersion IS detected

# Testing for zeroinflation
check_zeroinflation(glm.zi.p.v.egg )
# There is no longer zeroinflation 


# Because there is overdispersion, trying zeroinflation witn NegBin



#### Zero Inflated Negative Binomial ####
glm.zi.nb.v.egg  <- glmmTMB(
  egg_numbers   ~ ratio * condition * block + (1| block / plate),  
  ziformula =  ~ ratio * condition * block,               
  family = nbinom2(),                          
  data = combined_ovi_v_split
)

# Assumption checks 

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.zi.nb.v.egg, plot = T)
# Model is looking pretty cracking 

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm.zi.nb.v.egg)
# Overdispersion IS NOT detected

# Testing for zeroinflation
check_zeroinflation(glm.zi.nb.v.egg)
# There is no longer zeroinflation 

# This model ^^ seems best... 
#### Comparing models just to check... 

AIC(comb_v_egg_glm.p, glm_mm_v_egg, glm.nb_v_comb_egg, glm.zi.p.v.egg, glm.zi.nb.v.egg)
# NegBin without zeroinflation has better assumptions but there is zeroinflation so still choosing Zero-Inflated Negative Binomial 




#### Chosen model: Zero-Inflated Negative Binomial... 

# Testing for a 3-way interaction effect
# Note: this model only seems to work with the random effect how it is and not with block
glm.zi.nb.v.egg  <- glmmTMB(
  egg_numbers   ~ ratio * condition * block + (1| plate),  
  ziformula =  ~ ratio * condition * block,               
  family = nbinom2(),                          
  data = combined_ovi_v_split
)
# Testing for significance in the 3-way interaction effect
drop1(glm.zi.nb.v.egg, test = "Chisq")
# Significant 3-way interaction


#### 2. Data analysis for write-up ####

# Basic analysis 
summary(glm.zi.nb.v.egg)


# Confidence intervals 
exp(confint(glm.zi.nb.v.egg))


# Real values for write-up
emmeans::emmeans(glm.zi.nb.v.egg, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.zi.nb.v.egg, CSS = list(css.table = '+font-family: Arial;'))






#### OvoD1 Female #### 


#### Reading, binding, cleaning data 📖 ####
## Reading the data in 
combined_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
combined_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")

# Mutating a block variable 
combined_ovod1_b1 <- combined_ovod1_b1  %>% mutate(block = "one")
combined_ovod1_b2 <- combined_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
combined_ovod1 <- rbind(combined_ovod1_b1, combined_ovod1_b2)

# Making the data long
combined_of_egg  <- combined_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Splitting up diet variable 
combined_of_egg_split <- combined_of_egg %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")



#### 1. Preliminary Data Analysis ####

#### Poisson GLM ####
comb_ovi_egg_glm.p <- glm(egg_numbers ~ ratio * condition * block, family = poisson,  combined_of_egg_split)

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = comb_ovi_egg_glm.p, plot = T)
# Model is looking quite bad 
# As this isn't a mixed model, is it ok to do assumption checks like this? 

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(comb_ovi_egg_glm.p)
# Overdispersion IS detected

# Testing for zeroinflation
check_zeroinflation(comb_ovi_egg_glm.p)
# There is NO zeroinflation 







#### Poisson GLMM ####
glmm.p.ovo.egg <- glmmTMB(egg_numbers ~ ratio * condition * block 
                          + (1| block /plate) , family = poisson, data = combined_of_egg_split)





# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glmm.p.ovo.egg, plot = T)
# Model is looking quite bad 




# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glmm.p.ovo.egg)
# Overdispersion IS detected

# Testing for zeroinflation
check_zeroinflation(glmm.p.ovo.egg)
# There is NO zeroinflation 


# Because there is overdispersion, trying a NegBin GLM



#### Negative Binomial Generalised Linear Model ####
glm.nb_ovo_comb_egg <- glm.nb(egg_numbers
                              ~ ratio * condition * block, 
                              data =  combined_of_egg_split)



# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.nb_ovo_comb_egg, plot = T)
# Model is looking quite good, no significant tests




# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm.nb_ovo_comb_egg)
# Overdispersion IS detected

# Testing for zeroinflation
check_zeroinflation(glmm.p.ovo.egg)
# There is NO zeroinflation 



# Comparing models 
AIC(comb_ovi_egg_glm.p, glmm.p.ovo.egg, glm.nb_ovo_comb_egg)



# Using this model 
# Testing for a 3-way interaction: 
glm.nb_ovo_comb_egg <- glm.nb(egg_numbers
                              ~ ratio * condition * block, 
                              data =  combined_of_egg_split)



# Testing for  a significant  3-way interaction 
drop1(glm.nb_ovo_comb_egg, test = "Chisq")
# No 3-way interaction 


## Now testing for two-way interactions
glm.nb_ovo_comb_egg.2 <- glm.nb(egg_numbers
                                ~ ratio * condition + 
                                  condition * block + 
                                  ratio * block, 
                                data =  combined_of_egg_split)


# Testing for  a significant  2-way interactions 
drop1(glm.nb_ovo_comb_egg.2, test = "Chisq")
# condition and block significant 
# ratio and block significant 


# Final model, with the 2-way interactions
glm.nb_ovo_comb_egg.3 <- glm.nb(egg_numbers
                                
                                ~ condition * block + 
                                  ratio * block, 
                                
                                data =  combined_of_egg_split)


#### Data Analysis for write-up #### 

# Basic analysis
summary(glm.nb_ovo_comb_egg.3)

# Confidence intervals 
exp(confint(glm.nb_ovo_comb_egg.3))


# Real values for write-up
emmeans::emmeans(glm.nb_ovo_comb_egg.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.nb_ovo_comb_egg.3, CSS = list(css.table = '+font-family: Arial;'))




#### Chapter 2 ####

#### Packages #### 📦📦📦📦
library(tidyverse)
library(lmerTest)
library(readxl)
library(DHARMa)
library(glmmTMB)
library(lme4)
library(performance)
library(pscl)
library(MASS)
############### 📦📦📦📦




#### READING THE DATA IN: 

####################### --
#### VIRGIN FEMALE ####
####################### --

## Creating a path to virgin conditioning oviposition data: 
pathvirginoviposition <- "data/female_conditioning/virgin"

# Reading in Oviposition data for Virgin Conditioning 
read_raw_virgin_oviposition <-function(path = pathvirginoviposition, pattern_to_exclude = "4-1_1-4_oviposition"){
  list_of_files <- list.files(path = pathvirginoviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}



# using the function to find the path 
read_raw_virgin_oviposition(pathvirginoviposition)

#### Removing "Block 1" from the data-set, as this is not to be used for oviposition. 


## Creating a data-set that will read the paths
# first data frame - purr package 
df_virgin_oviposition <- pathvirginoviposition %>% 
  map_df(~read_raw_virgin_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently


## Mutating a variable for block 
df_virgin_oviposition <- df_virgin_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b2") ~ "two",
    str_detect(id, "b3") ~ "three",
    str_detect(id, "b4") ~ "four",
  ))


# Generating a new data-frame, separating diet. 
df2_virgin_oviposition <- df_virgin_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>% #separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ##  group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 


## New data 
df2_virgin_oviposition # does it recognise condition from the long data








############### ---
#### OVOD1 FEMALE ----
################# ---

# Creating a path to the appropriate data files 
pathovod1oviposition <- "data/female_conditioning/ovod1"

## Creating a function to read the appropriate data files
######################################################################################################################## --
read_raw_ovod1_oviposition <-function(path = pathovod1oviposition, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathovod1oviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
######################################################################################################################## --





## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovod1_oviposition(pathovod1oviposition) 


## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovod1_oviposition <- pathovod1oviposition %>% 
  map_df(~read_raw_ovod1_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently


## Mutating a variable for block
df_ovod1_oviposition <- df_ovod1_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    
  ))


# uses what was generated with "df"
df2_ovod1_oviposition <- df_ovod1_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_ovod1_oviposition # does it recognise condition from the long data? 









################ --
#### MALE ####
################ --

## Creating a path to get to the data
pathmaleoviposition <- "data/male_conditioning"


################################################################################################################ --
read_raw_male_oviposition <-function(path = pathmaleoviposition, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathmaleoviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
################################################################################################################--


## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male_oviposition(read_raw_male_oviposition)



## creating an actual data set that will read the paths
# first data frame - purr package 
df_male_oviposition <- pathmaleoviposition %>% 
  map_df(~read_raw_male_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently



## Mutating a variable for block 
df_male_oviposition <- df_male_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two"
  ))




# uses what was generated with "df"
df2_male_oviposition <- df_male_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_male_oviposition # does it recognise condition from the long data? 










######################################################################################################################## --
#### Data Analysis ####
# 4:1 and 1:4 Two-Choice Assays ####
######################################################################################################################## --


########################### --
#### Male Oviposition #####
########################### --


## Model 1 
# Binomial GLM model
glm.bin.m.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) 
                             ~ ratio * block, family = binomial, data = df2_male_oviposition)

## Assumption checking 

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.bin.m.ovi.2choice, plot = T)
# Model could be better 

## easystats
check_overdispersion(glm.bin.m.ovi.2choice)
# overdispersion detected

check_zeroinflation(glm.bin.m.ovi.2choice)
# cannot do zeroinflation


## Homogeneity 
performance::check_model(glm.bin.m.ovi.2choice, check = c("homogeneity")) 
## there is still some dispersion between variables




## Model 2
## Trying a Binomial GLMM to consider random effects. 
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                                ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_male_oviposition)


## Assumption Checking 

## DHARMa
simulation_Output <- simulateResiduals(fittedModel = glmm.bin.m.ovi.2choice, plot = T)
# slighlty better, but there is deviation


# easystats dispersion check 
check_overdispersion(glmm.bin.m.ovi.2choice) 
# No overdispersion detected 



## Note, the KS test is not great, not sure what other model could be used? 




## Comparing the two models 
AIC(glm.bin.m.ovi.2choice, glmm.bin.m.ovi.2choice)
## It says the mixed model has a much lower AIC,
## even though I think the assumptions look a bit worse? 




## Choosing the mixed model for now: 
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~
                                  ratio * block + (1|block/plate)  , family = binomial, data = df2_male_oviposition)

## Looking for the significance in Block
drop1(glmm.bin.m.ovi.2choice, test = "Chisq") 
## says block is quite significant, keeping it in the model






################################## --
### OvoD1 Female  Oviposition ####
################################## --


## Model 1
# GLM Binomial #
glm.bin.of.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_ovod1_oviposition)

# Assumption checking 

## Homogeneity
# easystats plot
performance::check_model(glm.bin.of.ovi.2choice , check = c("homogeneity")) ## the dots are not really lines up 
# first one should be homogeneity, not sure it looks great 


## Understanding homogeneity: 
# Assumptions: the level of variance for a particular variable is constant across the sample 
# Groups of data: the variance of the outcome variable should be the same in each group
# So, I think the dots should sort of be in line with eachother? 
# This doesn't really happen, so there isn't a great assumption of homogeneity? 

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glm.bin.of.ovi.2choice, plot = T)
## This model is worse, all tests are significant 






## Model 2 ##

## Binomial GLMM - to consider random effects 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) 
                                 ~ ratio * block + (1|block/plate)
                                 , family = binomial, data = df2_ovod1_oviposition)


# Assumption checking 


## Homogeneity check 
performance::check_model(glmm.bin.of.ovi.2choice, check = c("homogeneity")) 
## There is still not great homogeneity assumptions 
## This means the level of variance is not really constant still 



## MORE ASSUMPTIONS CHECKS 
# DHARMa 
## More DHARMa assumption checks 
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.of.ovi.2choice, plot = T)
## DHARMa assumption checks show the model to look a lot better 



# easystats check for overdispersion 
check_overdispersion(glmm.bin.of.ovi.2choice)
## No overdispersion detected 



## Doing an AIC check
AIC(glm.bin.of.ovi.2choice, glmm.bin.of.ovi.2choice)
## The model with random effects (Model 2) is a lot better, as expected from the other assumptions... 

## Using "Model 2" for now as I do not really know what else to do 


## Model choice: 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ 
                                   ratio * block + (1|block/plate)  , family = binomial, data = df2_ovod1_oviposition)




# Testing for a 2-way interaction effect
drop1(glmm.bin.of.ovi.2choice, test = "Chisq")
# There is a significant 2-way interaction effect



# The final chosen model... 
summary(glmm.bin.of.ovi.2choice)




############################## --
## Virgin Female Oviposition #### 
############################## --

## Model 1 

# Binomial model 
glm.bin.vf.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) ~ 
                                ratio * block, family = binomial, data = df2_virgin_oviposition)




# Assumption checking 


## Testing homogeneity assumptions 
performance::check_model(glm.bin.vf.ovi.2choice, check = c("homogeneity"))
## there seems to be quite a lot of variation 


# DHARMa assumption checks 
## Doing more DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glm.bin.vf.ovi.2choice, plot = T)
## Things do not line up too great, model is pretty bad 


## easystats overdispersion checker 
check_overdispersion(glm.bin.vf.ovi.2choice)
## There is overdispersion





## Model 2
## a glm mixed model
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                                 ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_virgin_oviposition)

# Assumption checking 


## Homogeneity assumption checks 
performance::check_model(glmm.bin.vf.ovi.2choice, check = c("homogeneity"))
## there seems to be lots of variation in the points 


##  DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.vf.ovi.2choice, plot = T)
# The model checks look pretty good 


## easystats dispersion check
check_overdispersion(glmm.bin.vf.ovi.2choice) 
## No overdispersion detected 


## Comparing models
AIC(glm.bin.vf.ovi.2choice, glmm.bin.vf.ovi.2choice)
## The mixed model has lower AIC 



# Final chosen model: Binomial GLMM
## Using the mixed model for now 
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~
                                   ratio * block + (1|block/plate)  , family = binomial, data = df2_virgin_oviposition)


## Looking for significance in block
drop1(glmm.bin.vf.ovi.2choice, test = "Chisq") ## block is significant
# Block is significant, keeping block in 



# Final chosen model:
summary(glmm.bin.vf.ovi.2choice)


##### Packages 📦📦📦📦 ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
##################---



#### Reading the data in: 
####################################-
#### WILD TYPE MALE CONDITIONING ####
####################################-

## Creating a path to the scripts for treatment 2 condtioning
malepath <- "data/male_conditioning"


## This creates a function, that finds the path, excludes the 4-1 and 1-4 assay
# Mutates a variable for the data file name (id) 
# Mutates the data frame to have a variable for the amount of flies on a diet and the diet 

############################################################################################################---
read_raw_male <- function(path = malepath, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = malepath,
                              pattern = "rawdata", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}


############################################################################################################-






## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male(malepath) # this will show the new data set 


## creating an actual data set that will read the paths
# first data frame - purr package 
df_male <- malepath %>% 
  map_df(~read_raw_male(.x)) #.x is a string or NULL - only applies to dfr apparently
# this will actually give it a label 

# mutating a variable for block from the data id 
df_male <- df_male %>%
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    
  ))

# Separate diet column and group by relevant variables
df2_male <- df_male %>%
  separate(diet, into = c("ratio", "treatment"), sep = " ") %>%
  group_by(id, observation, plate, ratio, treatment, block) %>%
  summarise(count = sum(fly_numbers)) %>%
  pivot_wider(names_from = "treatment", values_from = "count") 

## The data set 
df2_male









####################################-
#### VIRGIN FEMALE CONDITIONING  ----
####################################-

## Creating a path to get to the Virgin Conditioning data files 
pathvirgin <- "data/female_conditioning/virgin"

## This creates a function ## 
# Reads into the path where the Virgin - Conditioning data files are 
# Creates a pattern to exclude data files that are 4:1 and 1:4 in a four choice assay 
# Pivots a data frame, to include fly numbers and diet, and excludes na where there would not be numbers in the data frame


############################################################################################################-
read_raw_virgin <-function(path = pathvirgin, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathvirgin,
                              pattern = "rawresults", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
############################################################################################################-


## read_raw is the function created, and path shows the path made, so the list of files
read_raw_virgin(pathvirgin) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin <- pathvirgin %>% 
  map_df(~read_raw_virgin(.x)) #.x is a string or NULL - only applies to dfr apparently

# Mutating a variable for block, using the 'id' of the different datasets
df_virgin <- df_virgin %>% 
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    str_detect(id, "b3") ~ "three",
    str_detect(id, "b4") ~ "four"
  ))


## This separatess diet into ratio and condition as these are currently joined 
df2_virgin <- df_virgin %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>% # separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition, block) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

# The data frame 
df2_virgin 








###################################-
#### OvoD1 FEMALE CONDITIONING ----
####################################-

pathovod1 <- "data/female_conditioning/ovod1"

## This creates  function
## Path is interchangeable with path 2 
read_raw_ovod1 <-function(path = pathovod1, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathovod1,
                              pattern = "rawresults", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovod1(pathovod1) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovod1 <- pathovod1 %>% 
  map_df(~read_raw_ovod1(.x)) #.x is a string or NULL - only applies to dfr apparently

# Adding block variables
df_ovod1 <- df_ovod1 %>% 
  mutate( block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two"
  ))

# uses what was generated with "df"
df2_ovod1 <- df_ovod1 %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition, block) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")



















########################## Data Analysis Part 1 (4:1 + 1:4) !!!! ############################

##########--
## MALE ####
##########--


## MODELS 

# MODEL 1
# Binomial GLM
## A binomial model, not considering other "random" factors
## cbind() is used - the response variables are Conditioned and Unconditioned
glm.bin.m <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_male)

# ASSUMPTION CHECKING:
## Checking Residuals
glm.bin.m.residuals <- residuals(glm.bin.m, type = "pearson")

# pearson residual check
plot(glm.bin.m.residuals ~ fitted(glm.bin.m), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")

## MODEL CHECK for overdispersion
summary(glm.bin.m) # overdispersion




## MODEL 2 
# Binomial GLMM
# trying a mixed model, considers other "random" factors
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) 
                    ~ ratio * block
                    + (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)


## Assumption checks 

# Performance check of data using "easystats":
performance::check_model(glmm.bin.m, check = c("homogeneity")) #?? 

## DHARMa performance checks
testDispersion(glmm.bin.m) 


# easystats checks 
check_overdispersion(glmm.bin.m)
# No overdispersion


check_zeroinflation(glmm.bin.m)
# cannot check zero inflation 


## zero inflation test 
fittedModel <- glmmTMB(cbind(Conditioned, Unconditioned)   ~ ratio * block + (1|block/plate) + (1|block/observation), ziformula = ~1 , family = "binomial", data = df2_virgin)
summary(fittedModel)
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") 
# No zero inflation? 


## zero inflation test 
fittedModel <- glmmTMB(cbind(Conditioned, Unconditioned)   ~ ratio * block + (1|block/plate) + (1|block/observation), ziformula = ~1 , family = "binomial", data = df2_male)
summary(fittedModel)
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") 
# No zero inflation? 



# DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.m, plot = T)
# Model looks pretty good





## AIC Check of models 
AIC(glm.bin.m, glmm.bin.m)
## Binomial GLMM has a slighty lower AIC

# Using MODEL 2 (Binomial GLMM) for analysis? 
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) 
                    ~ ratio * block
                    + (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)


# two-way interaction test
drop1(glmm.bin.m, test = "Chisq")
# No two-way interaction

# MODEL 2.1 
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                        ratio + block +
                        (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)


## Looking at the results of the model 
summary(glmm.bin.m.2) 








###################--
## VIRGIN FEMALE ####
###################--

## MODEL 1 
## A binomial model, not considering other "random" factors
glm.bin.vf <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glm.bin.vf , check = c("homogeneity")) 

# Assumption checking 
summary(glm.bin.vf) ## There is overdispersion





## MODEL 2 
# Mixed model, considers other "random" factors
glmm.bin.vf  <- glmer(cbind(Conditioned, Unconditioned)
                      ~ ratio * block + (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glmm.bin.vf, check = c("homogeneity")) # what is going on? looks ok 

# DHARMa checks 
simulationOutput_glmm.bin.vf <- simulateResiduals(fittedModel = glmm.bin.vf, plot = T)
# Model looks pretty good

# easystats checks
check_overdispersion(glmm.bin.vf)
# No overdispersion detected 


check_zeroinflation(glmm.bin.vf)
# cannot check... 


## Doing AIC check 
AIC(glm.bin.vf,glmm.bin.vf) 
# The Binomial GLMM has slightly higher AIC, choosing this  



## Using Binomial GLMM for analysis
glmm.bin.vf  <- glmer(cbind(Conditioned, Unconditioned) ~ 
                        ratio * block +
                        (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)

# Looking for two-way interaction
drop1(glmm.bin.vf, test = "Chisq") # No interaction effect between ratio and block
# No two way interaction

# Model 2.1 - where block has been removed...
glmm.bin.vf.2 <- glmer(cbind(Conditioned, Unconditioned) 
                       ~ ratio + block +  (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)


# Finding results from the model
summary(glmm.bin.vf.2) 














##################--
## OVOD1 FEMALE ####
##################--

## Model 1- Trying a binomial model
glm.bin.of <- glm(cbind(Conditioned, Unconditioned) ~
                    ratio * block , family = binomial, data = df2_ovod1)

# Assumption checks for binomial model 
summary(glm.bin.of) # There is overdispersion? 







## Model 2 
# Mixed model, considers other "random" factors
glmm.bin.of <- glmer(cbind(Conditioned, Unconditioned) 
                     ~ ratio * block  + (1|block/plate) + (1|block/observation), family = binomial, data = df2_ovod1)


# Assumption checking 

# easystats
performance::check_model(glmm.bin.of, check = c("homogeneity")) # I think looks okay, bit slopey down 

# DHARMa checks 
simulationOutput_glmm.bin.of <- simulateResiduals(fittedModel = glmm.bin.of, plot = T)
# Assumptions look pretty good


## zero inflation test 
fittedModel <- glmmTMB(cbind(Conditioned, Unconditioned)   ~ ratio * block + (1|block/plate) + (1|block/observation), ziformula = ~1 , family = "binomial", data = df2_ovod1)
summary(fittedModel)
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") 
# No zero inflation? 



check_overdispersion(glmm.bin.of)
# No overdispersion detected

## Doing AIC checks 
AIC(glm.bin.of, glmm.bin.of)
## Mixed model has lower AIC, and it considers random effects, so stucking with this

# Using this model
glmm.bin.of <- glmer(cbind(Conditioned, Unconditioned) ~
                       ratio * block  + (1|block/plate) + (1|block/observation), family = binomial, data = df2_ovod1)


# Looking at the significance of block
drop1(glmm.bin.of, test = "Chisq") 
# block is significant, keep in the model


## Analysis of glmm.bin.of
summary(glmm.bin.of)





