

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
comb_v_egg_glm.p.2 <- glm(egg_numbers ~ ratio * condition * block, family = poisson,  combined_ovi_v_split)

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = comb_v_egg_glm.p.2, plot = T)
# Quite a bad model, all tests are significant 

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(comb_v_egg_glm.p.2)
# Overdispersion detected

# Testing for zeroinflation
check_zeroinflation(comb_v_egg_glm.p.2)
# There is an underfitting of zeros




#### Poisson GLMM ####
glm_mm_v_egg.2 <- glmmTMB(egg_numbers
                          ~ ratio * condition * block 
                          + (1| block / plate) , family = poisson, data = combined_ovi_v_split)


# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm_mm_v_egg.2, plot = T)
# A bit better, but still significant tests

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm_mm_v_egg.2)
# Overdispersion detected

# Testing for zeroinflation
check_zeroinflation(glm_mm_v_egg.2)
# There is an underfitting of zeros





#### Negative Binomial GLM ####
glm.nb_v_comb_egg.2 <- glm.nb(egg_numbers
                              ~ ratio * condition * block, 
                              data =  combined_ovi_v_split)



# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.nb_v_comb_egg.2, plot = T)
# A LOT better,  no significant tests

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm.nb_v_comb_egg.2)
# Overdispersion NOT detected

# Testing for zeroinflation
check_zeroinflation(glm.nb_v_comb_egg.2)
# There is an underfitting of zeros - still zeroinflation 


#### Zero inflation tests! ####

# Zero inflated poisson ####
glm.zi.p.MFE.flies <- glmmTMB(
  egg_numbers   ~ ratio * condition * block + (1| block / plate),  
  ziformula =  ~ ratio * condition * block,               
  family = poisson(),                          
  data = combined_ovi_v_split
)


# Assumption checks 

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.zi.p.MFE.flies, plot = T)
# Some significant tests

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm.zi.p.MFE.flies)
# Overdispersion IS detected

# Testing for zeroinflation
check_zeroinflation(glm.zi.p.MFE.flies)
# There is no longer zeroinflation 


# Because there is overdispersion, trying zeroinflation witn NegBin



#### Zero Inflated Negative Binomial ####
glm.zi.nb.MFE.flies <- glmmTMB(
  egg_numbers   ~ ratio * condition * block + (1| block / plate),  
  ziformula =  ~ ratio * condition * block,               
  family = nbinom2(),                          
  data = combined_ovi_v_split
)

# Assumption checks 

# DHARMa checks
simulationOutput <- simulateResiduals(fittedModel = glm.zi.nb.MFE.flies, plot = T)
# Model is looking pretty cracking 

# easystats / performance checks 

# Testing for overdispersion
check_overdispersion(glm.zi.nb.MFE.flies)
# Overdispersion IS NOT detected

# Testing for zeroinflation
check_zeroinflation(glm.zi.nb.MFE.flies)
# There is no longer zeroinflation 

# This model ^^ seems best... 
#### Comparing models just to check... 

AIC(comb_v_egg_glm.p.2, glm_mm_v_egg.2, glm.nb_v_comb_egg.2, glm.zi.p.MFE.flies, glm.zi.nb.MFE.flies)
 # NegBin without zeroinflation has better assumptions but there is zeroinflation so still choosing Zero-Inflated Negative Binomial 




#### Chosen model: Zero-Inflated Negative Binomial... 

# Testing for a 3-way interaction effect
 # Note: this model only seems to work with the random effect how it is and not with block
glm.zi.nb.MFE.flies <- glmmTMB(
  egg_numbers   ~ ratio * condition * block   + (1|  plate),  
  ziformula =  ~ ratio * condition * block,               
  family = nbinom2(),                          
  data = combined_ovi_v_split
)

# Testing for significance in the 3-way interaction effect
drop1(glm.zi.nb.MFE.flies, test = "Chisq")
 # Significant 3-way interaction


#### 2. Data analysis for write-up ####

# Basic analysis 
summary(glm.zi.nb.MFE.flies)


# Confidence intervals 
exp(confint(glm.zi.nb.MFE.flies))


# Real values for write-up
emmeans::emmeans(glm.zi.nb.MFE.flies, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.zi.nb.MFE.flies, CSS = list(css.table = '+font-family: Arial;'))






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
combined_ovi_v_split <- combined_ovi_v %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")



#### 1. Preliminary Data Analysis ####

#### Poisson GLM ####
comb_v_egg_glm.p.2 <- glm(egg_numbers ~ ratio * condition * block, family = poisson,  combined_ovi_v_split)

#### Negative Binomial GLM ####
glm.nb_v_comb_egg.2 <- glm.nb(egg_numbers ~ ratio * condition * block, data =  combined_ovi_v_split)

#### Poisson GLMM ####
glm_mm_v_egg.2 <- glmmTMB(egg_numbers ~ ratio * condition * block + (1|factor(block)/plate) , family = poisson, data = combined_ovi_v_split)

#### Zero-Inflated Poisson ####
zif.p_v_egg.2 <- zeroinfl(egg_numbers ~ ratio * condition * block | ratio * condition * block, dist = "poisson", link = "logit", data = combined_ovi_v_split)



