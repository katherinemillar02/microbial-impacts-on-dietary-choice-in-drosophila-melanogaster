

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
########################


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
