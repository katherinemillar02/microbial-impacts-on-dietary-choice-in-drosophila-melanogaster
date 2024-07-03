
#### This script will read the data in for the appropriate conditioning, it then shows the chosen model
### doesn't show how it got to that point, 

# Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
##################---




## MALE ####
 
## MALE FEEDING 

#### UPLOADING AND BINDING THE CORRECT DATA for male 
# 4:1 + 1:4 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_t2b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_t2b2.xlsx")

# mutating a variable for block 
fourone_onefour_male_b1 <- fourone_onefour_male_b1  %>% mutate(block = "one")
fourone_onefour_male_b2 <- fourone_onefour_male_b2  %>% mutate(block = "two")

# Binding the data
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)


# 4:1 and 1:4 - making the data long 
combined_m <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  mutate(fly_numbers = as.integer(fly_numbers))  



## Splitting diet into ratio and condition
combined_m_split <- combined_m %>% 
  separate(diet, into = c ("ratio", "condition"), sep = " ")


## Testing for interaction effects with the new model 
# Model 1 - glmm.m.4choice.2
glmm.m.4choice.2 <- glmmTMB(fly_numbers ~ ratio * condition * block  + (1 | factor(block) / plate) + (1 |  factor(block) / observation), family = poisson, data = combined_m_split)

#### JUST IN CASE I AM REQUIRED TO DO THIS, testing different models, with the new model design 

## Assumption checks for glmm.m.4choice.2
testDispersion(glmm.m.4choice.2)
## Looks underdispersed 


simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice.2, plot = T)

performance::check_model(glmm.m.4choice.2, check = c("qq"))
## Does not show qq results 

performance::check_model(glmm.m.4choice.2)
## Not sure what I am looking for, but I think the assumptions look ok? 

check_overdispersion(glmm.m.4choice.2)
# Overdispersion detected

check_zeroinflation(glmm.m.4choice.2)
# Probable zero inflation 

### The mixed model shows there is both overdispersion and zero inflation. 


## Choosing this model 
glmm.m.4choice.2 <- glmmTMB(fly_numbers ~ ratio * condition * block + (1 | factor(block) / plate) + (1 | factor(block)/ observation), family = poisson, data = combined_m_split)


## This will show results for the 3-way interaction
drop1(glmm.m.4choice.2, test = "Chisq")

## This shows everything
summary(glmm.m.4choice.2)

## Results show there is no 3-way interaction, so this can be removed
## Testing 2-way interactions
glmm.m.4choice.3 <- glmmTMB(fly_numbers ~ 
                              ratio + condition + block 
                            + ratio : block 
                            + condition : block  
                            + ratio:condition + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m_split)

drop1(glmm.m.4choice.3, test = "Chisq") ## An interaction between condition and block found? the others can be dropped

## Newest model - final model 
## One version of it
glmm.m.4choice.4 <- glmmTMB(fly_numbers ~  
                              ratio + condition * block + (1 | block / plate) + (1 | observation / plate), family = poisson, data = combined_m_split)


## Another version of it 
glmm.m.4choice.4.2 <- glmmTMB(fly_numbers ~  
                               condition : block +
                                condition + block + ratio + (1 | block / plate) + (1 | block / observation ), family = poisson, data = combined_m_split)


drop1(glmm.m.4choice.4.2, test = "Chisq")
#### Assumption checks of the final chosen model (with diet split)... 

testDispersion(glmm.m.4choice.4)
## Is it underdispersed?

simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice.4, plot = T)
## Not sure how to interpret this? I think it looks ok though?

residuals(simulationOutput)

residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7)) 

performance::check_model(glmm.m.4choice.4)
## Does this look ok? 

check_zeroinflation(glmm.m.4choice.4) 
# There is zero inflation

check_overdispersion(glmm.m.4choice.4) 
# Here it says there is overdispersion, even though I thought DHARMa showed undispersion... 


performance::check_model(glmm.m.4choice.4, check = c("qq"))
## It does not show the qq - why?


## Analysis of the results
summary(glmm.m.4choice.4.2)

## Tukey test pairwise 
emmeans::emmeans(glmm.m.4choice.4, pairwise ~  ratio + condition)


summaey()

## VIRGIN FEMALE 

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
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  mutate(fly_numbers = as.integer(fly_numbers))


combined_vf_split <- combined_vf %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")

## The new model with diet split

## First testing this model
glm.nb.vf.4choice.2  <- glm.nb(fly_numbers ~ ratio * condition * block + (1|  factor(block)/ plate) + (1|  factor(block)/ observation), data = combined_vf_split)


## Assumption checking 

## Using DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.nb.vf.4choice.2, plot = T)
 ## I think this is a good model 

check_zeroinflation(glm.nb.vf.4choice.2)
  ## No zero-inflation 

check_overdispersion(glm.nb.vf.4choice.2)
  ## No overdispersion 

## easystats checks 
performance::check_model(glm.nb.vf.4choice.2)
  ## I think these checks are ok



## Trying with multiple, 3-way interactions
glm.nb.vf.4choice.2  <- glm.nb(fly_numbers ~ ratio * condition * block + (1|factor(block)/plate) + (1|factor(block)/observation), data = combined_vf_split)

## Testing for interaction
drop1(glm.nb.vf.4choice.2, test = "Chisq") 
## No interaction effect, 3-way interaction can be dropped from the model. 

## Tests two-way interactions 
glm.nb.vf.4choice.3  <- glm.nb(fly_numbers ~  ratio * condition + ratio * block + condition * block + (1|factor(block)/plate) + (1|factor(block)/observation), data = combined_vf_split)

## Finds 2-way interaction effects 
drop1(glm.nb.vf.4choice.3, test = "Chisq")  
 ## 2- way interaction effect between condition and block found, 
# here, when I do ":" it finds an interaction effect 
summary(glm.nb.vf.4choice.3)


# Final model - only with a interaction effect of condition and block... 
glm.nb.vf.4choice.4  <- glm.nb(fly_numbers ~  ratio + condition * block + (1|factor(block)/plate) + (1|factor(block)/observation), data = combined_vf_split)


## Assumption checks with the chosen model

## DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.vf.4choice.4, plot = T)
  ## Model still seems an ok fit

check_overdispersion(glm.nb.vf.4choice.4)
  # Still no overdispersion detected

check_zeroinflation(glm.nb.vf.4choice.4)
  # Still no zero-inflation detected 

performance::check_model(glm.nb.vf.4choice.4)
 ## I think assumptions are ok

## Using model for analysis 

## To show interaction effects
drop1(glm.nb.vf.4choice.4, test = "Chisq")  


## Using the final model for analysis 
summary(glm.nb.vf.4choice.4)

## Two-way tukey test 
emmeans::emmeans(glm.nb_vf_2, pairwise ~ ratio + condition)

## Finding response variable for written analysis 
emmeans::emmeans(glm.nb_vf_2, ~ diet, type = "response")
# conditioning in 1:4 not significant. 




# OVOD1 (EGGLESS) FEMALE ####
# Absolute Assay Analysis #


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
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  mutate(fly_numbers = as.integer(fly_numbers))


combined_of_split <- combined_of %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")



## New model with all interactions
zi.p.of.4choice.2 <- zeroinfl(fly_numbers ~ ratio * condition * block , dist = "negbin", link = "logit", data = combined_of_split)


## Assumption checks for this model 
  ## Can't do assumption checks?? 


# Testing for interaction effect
drop1(zi.p.of.4choice.2, test = "Chisq") ## No 3-way interaction effect 

# New model without a 3-way interaction
zi.p.of.4choice.3 <- zeroinfl(fly_numbers ~  ratio * condition + ratio * block + condition * block, dist = "negbin", link = "logit", data = combined_of_split)

## Testing for 2-way interaction effects
drop1(zi.p.of.4choice.3, test = "Chisq") 
## Interaction effect found between condition and block, and ratio and condition 

## Final model?
zi.p.of.4choice.4 <- zeroinfl(fly_numbers ~  ratio * condition  + condition * block , dist = "negbin", link = "logit", data = combined_of_split)


## Assumption checks for this model 
## Can't do assumption checks?? 


## Testing for remaining interaction effects 
drop1(zi.p.of.4choice.4, test = "Chisq") 

## Using model for analysis
summary(zi.p.of.4choice.4)

## Tukey test pairwise 
emmeans::emmeans(zi.p.of.4choice.4, pairwise ~ ratio + condition )













