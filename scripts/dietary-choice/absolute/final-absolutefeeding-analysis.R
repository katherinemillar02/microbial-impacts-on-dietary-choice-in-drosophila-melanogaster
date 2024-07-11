



#### This script will read the data in for the appropriate conditioning, it then shows the chosen model
### BUT it doesn't show how it got to that point, like the other models tested... 




##### Packages 📦📦📦📦 ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
  ################## 




################## CONDITIONING TREATMENT: MALE ##################
 
################## DIETARY TREATMENT: ABSOLUTE ##################

## Using read excel to upload the data (block one and block two)
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_b2.xlsx")

# Mutating an additional variable for "block" 
fourone_onefour_male_b1 <- fourone_onefour_male_b1  %>% mutate(block = "one")
fourone_onefour_male_b2 <- fourone_onefour_male_b2  %>% mutate(block = "two")

# Using rbind() to bind the block 1 and block 2 data frames into one data frame.
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)

## Using pivot longer to add additional variables to the dataframe, and change variable names 
fourone_onefour_male_long <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  mutate(fly_numbers = as.integer(fly_numbers))  


## Splitting "diet" into "ratio" and "condition"
fourone_onefour_male_long_split <- fourone_onefour_male_long %>% 
  separate(diet, into = c ("ratio", "condition"), sep = " ")

#### I now show the chosen model, and test for interaction effects with the chosen model. 
#### For a previous demonstration on how I got to choosing this model, please see preliminary script. 

        #### The chosen model is a: Poisson GLMM   #### 

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

#### The mixed model shows there is both overdispersion and zero inflation
#### But the AIC showed it to be a good model compared to the other models. 



#### Changing what is in the intercept 

combined_m_split$ratio <- as.factor(combined_m_split$ratio)
combined_m_split$ratio <- relevel(combined_m_split$ratio, ref = "4:1")

combined_m_split$block <- as.factor(combined_m_split$block)
combined_m_split$block <- relevel(combined_m_split$block, ref = "one")

#### The chosen model (for reference): 


#### Testing for interaction effects with the new model 
#### Testing for 3-way interaction effects, 
#### As I have used "*", this will also test for two-way interaction effects.
glmm.m.4choice.2 <- glmmTMB(fly_numbers ~ 
                              
                              ratio * condition * block 
                            
                            + (1 | block / plate) + (1 |  block / observation), 
                            
                            family = poisson, data = combined_m_split)


## This will show results for the 3-way interaction
drop1(glmm.m.4choice.2, test = "Chisq")
   ## No 3-way interaction found, dropping from the model. 



## Testing 2-way interactions
glmm.m.4choice.3 <- glmmTMB(fly_numbers ~ 
                              ratio + condition + block 
                            + ratio : block 
                            + condition : block  
                            + ratio:condition + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m_split)




## Will display the 3 2-way interactions tested: 
drop1(glmm.m.4choice.3, test = "Chisq") 
    ## An interaction between condition and block found? the others can be dropped




## The final model 
glmm.m.4choice.4 <- glmmTMB(fly_numbers ~  
                              ratio + condition * block + (1 | block / plate) + (1 | observation / plate), family = poisson, data = combined_m_split)


## Using drop1, just to show the interaction effect again. 
drop1(glmm.m.4choice.4.2, test = "Chisq")




#### ASSUMPTION CHECKS WITH THE FINAL MODEL - to confirm it is still a "good" fit.


#### Using the DHARMa package for assumption checking. 
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


######## ANALYSIS OF RESULTS ######## 
summary(glmm.m.4choice.4.2)





















## VIRGIN FEMALE ####

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

combined_vf_split$ratio <- as.factor(combined_vf_split$ratio)
combined_vf_split$ratio <- relevel(combined_vf_split$ratio, ref = "4:1")


combined_vf_split$block <- as.factor(combined_vf_split$block)
combined_vf_split$block <- relevel(combined_vf_split$block, ref = "one")

# Final model - only with a interaction effect of condition and block... 
glm.nb.vf.4choice.4  <- glm.nb(fly_numbers ~  ratio + condition * block  , data = combined_vf_split)


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

combined_of_split$ratio <- as.factor(combined_of_split$ratio)
combined_of_split$ratio <- relevel(combined_of_split$ratio, ref = "4:1")

combined_of_split$block <- as.factor(combined_of_split$block)
combined_of_split$block <- relevel(combined_of_split$block, ref = "one")


## Final model?
zi.p.of.4choice.4 <- zeroinfl(fly_numbers ~  
                                
                                ratio : condition  
                              + condition : block

                              + ratio + condition + block , dist = "negbin", link = "logit", data = combined_of_split)


## Assumption checks for this model 
## Can't do assumption checks?? 


## Testing for remaining interaction effects 
drop1(zi.p.of.4choice.4, test = "Chisq") 

## Using model for analysis
summary(zi.p.of.4choice.4)

## Tukey test pairwise 
emmeans::emmeans(zi.p.of.4choice.4, pairwise ~ ratio + condition )













