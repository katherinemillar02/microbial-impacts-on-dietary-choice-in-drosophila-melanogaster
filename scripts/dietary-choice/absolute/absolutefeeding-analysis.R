



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
library(emmeans)
  ################## --




################## CONDITIONING TREATMENT: MALE ##################
 
################## DIETARY TREATMENT: ABSOLUTE ##################--

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



## Splitting up diet within the actual data frame
combined_m_split <- fourone_onefour_male_long %>%
  separate(diet, into = c("ratio", "condition"), sep = " ")


#### I now show the chosen model, and test for interaction effects with the chosen model. 
#### For a previous demonstration on how I got to choosing this model, please see preliminary script. 

        #### The chosen model is a: Poisson GLMM   ####-- 
glmm.m.4choice.2 <- glmmTMB(fly_numbers ~ 
                              
                              ratio * condition * block 
                            
                            + (1 | block / plate) + (1 |  block / observation), 
                            
                            family = poisson, data = combined_m_split)

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

## Changing the intercept to ratio: 4:1 
combined_m_split$ratio <- as.factor(combined_m_split$ratio)
combined_m_split$ratio <- relevel(combined_m_split$ratio, ref = "4:1")

## Changing the block to: one. 
combined_m_split$block <- as.factor(combined_m_split$block)
combined_m_split$block <- relevel(combined_m_split$block, ref = "one")



#### The chosen model: glmm.m.4choice.2, Poisson GLMM

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
drop1(glmm.m.4choice.4, test = "Chisq")




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
summary(glmm.m.4choice.4)


## Changing the intercept to ratio: 4:1 
combined_m_split$ratio <- as.factor(combined_m_split$ratio)
combined_m_split$ratio <- relevel(combined_m_split$ratio, ref = "4:1")


## The final model 
glmm.m.4choice.4.1 <- glmmTMB(fly_numbers ~  
                              ratio + condition * block + (1 | block / plate) + (1 | observation / plate), family = poisson, data = combined_m_split)




glmm.m.4choice.4.11 <- glmmTMB(fly_numbers ~  
                                ratio * condition + (1 | block / plate) + (1 | observation / plate), family = poisson, data = combined_m_split)


summary(glmm.m.4choice.4.1)



emmeans(glmm.m.4choice.4.11, pairwise ~ ratio * condition)


exp(confint(glmm.m.4choice.4))


# proportional 
emmeans(glmm.m.4choice.4, ~ ratio + block, type = "response")

# table 
tab_model(glmm.m.4choice.4)









################## CONDITIONING TREATMENT: VIRGIN FEMALE ##################

################## DIETARY TREATMENT: ABSOLUTE ##################--

#### Using readxl() to upload the data files, virgin female has 4 blocks for feeding behaviour. 
fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx")
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")
fourone_onefour_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx")


## Mutating block variables to the different data frames.
fourone_onefour_virgin_b1 <- fourone_onefour_virgin_b1  %>% mutate(block = "one")
fourone_onefour_virgin_b2 <- fourone_onefour_virgin_b2  %>% mutate(block = "two")
fourone_onefour_virgin_b3 <- fourone_onefour_virgin_b3  %>% mutate(block = "three")
fourone_onefour_virgin_b4 <- fourone_onefour_virgin_b3  %>% mutate(block = "four")

# Binding the data using rbind(), (blocks one to four). 
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2, fourone_onefour_virgin_b3, fourone_onefour_virgin_b4)


# Using pivot_longer() to add variables to the data, and making fly numbers an integer.
combined_vf <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  mutate(fly_numbers = as.integer(fly_numbers))


# Splitting "diet" into "ratio" and "condition" 
combined_vf_split <- combined_vf %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")



#### Changing what the intercept is for ratio - 4:1 
combined_vf_split$ratio <- as.factor(combined_vf_split$ratio)
combined_vf_split$ratio <- relevel(combined_vf_split$ratio, ref = "4:1")

#### Changing what the intercept is for block - one 
combined_vf_split$block <- as.factor(combined_vf_split$block)
combined_vf_split$block <- relevel(combined_vf_split$block, ref = "one")

## The chosen model: glm.nb.vf.4choice.2, Negative Binomial GLM
## For demonstration of how I chose this model, please see a preliminary script. 
glm.nb.vf.4choice.2  <- glm.nb(fly_numbers ~
                                 ratio * condition * block 
                               + (1|  block / plate) + (1|  block / observation), data = combined_vf_split)


## Assumption checking with the chosen model, to demonstrate the goodness of fit. 
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




#### Using the chosen model:
#### Testing for interaction effects. 


## Trying for 3-way interaction effects, as I am using *, it will also test two-way interaction effects. 
glm.nb.vf.4choice.2  <- glm.nb(fly_numbers ~ 
                                 
                                 ratio * condition * block
                               
                              , data = combined_vf_split)

## Testing for two-way interaction effects
drop1(glm.nb.vf.4choice.2, test = "Chisq") 
   ## No interaction effect, 3-way interaction can be dropped from the model. 

## Tests two-way interactions, "*" can be used, as there is nothing lower than * two way interactions.  
glm.nb.vf.4choice.3  <- glm.nb(fly_numbers ~ 
                                ratio * condition 
                               + ratio * block 
                               + condition * block, data = combined_vf_split)

# ### As an example, this is the exact same as the one above, but I am using *.
# glm.nb.vf.4choice.32  <- glm.nb(fly_numbers ~ 
#                                  ratio : condition 
#                                + ratio : block 
#                                + condition : block +
#                                  ratio + condition + block , data = combined_vf_split)


## Finds 2-way interaction effects 
drop1(glm.nb.vf.4choice.3, test = "Chisq")  
 ## 2- way interaction effect between condition and block found, 




# Final model - Contains an interaction effect between condition and block,  ratio is added normally.
glm.nb.vf.4choice.4  <- glm.nb(fly_numbers ~ 
                                 
                                 condition * block + ratio   
                               
                               , data = combined_vf_split)

## To show interaction effects
drop1(glm.nb.vf.4choice.4, test = "Chisq")  




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




####  ANALYSIS OF RESULTS ####
## Using the final model for analysis 
summary(glm.nb.vf.4choice.4)



glm.nb.vf.4choice.41  <- glm.nb(fly_numbers ~ 
                                 
                                 condition * ratio   
                               
                               , data = combined_vf_split)
emmeans(glm.nb.vf.4choice.41, pairwise ~ ratio * condition)

coef <- coef(glm.nb.vf.4choice.4)
exp(coef)

exp(confint(glm.nb.vf.4choice.4))

# proportional 
emmeans(glm.nb.vf.4choice.4 ~ ratio, type = "response")

# table 
tab_model(glm.nb.vf.4choice.4)







#### CONDITIONING TREATMENT: OVOD1 FEMALE ####
#### ABSOLUTE ####--


## Using read_excel() to read upload the data files.
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")

# Mutating a block variable to the different data files. 
fourone_onefour_ovod1_b1 <- fourone_onefour_ovod1_b1  %>% mutate(block = "one")
fourone_onefour_ovod1_b2 <- fourone_onefour_ovod1_b2  %>% mutate(block = "two")

# Binding the block files (one and two) using rbind(). 
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)


## Cleaning the data, adding relevant variables, using pivot_longer()
combined_of <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  mutate(fly_numbers = as.integer(fly_numbers))





#### Splitting "diet" into "ratio" and "condition". 
combined_of_split <- combined_of %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")





#### The chosen model: Negative Binomial Zero Inflation: 
zi.nb.of.4choice.2 <- zeroinfl(fly_numbers ~
                                 
                                 ratio * condition * block 
                               
                               , dist = "negbin", link = "logit", data = combined_of_split)

## Assumption checks for this model - note: struggling to find zero-inflation assumption checks. 



#### ASSUMPTION CHECKS WOULD BE INSERTED HERE: 








####

## Changing what the intercept is - making ratio 4:1, instead of 1:4.
combined_of_split$ratio <- as.factor(combined_of_split$ratio)
combined_of_split$ratio <- relevel(combined_of_split$ratio, ref = "4:1")





#### Testing for a 3-way interaction effect: 

zi.nb.of.4choice.2 <- zeroinfl(fly_numbers ~
                                 
                                 ratio * condition * block 
                               
                               , dist = "negbin", link = "logit", data = combined_of_split)



# Testing for the 3-way interaction effects
drop1(zi.nb.of.4choice.2, test = "Chisq") 
   ## No 3-way interaction effect 



# New model with 2-way interaction effects only 
zi.nb.of.4choice.3 <- zeroinfl(fly_numbers ~  
                                 ratio * condition 
                               + ratio * block 
                               + condition * block, 
                               dist = "negbin", link = "logit", data = combined_of_split)



## Testing for 2-way interaction effects
drop1(zi.nb.of.4choice.3, test = "Chisq") 
## Interaction effect found between condition and block, and ratio and condition 



## Final model?
zi.nb.of.4choice.4 <- zeroinfl(fly_numbers ~  
                                
                                ratio : condition  
                              + condition : block

                              + ratio + condition + block , dist = "negbin", link = "logit", data = combined_of_split)


## Assumption checks for this model 


     ## Can't do assumption checks?? 


## Testing for remaining interaction effects 
drop1(zi.nb.of.4choice.4, test = "Chisq") 

## Using model for analysis
summary(zi.nb.of.4choice.4)



zi.nb.of.4choice.41 <- zeroinfl(fly_numbers ~  
                                 
                                 ratio * condition
                                + condition * block , dist = "negbin", link = "logit", data = combined_of_split)

emmeans(zi.nb.of.4choice.41, pairwise ~ ratio * condition)


 
exp(confint(zi.nb.of.4choice.4))

# proportional 
emmeans(zi.nb.of.4choice.4, ~ ratio, type = "response")

# table 
tab_model(zi.nb.of.4choice.4)












