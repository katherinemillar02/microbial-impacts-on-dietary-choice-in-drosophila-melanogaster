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




################################################################################################################################################-- 



#### (4:1/1:4) ####

## For the 4:1/1:4 Assay only 


## MALE ####


#### UPLOADING AND BINDING THE CORRECT DATA
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
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")




## Data Analysis 

## Doing poisson to begin with 
glm.pois.m.4choice <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_m)

# Doing assumption checks before seeing if overdispersion can be checked
performance::check_model(glm.pois.m.4choice, check = c("qq")) # qq looks ok?
performance::check_model(glm.pois.m.4choice, check = c("outliers")) # weird 
performance::check_model(glm.pois.m.4choice, check = c("homogeneity")) # does this look okay maybe? 


# Checking for overdispersion
summary(glm.pois.m.4choice) # A bit overdispersed 

# Look for 0s 
check_zeroinflation(glm.pois.m.4)
# there is zero inflation 

# There is overdispersion, this could be independent of or because of the zero inflation
# So trying negative binomial family 
## Trying to look for significance of experiment with a negative binomial model




# Negative binomial model
glm.nb.m.4choice <- glm.nb(fly_numbers ~ diet * block, data = combined_m)

# assumption checks for negative binomial model
# easystats
performance::check_model(glm.nb.m.4choice, check = c("qq")) # looks the same as glm poisson. 
performance::check_model(glm.nb.m.4choice, check = c("outliers")) # weird 
performance::check_model(glm.nb.m.4choice, check = c("homogeneity")) # looks the same again 

# Checking for overdispersion 
summary(glm.nb.m.4choice)
   # No evidence of overdispersion but still highly skewed residuals, do random effects help?





    # Trying a mixed model with GLM and poisson 
glmm.m.4choice <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|block/observation), family = poisson, data = combined_m)

## Assumption checks
performance::check_model(glmm.m.4, check = c("qq")) # qq looks a lot better

## Looking for inflation of zeros
check_zeroinflation(glmm.m.4) # There is still zero inflation 

# using DHARMa to look at residuals of new model 
simulateResiduals(fittedModel = glmm.m.4, plot = T)

# Trying a zero inflated poisson model with poisson
zi.pois.m.4choice <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_m)

## Assumption checks 
sresid <- residuals(zi.pois.m.4, type = "pearson")
pred <- fitted(zi.pois.m.4)
hist(sresid)
plot(sresid ~  pred)


# Trying a zero inflated negative binomial model 
zi.nb.m.4choice <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_m)

 


# Comparing AIC of the different models 
AIC(glm.pois.m.4choice, glm.nb.mchoice, glmm.m.4choice, zi.pois.m.4choice, zi.nb.m.4choice)
# glm_mm_m has the lowest AIC, but AIC values close to each other are basically the same (like 5 values?)






# This model to be chosen:
glmm.m.4choice <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_m)

## Testing for an interaction effect 
drop1(glmm.m.4, test = "Chi") # Small interaction, but still exists 



# With the chosen model, splitting up "diet" into ratio and condition.



## Splitting up diet within the actual data frame
combined_m_split <- combined_m %>%
     separate(diet, into = c("ratio", "condition"), sep = " ")


## Testing for interaction effects with the new model 
# Model 1 - glmm.m.4choice.2
glmm.m.4choice.2 <- glmmTMB(fly_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m_split)

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
    

## First, trying a zero-inflation model

## zero inflation with poisson
zi.pois.m.4choice.2 <- zeroinfl(fly_numbers ~ratio * condition * block + ratio * condition + ratio * block + condition * block | ratio * condition * block + ratio * condition + ratio * block + condition * block, dist = "poisson", link = "logit", data = combined_m_split)

## Assumption checks??

## zero inflation with neg bin
zi.nb.m.4choice.2 <- zeroinfl(fly_numbers ~ratio * condition * block + ratio * condition + ratio * block + condition * block | ratio * condition * block + ratio * condition + ratio * block + condition * block, dist = "negbin", link = "logit", data = combined_m_split)

## Assumption checks??

## Negative Binomial GLM
glm.nb.m.4choice.2 <- glm.nb(fly_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block, data = combined_m_split)


## Assumption checks
simulationOutput <- simulateResiduals(fittedModel = glm.nb.m.4choice.2, plot = T)
  ## Not sure what the red represents 

check_zeroinflation(glm.nb.m.4choice.2) ## No zero inflation 

check_overdispersion(glm.nb.m.4choice.2) ## No overdispersion 

AIC(glmm.m.4choice.2, zi.pois.m.4choice.2, zi.nb.m.4choice.2, glm.nb.m.4choice.2)
  # GLMM has lowest AIC, BUT is overdispersed and has zero infaltion - choosing anyway... 







## Chosen Model... 
glmm.m.4choice.2 <- glmmTMB(fly_numbers ~ ratio * condition * block + (1 | factor(block) / plate) + (1 | factor(block)/observation), family = poisson, data = combined_m_split)


## This will show results for the 3-way interaction
drop1(glmm.m.4choice.2, test = "Chisq")

## This shows everything
summary(glmm.m.4choice.2)

## Results show there is no 3-way interaction, so this can be removed
## Testing 2-way interactions
glmm.m.4choice.3 <- glmmTMB(fly_numbers ~  ratio + condition + block + ratio : block + condition : block  + ratio:condition + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m_split)

drop1(glmm.m.4choice.3, test = "Chisq") ## An interaction between condition and block found? the others can be dropped

## Newest model - final model 
glmm.m.4choice.4 <- glmmTMB(fly_numbers ~  ratio + condition * block + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m_split)


#### Assumption checks of the chosen model (with diet split)... 

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
summary(glmm.m.4choice.4)

## Tukey test pairwise 
emmeans::emmeans(glmm.m.4choice.4, pairwise ~  ratio + condition)



















# VIRGIN FEMALE ####

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


#### DATA ANALYSIS 


# GLM with with poisson
glm.pois.vf.4choice <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_vf)


# Assumption checking
performance::check_model(glm.poisson.vf.4choice, check = c("qq")) # almost banana - not great 
performance::check_model(glm.poisson.vf.4choice, check = c("homogeneity")) # looks okay ? 

# looking for overdispersion in the model
summary(glm.poisson.vf.4choice) # shows slight overdispersion 

# overdispersion so checking for zero inflation
check_zeroinflation(glm.poisson.vf.4choice) # No zero inflation?? 


# Trying quasipoisson
glm.quasipoisson.vf.4choice <- glm(fly_numbers ~ diet * block, family = quasipoisson, data = combined_vf)

# Assumption checking
performance::check_model(glm.quasipoisson.vf.4choice, check = c("qq")) # almost banana - not great - looks same 
performance::check_model(glm.quasipoisson.vf.4choice, check = c("homogeneity")) # looks okay ? 




# Trying a negative binomial model to compare more models
glm.nb.vf.4choice <- glm.nb(fly_numbers ~ diet * block + (1|plate) + (1|observation), data = combined_vf)

## Assumption checks
performance::check_model(glm.nb.vf.4choice, check = c("qq")) # qq looks slighty better? still off 
performance::check_model(glm.nb.vf.4choice, check = c("homogeneity")) # maybe the same 

## Checking for overdispersion
summary(glm.nb.vf.4choice)  # a lot less overdispersion 



# Trying a generalised linear mixed model to compare more models
glmm.vf.4choice <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_vf)

## Assumption checks
performance::check_model(glmm.vf.4choice, check = c("qq")) # looks a lot better, slopes off at the end though


# Assumption checking with DHARMa
simulateResiduals(fittedModel = glmm.vf.4choice , plot = T)




## Checking the AIC of all the tested models 
AIC(glm.pois.vf.4choice, glm.quasipoisson.vf.4choice, glm.nb.vf.4choice, glmm.vf.4choice) 
    #### Negative Binomial GLM has lowest AIC (slightly), go with this for now as assumption checks were also ok? 


## Chosen model 
glm.nb.vf.4choice <- glm.nb(fly_numbers ~ diet * block + (1|plate) + (1|observation), data = combined_vf)


# Splitting diet up with chosen model

##  Using separate to split diet up into ratio and condition
combined_vf_split <- combined_vf %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


## The new model with diet split

## Trying with multiple, 3-way interactions
glm.nb.vf.4choice.2  <- glm.nb(fly_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block + (1|plate) + (1|observation), data = combined_vf_split)

## Testing for interaction
drop1(glm.nb.vf.4choice.2, test = "Chisq") 
   ## No interaction effect, 3-way interaction can be dropped from the model. 

## Tests two-way interactions 
glm.nb.vf.4choice.3  <- glm.nb(fly_numbers ~  ratio * condition + ratio * block + condition * block + (1|plate) + (1|observation), data = combined_vf_split)

## Finds interaction effects 
drop1(glm.nb.vf.4choice.3, test = "Chisq")  
     ## An interaction effect of condition and block found 

# Final model
glm.nb.vf.4choice.4  <- glm.nb(fly_numbers ~  ratio + condition * block + (1|plate) + (1|observation), data = combined_vf_split)

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
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



#### DATA ANALYSIS


# GLM with poisson 
glm.pois.of.4choice <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_of)

# Assumption checks of model
performance::check_model(glm.pois.of.4choice, check = c("qq")) # doesn't look great - banana 
performance::check_model(glm.pois.of.4choice, check = c("homogeneity")) # slopey 

# Checking for overdispersion
summary(glm.pois.of.4choice) # There is overdispersion 

# Overdispersion is found
# Checking for zero inflation
check_zeroinflation(glm.pois.of.4choice) # There is zero inflation 




# Doing a negative binomial as there is overdispersion
glm.nb.of.4choice <- glm.nb(fly_numbers ~ diet * block, data = combined_of)

# Checking the Negative Binomial GLM 
performance::check_model(glm.nb.of.4choice, check = c("qq")) # still very slopey 

# using DHARMa to check 
testDispersion(glm.nb.of.4choice) # this is fairly dispersed 

simulationOutput_of  <- simulateResiduals(fittedModel = glm.nb.of.4choice, plot = F)

residuals(simulationOutput_of)
plot(simulationOutput_of)
testZeroInflation(simulationOutput_of)


# Checking for overdispersion 
summary(glm.nb.of.4choice) # very little overdispersion 





# Trying a mixed GLMM, to consider random effects
glmm.of.4choice <- glmmTMB(fly_numbers ~ diet + block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_of)

## Checking for overdispersion 
check_overdispersion(glmm.of.4choice)
   ## Overdispersion detected 

## More assumption checks 
performance::check_model(glmm.of.4choice, check = c("qq")) 
    # Currently won't run, 21/06/24



# using DHARMa for assumption checks
testDispersion(glmm.of.4choice) # This is fairly overdispersed 

simulationOutput_glmm.of.4choice <- simulateResiduals(fittedModel = glmm.of.4choice, plot = F)

residuals(simulationOutput_glmm.of.4choice)
plot(simulationOutput_glmm.of.4choice)
testZeroInflation(simulationOutput_glmm.of.4choice)




# ZERO INFLATED MODELS - zeroinflation has been found, so trying zeroinflated models.

# Trying a zero inflated poisson model 
zi.p.of.4choice <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_of)


# trying a zero inflated - negative binomial model 
zi.nb.of.4choice <- zeroinfl(fly_numbers ~ diet + block , dist = "negbin", link = "logit", data = combined_of )


    #### For the zeroinflated models, I don't know how to do performnance checks. 



# Comparing all the models together 
AIC(glm.pois.of.4choice, glm.nb.of.4choice, glmm.of.4choice, zi.p.of.4choice, zi.nb.of.4choice)

  #### Choosing Zero Inflatted Poisson for now 


#### Splitting variables in the model up: 
combined_of_split <- combined_of_split %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


## New model with all interactions
zi.p.of.4choice.2 <- zeroinfl(fly_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block, dist = "negbin", link = "logit", data = combined_of )

# Testing for interaction effect
drop1(zi.p.of.4choice.2, test = "Chisq") ## No 3-way interaction effect 

# New model without a 3-way interaction
zi.p.of.4choice.3 <- zeroinfl(fly_numbers ~  ratio * condition + ratio * block + condition * block, dist = "negbin", link = "logit", data = combined_of )

## Testing for 2-way interaction effects
drop1(zi.p.of.4choice.3, test = "Chisq") 
 ## Interaction effect found between condition and block, and ratio and condition 

## Final model?
zi.p.of.4choice.4 <- zeroinfl(fly_numbers ~  ratio * condition  + condition * block , dist = "negbin", link = "logit", data = combined_of )

## Testing for remaining interaction effects 
drop1(zi.p.of.4choice.4, test = "Chisq") 

## Using model for analysis
summary(zi.p.of.4choice.4)

## Tukey test pairwise 
emmeans::emmeans(zi.p.of.4choice.4, pairwise ~ ratio + conditi