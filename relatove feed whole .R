

                                                  #### Chapter 2  #### 
 
 
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
 library(sjPlot)
 ################## --
 
 
 #### Male ####
 
 #### Reading, binding and cleaning the data ####
 
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
 
 
 
 
 
 #### 1. Preliminary Data Analysis ####
 
 # Model 1 - glmm.m.4choice.2
 #### Poisson GLMM ####
 glmm.m.4choice <- glmmTMB(fly_numbers 
                             
                             ~ ratio * condition * block 
                             
                             + (1 | block / plate) + (1 | observation), 
                             
                             family = poisson, data = combined_m_split)
   
 
 
 ## Assumption checks: 
 
 # DHARMa 
 simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice, plot = T)
 ## Assumptions of the model look pretty good
 
 
 # easystats checks 
 check_overdispersion(glmm.m.4choice)
  # Overdispersion is detected
 
 check_zeroinflation(glmm.m.4choice)
 # Zeroinflation detected 
 
 
 
 #### Model 2
 #### Negative Binomial GLM ####
 glm.nb.m.4choice <- glm.nb(fly_numbers ~ 
                              ratio * condition * block,
                            data = combined_m_split)
 
 ## Assumption checks: 
 
 # DHARMa 
 simulationOutput <- simulateResiduals(fittedModel = glm.nb.m.4choice, plot = T)
 ## Assumptions of the model look pretty good but there is a significant test with homogeneity
 
 
 # easystats checks 
 check_overdispersion(glm.nb.m.4choice)
 # NO overdispersion is detected
 
 check_zeroinflation(glmm.m.4choice)
 # Zeroinflation STILL detected 
 
 
 
 # Comparing models... 
 AIC(glm.nb.m.4choice, glmm.m.4choice)
# The mixed model is better, despite there being overdispersion 
 
 
## Using this
  # Testing for a 3-way interaction
glmm.m.4choice <- glmmTMB(fly_numbers 
                           
                           ~ ratio * condition * block 
                           
                           + (1 | block / plate) + (1 | observation), 
                           
                           family = poisson, data = combined_m_split)
 

# Significance of 3-way interaction
drop1(glmm.m.4choice, test = "Chisq")
 # No 3-way interaction 


# Testing for a 2-way interaction
glmm.m.4choice.2 <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition +
                            condition * block +
                            ratio * block
                          
                          + (1 | block / plate) + (1 | observation), 
                          
                          family = poisson, data = combined_m_split)

# Significance of 2-way interaction
drop1(glmm.m.4choice.2, test = "Chisq")
 # Significant interaction between condition and block



#### Final model 
glmm.m.4choice.3 <- glmmTMB(fly_numbers 
                            
                            ~ 
                              condition * block + ratio
                             
                            
                            + (1 | block / plate) + (1 | observation), 
                            
                            family = poisson, data = combined_m_split)


#### Data Analysis for write-up ####

# Basic analysis 
summary(glmm.m.4choice.3)

# Confidence intervals 
exp(confint(glmm.m.4choice.3))


# Real values for write-up
emmeans::emmeans(glmm.m.4choice.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glmm.m.4choice.3, CSS = list(css.table = '+font-family: Arial;')) 











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
glm.nb.m.4choice <- glm.nb(fly_numbers ~ 
                             ratio * condition * block,
                           data = combined_vf_split)



## Assumption checks: 

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glm.nb.m.4choice, plot = T)
## Assumptions of the model look pretty good 


# easystats checks 
check_overdispersion(glm.nb.m.4choice)
# Overdispersion is NO LONGER detected

check_zeroinflation(glm.nb.m.4choice)
# Zeroinflation IS STILL NOT detected 



# Going to chooose NegBin but doing AIC just in case 
AIC(glmm.vf.4choice, glm.nb.m.4choice)
 # NegBin AIC only slightly lower 






## Choosing this model: 
# Testing for a 3-way significant effect 
glm.nb.m.4choice <- glm.nb(fly_numbers ~ 
                             ratio * condition * block,
                           data = combined_vf_split)


# Significance of 3-way interaction
drop1(glm.nb.m.4choice, test = "Chisq")
 # No significant interaction


## Testing for 2-way interaction effects
glm.nb.m.4choice.2 <- glm.nb(fly_numbers ~ 
                               ratio * condition +
                               condition * block +
                               ratio * block,
                           data = combined_vf_split)


# Looking for significance in the 2-way interaction effecrs
drop1(glm.nb.m.4choice.2, test = "Chisq")
 # Only a significant interaction between condition and block 


# Final model 
glm.nb.m.4choice.3 <- glm.nb(fly_numbers ~ 
                              
                               condition * block + ratio,
                               
                             data = combined_vf_split)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glm.nb.m.4choice.3)

# Confidence intervals 
exp(confint(glm.nb.m.4choice.3))


# Real values for write-up
emmeans::emmeans(glm.nb.m.4choice.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.nb.m.4choice.3, CSS = list(css.table = '+font-family: Arial;')) 








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











