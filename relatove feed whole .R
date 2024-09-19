

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
 
 
 
 # Comparing models.. 
 AIC(glm.nb.m.4choice, glmm.m.4choice)
 
 
 
 
