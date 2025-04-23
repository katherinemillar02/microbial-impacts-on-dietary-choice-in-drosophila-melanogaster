## scraps 
# putting the different conditioning effects into a model 


################################## Packages and Data 📦📦📦📦 ##################################
source("packages.R")
# a script containing all packages needed for data analysis 
source("scripts/dietary-choice/dietarychoice.dataread.R") 
# a script containing all data needed for the dietary choice analysis 
################################################################################################


glmm.m.4choice <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition * block 
                          
                          + (1 | block / plate) + (1 | observation), 
                          
                          family = poisson, data = combined_m_split)


combined_m_split <- combined_m_split %>% 
  mutate(data = "male")

combined_vf_split <- combined_vf_split %>% 
  mutate(data = "v_female")

combined_of_split <- combined_of_split %>% 
  mutate(data = "o_female")



all <- rbind(combined_m_split, combined_of_split, combined_vf_split)
all


glmm.m.4choice <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition * block * data
                          
                          + (1 | block / plate) + (1 | observation), 
                          
                          family = poisson, data = all)

summary(glmm.m.4choice)
drop1(glmm.m.4choice, test = "Chisq")


glmm.m.4choice <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition * block +
                            data * ratio * condition + 
                            block * data * ratio +
                            ratio * data * block 
                          
                          + (1 | block / plate) + (1 | observation), 
                          
                          family = poisson, data = all)

drop1(glmm.m.4choice, test = "Chisq")

glmm.m.4choice <- glmmTMB(fly_numbers 
                          
                          ~ data * ratio * condition + 
                            block * data * ratio +
                            ratio * data * block +
                            
                            data * ratio + 
                            ratio * condition + 
                            ratio * block +
                            
                            condition * block + 
                            data * condition +
                            
                            
                            data * block 
                            
                            
                          + (1 | block / plate) + (1 | observation), 
                          
                          family = poisson, data = all)

 ## only shows 3? 
summary(glmm.m.4choice)

simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice, plot = T)

