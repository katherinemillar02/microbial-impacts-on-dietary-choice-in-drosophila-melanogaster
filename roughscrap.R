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





glmm.m.4choice <- glmmTMB(fly_numbers 
                          
                          ~ ratio * condition * block, 
                          
    
                          
                          family = poisson, data = all)



simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice, plot = T)

## this works - with the random effects removed
















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

simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice, plot = T)


view(all)

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

glmm.m.4choice <- glmmTMB(
  fly_numbers ~ data:ratio:condition +
    block:data:ratio +
    data:ratio +
    ratio:condition +
    ratio:block +
    condition:block +
    data:condition +
    data:block,

  family = poisson,
  data = all)


 ## only shows 3? 
summary(glmm.m.4choice)

simulationOutput <- simulateResiduals(fittedModel = glmm.m.4choice, plot = T)





#### Poisson GLMM #### 
glmm.p.pupae <- glmmTMB(time_hours ~ treatment  + (1| vial), family = poisson, 
                        data = pupae_fitness_MFE_2)



#### ASSUMPTION CHECKS:  
# DHARMa checks 
plot(simulateResiduals(glmm.p.pupae)) 
## Does not look great
# DHARMa assumptions shows this model is poor for both qq and homogeneity 
# tests are significant for both 


## Performance checks - easystats 

# Looking for zeroinflation
check_zeroinflation(glmm.p.pupae) 
## There NO is zero inflation, indicating issues may be coming from overdispersion 

# Looking for overdispersion 
check_overdispersion(glmm.p.pupae) 
## There is overdispersion, indicating a negative binomial model might be good to test 






# Model 2
#### Negative Binomial GLM ####
glm.nb_pupae <- glm.nb(time_hours ~ 
                         treatment   
                       
                       , data = pupae_fitness_MFE_2)



#### ASSUMPTION CHECKS 
# DHARMa checks 
plot(simulateResiduals(glm.nb_pupae)) 
## qq looks a lot better, BUT there are still significant P values

## Performance checks
check_zeroinflation(glm.nb_pupae)
## No zeroinflation 
check_overdispersion(glm.nb_pupae) 
## No overdispersion 




## In this situation, I do not know what to do? 




## Comparing models:
AIC(glmm.p.pupae,glm.nb_pupae)
# Negative Binomial GLM is lower, but only slightly
# It is still not a great model... need to research into what models could be better











