

#############################################################################################
############## The Effects of Diet Conditioning on Female  Dietary Choice ################### 
############################### Edited by: Katie Millar, ####################################
##################### This script shows the final models and analysis for ###################
##################### the dietary choice assays set in a "relative" environment, ############
###################### where flies where given the choice of 4 different diets ##############
#############################################################################################

#### Packages and Data Read in 📦📦📦📦 #### 
library(ggpubr)
source("packages.R")
source("scripts/dietary-choice/dietarychoice.dataread.R")
#### 


                                  #### FEEDING BEHAVIOUR ANALYSIS ####


                                  #### Diets Conditioned by Males ####

# Testing for a 3-way interaction
m.glmm.p.4choice.feed.1 <- glmmTMB(fly_numbers 
                                   ~ ratio * condition * block 
                                   + (1 | block / plate) + (1 | observation), 
                                   family = poisson, 
                                   data = combined_m_split)

# Significance of 3-way interaction
drop1(m.glmm.p.4choice.feed.1, test = "Chisq")
# No 3-way interaction 

# Dropping the three way interaction and testing for 2-way interactions
m.glmm.p.4choice.feed.2 <- glmmTMB(fly_numbers 
                            ~ ratio * condition +
                            condition * block +
                            ratio * block
                            + (1 | block / plate) + (1 | observation), 
                            family = poisson, 
                            data = combined_m_split)

# Significance of 2-way interaction
drop1(m.glmm.p.4choice.feed.2, test = "Chisq")
# Significant interaction between "condition" and "block"

# Final model! 
m.glmm.p.4choice.feed.3 <- glmmTMB(fly_numbers 
                            ~ condition * block + ratio
                            + (1 | block / plate) + (1 | observation), 
                            family = poisson, 
                            data = combined_m_split)



#### Analysis with final chosen model: 

# Basic analysis 
summary(m.glmm.p.4choice.feed.3)

# Confidence intervals 
exp(confint(m.glmm.p.4choice.feed.3))

# Real values for write-up
emmeans::emmeans(m.glmm.p.4choice.feed.3, specs = ~ ratio + condition + block, type = "response")

## Table of model for write-up
tab_model(m.glmm.p.4choice.feed.3, CSS = list(css.table = '+font-family: Arial;')) 










                         #### Diets Conditioned by Virgin Females ####

# Testing for a 3-way significant effect 
vf.glm.nb.4choice.feed.1 <- glm.nb(fly_numbers ~ 
                             ratio * condition * block,
                             data = combined_vf_split)

# Significance of 3-way interaction
drop1(vf.glm.nb.4choice.feed.1, test = "Chisq")
# No significant interaction

## Testing for 2-way interaction effects
vf.glm.nb.4choice.feed.2 <- glm.nb(fly_numbers ~ 
                             ratio * condition +
                             condition * block +
                             ratio * block,
                             data = combined_vf_split)


# Looking for significance in the 2-way interaction effects
drop1(vf.glm.nb.4choice.feed.2, test = "Chisq")
# Only a significant interaction between "condition" and "block", 

# Final model! 
vf.glm.nb.4choice.feed.3 <- glm.nb(fly_numbers ~ 
                             condition * block + ratio,
                             data = combined_vf_split)

#### Analysis with final chosen model: 
# Basic analysis 
summary(vf.glm.nb.4choice.feed.3)

# Confidence intervals 
exp(confint(vf.glm.nb.4choice.feed.3))

# Real values for write-up
emmeans::emmeans(vf.glm.nb.4choice.feed.3, specs = ~ ratio + condition + block, type = "response")

## Table of model for write-up
tab_model(vf.glm.nb.4choice.feed.3, CSS = list(css.table = '+font-family: Arial;')) 









                                        #### OvoD1 Females ####

## Looking for a 3-way interaction effect: 
of.glm.nb.4choice.feed.1 <- glm.nb(fly_numbers ~ 
                              ratio * condition * block,
                              data = combined_of_split)


# Testing for significance in a 3-way interaction
drop1(of.glm.nb.4choice.feed.1, test = "Chisq")
# No significant 3-way interaction found

# Testing for 2-way interactions 
of.glm.nb.4choice.feed.2 <- glm.nb(fly_numbers ~ 
                                ratio * condition +
                                block * condition + 
                                ratio * block,
                              data = combined_of_split)


# Testing for significance in 2-way interactions
drop1(of.glm.nb.4choice.feed.2, test = "Chisq")
# 2 way interaction between ratio and condition found 


## Final chosen model 
of.glm.nb.4choice.feed.3 <- glm.nb(fly_numbers ~  
                                ratio * condition + block
                              ,data = combined_of_split)




#### Analysis with chosen model: 
# Basic analysis 
summary(of.glm.nb.4choice.feed.3)

# Confidence intervals 
exp(confint(of.glm.nb.4choice.feed.3))

# Real values for write-up
emmeans::emmeans(of.glm.nb.4choice.feed.3, specs = ~ ratio + condition + block, type = "response")

## Table of model for write-up
tab_model(of.glm.nb.4choice.feed.3, CSS = list(css.table = '+font-family: Arial;')) 







############################################ OVIPOSITION ########################################

#### Male Conditioning ~ Data Analysis ####

## Finding the appropriate model: 

# Testing for 3-way interaction 
m.glm.nb.4choice.ovi.1 <- glm.nb(egg_numbers
                            ~ ratio * condition * block, 
                            data =  combined_ovi_m_split)

# Using drop1 to test for the significance of the 3-way interaction 
drop1(m.glm.nb.4choice.ovi.1, test = "Chisq")
# No 3-way interaction 

# Testing for 2-way interactions 
m.glm.nb.4choice.ovi.2 <- glm.nb(egg_numbers
                                  ~ ratio * condition + 
                                  ratio * block + 
                                  condition * block, 
                                  data =  combined_ovi_m_split)

# Using drop1 to test for the significance of the 2-way interaction 
drop1(m.glm.nb.4choice.ovi.2, test = "Chisq")
# No 2-way interactions

# Final model 
m.glm.nb.4choice.ovi.3 <- glm.nb(egg_numbers
                                  ~ ratio + condition + block, 
                                  data =  combined_ovi_m_split)



#### Code for analysis with chosen model:  
# Basic analysis 
summary(m.glm.nb.4choice.ovi.3)

# Confidence intervals 
exp(confint(m.glm.nb.4choice.ovi.3))

# Real values for write-up
emmeans::emmeans(m.glm.nb.4choice.ovi.3, specs = ~ ratio + condition + block, type = "response")

## Table of model for write-up
tab_model(m.glm.nb.4choice.ovi.3, CSS = list(css.table = '+font-family: Arial;'))












#### Virgin Female ~ Data Analysis #### 

# Testing for a 3-way interaction effect
# Note: this model only seems to work with the random effect
# how it is and not with block
vf.glm.z.nb.4choice.ovi.1  <- glmmTMB(
  egg_numbers   ~ ratio * condition * block + (1| plate),  
  ziformula =  ~ ratio * condition * block,               
  family = nbinom2(),                          
  data = combined_ovi_v_split)

# Testing for significance in the 3-way interaction effect
drop1(vf.glm.z.nb.4choice.ovi.1, test = "Chisq")
# Significant 3-way interaction


#### Data analysis with chosen model: 
# Basic analysis 
summary(vf.glm.z.nb.4choice.ovi.1)

# Confidence intervals 
exp(confint(vf.glm.z.nb.4choice.ovi.1))

# Real values for write-up
emmeans::emmeans(vf.glm.z.nb.4choice.ovi.1, specs = ~ ratio + condition + block, type = "response")

## Table of model for write-up
tab_model(vf.glm.z.nb.4choice.ovi.1, CSS = list(css.table = '+font-family: Arial;'))









#### OvoD1 ~ Data Analysis #### 

# Testing for a 3-way interaction: 
of.glm.nb.4choice.ovi.1 <- glm.nb(egg_numbers
                                  ~ ratio * condition * block, 
                                  data =  fourone_onefour_oviposition_ovod1_long_split)


# Testing for  a significant  3-way interaction 
drop1(of.glm.nb.4choice.ovi.1, test = "Chisq")
# No 3-way interaction 

## Tesing for 2-way interactions:
of.glm.nb.4choice.ovi.2 <- glm.nb(egg_numbers
                                  ~ ratio * condition + 
                                  condition * block + 
                                  ratio * block, 
                                  data =  fourone_onefour_oviposition_ovod1_long_split)


# Testing for  a significant  2-way interactions 
drop1(of.glm.nb.4choice.ovi.2, test = "Chisq")
# condition and block significant 
# ratio and block significant 


# Final model, with the 2-way interactions
of.glm.nb.4choice.ovi.3 <- glm.nb(egg_numbers
                                ~ condition * block + 
                                  ratio * block, 
                                  data =  fourone_onefour_oviposition_ovod1_long_split)


#### Data Analysis for write-up

# Basic analysis
summary(of.glm.nb.4choice.ovi.3)

# Confidence intervals 
exp(confint(of.glm.nb.4choice.ovi.3))

# Real values for write-up
emmeans::emmeans(of.glm.nb.4choice.ovi.3, specs = ~ ratio + condition, type = "response")

## Table of model for write-up
tab_model(of.glm.nb.4choice.ovi.3, CSS = list(css.table = '+font-family: Arial;')) 






