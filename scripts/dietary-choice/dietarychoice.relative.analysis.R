

#############################################################################################
############## The Effects of Diet Conditioning on Female  Dietary Choice ################### 
####################################### Katie Millar ########################################
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










#### Data Analysis ####



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















#### Data Analysis ####



#### Negative Binomial GLM #### 

# 3-way interaction test 
glm.nb.of.4choice <- glm.nb(fly_numbers ~ 
                              ratio * condition * block,
                            data = combined_of_split)


# Testing for significance in a 3-way interaction
drop1(glm.nb.of.4choice, test = "Chisq")
# No significant 3-way interaction found



# 2-way interaction tests 
glm.nb.of.4choice.2 <- glm.nb(fly_numbers ~ 
                                ratio * condition +
                                block * condition + 
                                ratio * block,
                              data = combined_of_split)


# Testing for significance in 2-way interactions
drop1(glm.nb.of.4choice.2, test = "Chisq")
# 2 way interaction between ratio and condition found 


## Final chosen model 
glm.nb.of.4choice.3 <- glm.nb(fly_numbers ~  
                                ratio * condition + block
                              ,
                              data = combined_of_split)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glm.nb.of.4choice.3)

# Confidence intervals 
exp(confint(glm.nb.of.4choice.3))


# Real values for write-up
emmeans::emmeans(glm.nb.of.4choice.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.nb.of.4choice.3, CSS = list(css.table = '+font-family: Arial;')) 

#### OVIPOSITION ####
#### Male Conditioning ~ Data Analysis ####

## Finding the appropriate model: 

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



#### Code for analysis with chosen model:  

# Basic analysis 
summary(comb_m_egg_glm.nb.3)

# Confidence intervals 
exp(confint(comb_m_egg_glm.nb.3))

# Real values for write-up
emmeans::emmeans(comb_m_egg_glm.nb.3, specs = ~ ratio + condition + block, type = "response")

## Table of model for write-up
tab_model(comb_m_egg_glm.nb.3, CSS = list(css.table = '+font-family: Arial;'))












#### Virgin Female ~ Data Analysis #### 


#### Chosen model: Zero-Inflated Negative Binomial... 

# Testing for a 3-way interaction effect
# Note: this model only seems to work with the random effect how it is and not with block
glm.zi.nb.v.egg  <- glmmTMB(
  egg_numbers   ~ ratio * condition * block + (1| plate),  
  ziformula =  ~ ratio * condition * block,               
  family = nbinom2(),                          
  data = combined_ovi_v_split
)
# Testing for significance in the 3-way interaction effect
drop1(glm.zi.nb.v.egg, test = "Chisq")
# Significant 3-way interaction


#### 2. Data analysis for write-up

# Basic analysis 
summary(glm.zi.nb.v.egg)


# Confidence intervals 
exp(confint(glm.zi.nb.v.egg))


# Real values for write-up
emmeans::emmeans(glm.zi.nb.v.egg, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.zi.nb.v.egg, CSS = list(css.table = '+font-family: Arial;'))













#### OvoD1 ~ Data Analysis #### 


# Using this model 
# Testing for a 3-way interaction: 
glm.nb_ovo_comb_egg <- glm.nb(egg_numbers
                              ~ ratio * condition * block, 
                              data =  combined_of_egg_split)



# Testing for  a significant  3-way interaction 
drop1(glm.nb_ovo_comb_egg, test = "Chisq")
# No 3-way interaction 


## Now testing for two-way interactions
glm.nb_ovo_comb_egg.2 <- glm.nb(egg_numbers
                                ~ ratio * condition + 
                                  condition * block + 
                                  ratio * block, 
                                data =  combined_of_egg_split)


# Testing for  a significant  2-way interactions 
drop1(glm.nb_ovo_comb_egg.2, test = "Chisq")
# condition and block significant 
# ratio and block significant 


# Final model, with the 2-way interactions
glm.nb_ovo_comb_egg.3 <- glm.nb(egg_numbers
                                
                                ~ condition * block + 
                                  ratio * block, 
                                
                                data =  combined_of_egg_split)


#### Data Analysis for write-up

# Basic analysis
summary(glm.nb_ovo_comb_egg.3)

# Confidence intervals 
exp(confint(glm.nb_ovo_comb_egg.3))

# Real values for write-up
emmeans::emmeans(glm.nb_ovo_comb_egg.3, specs = ~ ratio + condition + block, type = "response")

## Table of model for write-up
tab_model(glm.nb_ovo_comb_egg.3, CSS = list(css.table = '+font-family: Arial;')) 






