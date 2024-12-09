

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


#### Data Analysis ####


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


