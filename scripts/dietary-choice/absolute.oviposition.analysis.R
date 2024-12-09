#### Chapter 2 ####

# Analyses for write up 

#### Packages #### 📦📦📦📦
library(tidyverse)
library(lmerTest)
library(readxl)
library(DHARMa)
library(glmmTMB)
library(lme4)
library(performance)
library(pscl)
library(MASS)
############### 📦📦📦📦




#### Data Analysis ####

## Testing for a two-way interaction effect
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                                ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_male_oviposition)



## Checking for significance in the two-way interaction effect:
drop1(glmm.bin.m.ovi.2choice, test = "Chisq")
  # Significant two way interaction effect between ratio and block


#### Data Analysis #### 

# Basic analysis
summary(glmm.bin.m.ovi.2choice)

# Confidence intervals
exp(confint(glmm.bin.m.ovi.2choice))

# Values for analysis write-up
emmeans::emmeans(glmm.bin.m.ovi.2choice, specs = ~ ratio, type = "response")

# Table for write-up
tab_model(glmm.bin.m.ovi.2choice, CSS = list(css.table = '+font-family: Arial;'))




#### Data Analysis ####

# Final chosen model: Binomial GLMM
 # Testing for two-way interaction effect 
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~
                                   ratio * block + (1|block/plate)  , family = binomial, data = df2_virgin_oviposition)


# Looking for significance in the two-way interaction effect 
drop1(glmm.bin.vf.ovi.2choice, test = "Chisq")
 # A significant two-way interaction effect 




#### Data Analysis for write-up #### 

# Basic analysis
summary(glmm.bin.vf.ovi.2choice)

# Confidence intervals
exp(confint(glmm.bin.vf.ovi.2choice))

# Values for analysis write-up
emmeans::emmeans(glmm.bin.vf.ovi.2choice, specs = ~ ratio, type = "response")

# Table for write-up
tab_model(glmm.bin.vf.ovi.2choice, CSS = list(css.table = '+font-family: Arial;'))




#### Data Analysis ####

## Binomial GLMM - to consider random effects 
 # Testing for the significance of a two-way interaction 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) 
                                 ~ ratio * block + (1|block/plate)
                                 , family = binomial, data = df2_ovod1_oviposition)


# Testing for two-way interaction significance 
drop1(glmm.bin.of.ovi.2choice, test = "Chisq")
 # two-way interaction is significant 


#### Data Analysis for write-up ####
# Basic analysis
summary(glmm.bin.of.ovi.2choice)

# Confidence intervals
exp(confint(glmm.bin.of.ovi.2choice))

# Values for analysis write-up
emmeans::emmeans(glmm.bin.of.ovi.2choice, specs = ~ sex + treatment, type = "response")

# Table for write-up
tab_model(glmm.bin.of.ovi.2choice, CSS = list(css.table = '+font-family: Arial;'))









