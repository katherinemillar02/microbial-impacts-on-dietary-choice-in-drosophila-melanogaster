##### Packages 📦📦📦📦 ####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
##################---


##### Data Analysis ####
## Model with two-way interaction
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) 
                    ~ ratio * block
                    + (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)


# Testing for the significance of the two-way interaction 
drop1(glmm.bin.m, test = "Chisq")
 # No significant 2-way interaction 


# New model with two-way interaction dropped
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) 
                    ~ ratio + block
                    + (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)



#### Data analysis for write-up ####

# Basic analysis 
summary(glmm.bin.m.2)

# Values for analysis write-up
emmeans::emmeans(glmm.bin.m.2, specs = ~ ratio, type = "response")
# assume that condition is conditioned (not unconditioned)

# Table for write-up
tab_model(glmm.bin.m.2, CSS = list(css.table = '+font-family: Arial;'))











#### Data Analysis ####

# Testing model with a two-way interaction effect 
glmm.bin.vf  <- glmer(cbind(Conditioned, Unconditioned)
                      ~ ratio * block + (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)


# Using drop1 to look for significance in a two-way interaction effect 
drop1(glmm.bin.vf, test = "Chisq")
  # No two-way interaction effect found 

# New model - without a two-way interaction effect 
glmm.bin.vf.2  <- glmer(cbind(Conditioned, Unconditioned)
                      ~ ratio + block + (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)




#### Data analysis for write-up ####
# Basic analysis
summary(glmm.bin.vf.2)

# Confidence intervals
exp(confint(glmm.bin.vf.2))

# Values for analysis write-up
emmeans::emmeans(glmm.bin.vf.2, specs = ~ ratio, type = "response")
 # assume that condition is conditioned (not unconditioned)

# Table for write-up
tab_model(glmm.bin.vf.2, CSS = list(css.table = '+font-family: Arial;'))










#### Data Analysis ####

# Model with two-way interaction effect
glmm.bin.of <- glmer(cbind(Conditioned, Unconditioned) 
                     ~ ratio * block  + (1|block/plate) + (1|block/observation), family = binomial, data = df2_ovod1)

## Testing for significance of the two-way interaction effect 
drop1(glmm.bin.of, test = "Chisq")
 # A significant 2-way interaction effect



#### Data analysis for write-up #### 

# Basic analysis
summary(glmm.bin.of)

# Confidence intervals
exp(confint(glmm.bin.of))

# Values for analysis write-up
emmeans::emmeans(glmm.bin.of, specs = ~ ratio, type = "response")
 # Assume condition is conditioned (not unconditioned)

# Table for write-up
tab_model(glmm.bin.of, CSS = list(css.table = '+font-family: Arial;'))










