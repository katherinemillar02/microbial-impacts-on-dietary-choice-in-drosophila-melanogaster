
#### Chapter 2 ####

# Just for analysis script

#### Packages 📦📦📦📦 #### 
library(tidyverse)
library(lmerTest)
library(readxl)
library(DHARMa)
library(glmmTMB)
library(lme4)
library(performance)
library(pscl)
library(MASS)
library(sjPlot)
#### 





#### Male #### 


#### Reading, binding, cleaning data 📖 ####

# 4:1 + 1:4 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_1-4_b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_1-4_b2_oviposition.xlsx")
# Mutating a block variable
fourone_onefour_male_oviposition_b1 <- fourone_onefour_male_oviposition_b1 %>% mutate(block = "one")
fourone_onefour_male_oviposition_b2 <- fourone_onefour_male_oviposition_b2%>% mutate(block = "two")
# Binding the data for 4:1/1:4 
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)

## Making the data different dataframes
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Splitting "diet" up
combined_ovi_m_split <- combined_ovi_m %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")







#### Data Analysis ####

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



#### Data analysis for write-up #### 

# Basic analysis 
summary(comb_m_egg_glm.nb.3)


# Confidence intervals 
exp(confint(comb_m_egg_glm.nb.3))


# Real values for write-up
emmeans::emmeans(comb_m_egg_glm.nb.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(comb_m_egg_glm.nb.3, CSS = list(css.table = '+font-family: Arial;'))











#### Virgin Female ####
#### Reading, binding, cleaning data 📖 ####

## Reading in the different data-sets
fourone_onefour_oviposition_virgin_b2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx")
fourone_onefour_oviposition_virgin_b3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx")
fourone_onefour_oviposition_virgin_b4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx")
## Mutating a block variable to the data-sets
fourone_onefour_oviposition_virgin_b2 <- fourone_onefour_oviposition_virgin_b2 %>% mutate(block = "two")
fourone_onefour_oviposition_virgin_b3 <- fourone_onefour_oviposition_virgin_b3 %>% mutate(block = "three")
fourone_onefour_oviposition_virgin_b4 <- fourone_onefour_oviposition_virgin_b4 %>% mutate(block = "four")

## Binding the different data-sets
fourone_onefour_oviposition_virgin <- rbind(fourone_onefour_oviposition_virgin_b2, fourone_onefour_oviposition_virgin_b3, fourone_onefour_oviposition_virgin_b4)


## adding some data names
combined_ovi_v <- fourone_onefour_oviposition_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Using separate to split the diet into rartio and condition 
combined_ovi_v_split <- combined_ovi_v %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")





#### Data Analysis #### 


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


#### 2. Data analysis for write-up ####

# Basic analysis 
summary(glm.zi.nb.v.egg)


# Confidence intervals 
exp(confint(glm.zi.nb.v.egg))


# Real values for write-up
emmeans::emmeans(glm.zi.nb.v.egg, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.zi.nb.v.egg, CSS = list(css.table = '+font-family: Arial;'))












#### OvoD1 Female #### 


#### Reading, binding, cleaning data 📖 ####
## Reading the data in 
combined_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
combined_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")

# Mutating a block variable 
combined_ovod1_b1 <- combined_ovod1_b1  %>% mutate(block = "one")
combined_ovod1_b2 <- combined_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
combined_ovod1 <- rbind(combined_ovod1_b1, combined_ovod1_b2)

# Making the data long
combined_of_egg  <- combined_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Splitting up diet variable 
combined_of_egg_split <- combined_of_egg %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")




#### Data Analysis #### 


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


#### Data Analysis for write-up #### 

# Basic analysis
summary(glm.nb_ovo_comb_egg.3)

# Confidence intervals 
exp(confint(glm.nb_ovo_comb_egg.3))


# Real values for write-up
emmeans::emmeans(glm.nb_ovo_comb_egg.3, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.nb_ovo_comb_egg.3, CSS = list(css.table = '+font-family: Arial;')) 






