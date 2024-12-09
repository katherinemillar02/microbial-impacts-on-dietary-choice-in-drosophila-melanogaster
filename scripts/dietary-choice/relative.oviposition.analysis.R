
#### Packages 📦📦📦📦 #### 
library(ggpubr)
source("packages.R")
source("scripts/dietary-choice/dietarychoice_dataread.R")
#### 

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


#### 2. Data analysis for write-up ####

# Basic analysis 
summary(glm.zi.nb.v.egg)


# Confidence intervals 
exp(confint(glm.zi.nb.v.egg))


# Real values for write-up
emmeans::emmeans(glm.zi.nb.v.egg, specs = ~ ratio + condition + block, type = "response")


## Table of model for write-up
tab_model(glm.zi.nb.v.egg, CSS = list(css.table = '+font-family: Arial;'))













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






