####################################### Data Analysis ###########################################
##################### Dietary Choice by Mated Females in an Absolute Environment ################ 
####################################, ####################################
#################################################################################################


################################## Packages and Data 📦📦📦📦 ##################################
source("packages.R")
# a script containing all packages needed for data analysis 
source("scripts/dietary-choice/dietarychoice.dataread.R") 
# a script containing all data needed for the dietary choice analysis 
################################################################################################



###################################### FEEDING ANALYSIS ########################################

#### Diets Conditioned by Males ####
## Model with two-way interaction
m.glmm.bin.feed.1 <- glmer(cbind(Conditioned, Unconditioned) 
                    ~ ratio * block
                    + (1|block/plate) 
                    + (1|block/ observation), 
                    family = binomial, 
                    data = df2_male)

# Testing for the significance of the two-way interaction 
drop1(m.glmm.bin.feed.1 , test = "Chisq")
 # No significant 2-way interaction 

# New model with two-way interaction dropped:
m.glmm.bin.feed.2 <- glmer(cbind(Conditioned, Unconditioned) 
                           ~ ratio + block +
                             (1|block/plate) +
                             (1|block/ observation),
                           family = binomial,
                           data = df2_male)

# Final model: 
summary(m.glmm.bin.feed.2)

# Values for analysis write-up
emmeans::emmeans(m.glmm.bin.feed.2, specs = ~ ratio, type = "response")
# assume that condition is conditioned (not unconditioned)

# Table for write-up
tab_model(m.glmm.bin.feed.2, CSS = list(css.table = '+font-family: Arial;'))








#### Diets Conditioned by Virgin Females ####

# Testing model with a two-way interaction effect 
vf.glmm.bin.feed.1  <- glmer(cbind(Conditioned, Unconditioned)
                             ~ ratio * block +
                               (1|block/plate) + 
                               (1|block/observation),
                             family = binomial, 
                             data = df2_virgin)


# Using drop1 to look for significance in a two-way interaction effect 
drop1(vf.glmm.bin.feed.1, test = "Chisq")
  # No two-way interaction effect found 

# New model - without a two-way interaction effect 
vf.glmm.bin.feed.3  <- glmer(cbind(Conditioned, Unconditioned)
                      ~ ratio + block +
                        (1|block/plate) +
                        (1|block/observation),
                      family = binomial, 
                      data = df2_virgin)

# Final model: 
summary(vf.glmm.bin.feed.3)

# Confidence intervals
exp(confint(vf.glmm.bin.feed.3))

# Values for analysis write-up
emmeans::emmeans(vf.glmm.bin.feed.3, specs = ~ ratio, type = "response")
 # assume that condition is conditioned (not unconditioned)

# Table for write-up
tab_model(vf.glmm.bin.feed.3, CSS = list(css.table = '+font-family: Arial;'))









#### Diets Conditioned by OvoD1 Females ####

# Model with two-way interaction effect
of.glmm.bin.feed.1  <- glmer(cbind(Conditioned, Unconditioned) 
                     ~ ratio * block  +
                       (1|block/plate) +
                       (1|block/observation),
                     family = binomial,
                     data = df2_ovod1)

## Testing for significance of the two-way interaction effect 
drop1(of.glmm.bin.feed.1, test = "Chisq")
 # A significant 2-way interaction effect

# Final analysis
summary(of.glmm.bin.feed.1)

# Confidence intervals
exp(confint(of.glmm.bin.feed.1))

# Values for analysis write-up
emmeans::emmeans(of.glmm.bin.feed.1, specs = ~ ratio, type = "response")
 # Assume condition is conditioned (not unconditioned)

# Table for write-up
tab_model(of.glmm.bin.feed.1, CSS = list(css.table = '+font-family: Arial;'))








###################################### OVIPSOSITION ANALYSIS ########################################


#### Diets Conditioned by Males ####

## Testing for a two-way interaction effect
m.glmm.bin.ovi.1 <- glmer(cbind(Conditioned, Unconditioned)
                                ~ ratio * block +
                                  (1|block/plate),
                                family = binomial,
                                data = df2_male_oviposition)



## Checking for significance in the two-way interaction effect:
drop1(m.glmm.bin.ovi.1, test = "Chisq")
# Significant two way interaction effect between ratio and block

# Basic analysis
summary(m.glmm.bin.ovi.1)

# Confidence intervals
exp(confint(m.glmm.bin.ovi.1))

# Values for analysis write-up
emmeans::emmeans(m.glmm.bin.ovi.1, specs = ~ ratio, type = "response")

# Table for write-up
tab_model(m.glmm.bin.ovi.1, CSS = list(css.table = '+font-family: Arial;'))




#### Data Analysis ####

# Final chosen model: Binomial GLMM
# Testing for two-way interaction effect 
vf.glmm.bin.ovi.1 <- glmer(cbind(Conditioned, Unconditioned) ~
                            ratio * block +
                            (1|block/plate), 
                          family = binomial,
                          data = df2_virgin_oviposition)


# Looking for significance in the two-way interaction effect 
drop1(vf.glmm.bin.ovi.1, test = "Chisq")
# A significant two-way interaction effect 

# Basic analysis
summary(vf.glmm.bin.ovi.1)

# Confidence intervals
exp(confint(vf.glmm.bin.ovi.1))

# Values for analysis write-up
emmeans::emmeans(vf.glmm.bin.ovi.1, specs = ~ ratio, type = "response")

# Table for write-up
tab_model(vf.glmm.bin.ovi.1, CSS = list(css.table = '+font-family: Arial;'))




#### Diets Conditioned by OvoD1 Females ####

# Testing for the significance of a two-way interaction 
of.glmm.bin.ovi.1 <- glmer(cbind(Conditioned, Unconditioned) 
                           ~ ratio * block +
                             (1|block/plate),
                           family = binomial, 
                           data = df2_ovod1_oviposition)


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






