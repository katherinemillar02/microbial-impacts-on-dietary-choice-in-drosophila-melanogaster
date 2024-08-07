## Packages ## 📦📦📦📦
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

#### READING THE DATA IN: 
## Reading the data in using paths. 

####################### --
#### VIRGIN FEMALE ####
####################### --

## Creating a path to Virgin Conditioning Oviposition data
pathvirginoviposition <- "data/female_conditioning/virgin"

# Reading in Oviposition data for Virgin Conditioning 
read_raw_virgin_oviposition <-function(path = pathvirginoviposition, pattern_to_exclude = "4-1_1-4_oviposition"){
  list_of_files <- list.files(path = pathvirginoviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}



# using the function to find the path 
read_raw_virgin_oviposition(pathvirginoviposition)

#### Removing "Block 1" from the data-set, as this is not to be used for oviposition. 


## Creating a data-set that will read the paths
# first data frame - purr package 
df_virgin_oviposition <- pathvirginoviposition %>% 
  map_df(~read_raw_virgin_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently


## Mutating a variable for block 
df_virgin_oviposition <- df_virgin_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b2") ~ "two",
    str_detect(id, "b3") ~ "three",
    str_detect(id, "b4") ~ "four",
  ))


# Generating a new data-frame, separating diet. 
df2_virgin_oviposition <- df_virgin_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>% #separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ##  group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 


## New data 
df2_virgin_oviposition # does it recognise condition from the long data





############### ---
#### OVOD1 FEMALE ----
################# ---

# Creating a path to the appropriate data files 
pathovod1oviposition <- "data/female_conditioning/ovod1"

## Creating a function to read the appropriate data files
######################################################################################################################## --
read_raw_ovod1_oviposition <-function(path = pathovod1oviposition, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathovod1oviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
######################################################################################################################## --





## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovod1_oviposition(pathovod1oviposition) 


## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovod1_oviposition <- pathovod1oviposition %>% 
  map_df(~read_raw_ovod1_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently


## Mutating a variable for block
df_ovod1_oviposition <- df_ovod1_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    
  ))


# uses what was generated with "df"
df2_ovod1_oviposition <- df_ovod1_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_ovod1_oviposition # does it recognise condition from the long data? 








################ --
#### MALE ####
################ --

## Creating a path to get to the data
pathmaleoviposition <- "data/male_conditioning"


################################################################################################################ --
read_raw_male_oviposition <-function(path = pathmaleoviposition, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathmaleoviposition,
                              pattern = "oviposition", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "plate")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(3:6), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "egg_numbers") %>%
    drop_na(egg_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
################################################################################################################--


## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male_oviposition(read_raw_male_oviposition)



## creating an actual data set that will read the paths
# first data frame - purr package 
df_male_oviposition <- pathmaleoviposition %>% 
  map_df(~read_raw_male_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently



## Mutating a variable for block 
df_male_oviposition <- df_male_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two"
  ))



 
# uses what was generated with "df"
df2_male_oviposition <- df_male_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_male_oviposition # does it recognise condition from the long data? 




## This is the data frame for male oviposition, for just 4:1 and 1:4 - and includes a variable for block one and block two


############################################################ --







######################################################################################################################## --
#### Data Analysis ####
# 4:1 and 1:4 Two-Choice Assays ####
######################################################################################################################## --


########################### --
#### Male Oviposition #####
########################### --


## Model 1 
# Binomial GLM model
glm.bin.m.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_male_oviposition)

## Assumption checking 

## easystats

## qq 
performance::check_model(glm.bin.m.ovi.2choice, check = c("qq")) ## the qq for this model looks pretty good

## different qq
residuals_m <- residuals(glm.bin.m.ovi.2choice, type = "pearson")
qnorm(residuals_m)
qqnorm(residuals_m)
qqline(residuals_m) ## there are still some dispersed lines 

## Homogeneity 
performance::check_model(glm.bin.m.ovi.2choice, check = c("homogeneity")) 
## there is still some dispersion between variables
## not everything completley lines up 

## Another way to look at qq and homogeneity 
plot(glm.bin.m.ovi.2choice) ## similar results, points aren't completley together 

## Using DHARMa
testDispersion(glm.bin.m.ovi.2choice) ## This is quite bad? 
## seems to be quite overdispersed 

check_overdispersion(glm.bin.m.ovi.2choice) ## model is quite overdispersed 

## Looking at qq and homogenity again
simulation_Output <- simulateResiduals(fittedModel = glm.bin.m.ovi.2choice, plot = F)
plot(simulation_Output) 
## This shows even worse results, why is this? 


#### The assumptions of a Binomial GLM weren't great, so trying a GLMM. 




## Model 2
## Trying a Binomial GLMM to consider random effects. 
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_male_oviposition)


## Assumption Checking 

## qq checking 
performance::check_model(glmm.bin.m.ovi.2choice, check = c("qq")) 
## This qqplot seems to go more off at the end 
## but dots are sort of on the line 


## different qq
residuals_m_mixed <- residuals(glmm.bin.m.ovi.2choice, type = "pearson")
qnorm(residuals_m_mixed)
qqnorm(residuals_m_mixed)
qqline(residuals_m_mixed)
## There is some weird dispersion, goes off at the end 


## Homogeneity assumption checking 
performance::check_model(glmm.bin.m.ovi.2choice, check = c("homogeneity")) 
## This is all a lot less lined up than the binomial model 
## assumptions do not look greatly met here 

## other ways to look as assumptions 
plot(glmm.bin.m.ovi.2choice) ## This doesn't generate everything? 

## Using DHARMa 
## looking for overdispersion 
testDispersion(glmm.bin.m.ovi.2choice) ## This looks pretty good

# easystats dispersion check 
check_overdispersion(glmm.bin.m.ovi.2choice) ## still says overdispersion is detected 

## More ways to look at qq and homogeneity
simulation_Output <- simulateResiduals(fittedModel = glmm.bin.m.ovi.2choice, plot = T)
plot(simulation_Output) 
## Not much really lines up well in either of these 


## Comparing the two models 
AIC(glm.bin.m.ovi.2choice, glmm.bin.m.ovi.2choice)
## It says the mixed model has a much lower AIC,
## even though I think the assumptions look a bit worse? 

## Changing "ratio" to 4:1 
df2_male_oviposition$ratio <- as.factor(df2_male_oviposition$ratio)
df2_male_oviposition$ratio <- relevel(df2_male_oviposition$ratio, ref = "4:1")

## Changing block to "two" (as one will not be used for oviposition data)
df2_male_oviposition$block <- as.factor(df2_male_oviposition$block)
df2_male_oviposition$block <- relevel(df2_male_oviposition$block, ref = "one")


## Choosing the mixed model for now: 
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_male_oviposition)

## Looking for the significance in Block
drop1(glmm.bin.m.ovi.2choice, test = "Chisq") ## says block is quite significant, keeping it in the model


## Using model for now, looking at results: 
summary(glmm.bin.m.ovi.2choice) ## says block is significant 

exp(confint(glmm.bin.m.ovi.2choice, method = "Wald"))



## Looking at the response variables for analysis
emmeans::emmeans(glmm.bin.m.ovi.2choice, ~ ratio + block , type = "response")

tab_model(glmm.bin.m.ovi.2choice)








################################## --
### OvoD1 Female  Oviposition ####
################################## --


## Model 1 

# Binomial model 
glm.bin.of.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_ovod1_oviposition)

# Assumption checking 

# easystats does not work at the moment 
performance::check_model(glm.bin.of.ovi.2choice , check = c("qq")) ## looks okay

## comparing this with a different qqplot to understand qq plots 
## Generating residuals 
residuals <- residuals(glm.bin.of.ovi.2choice, type = "pearson")
qnorm(residuals)
## Will generate a qq 
qqnorm(residuals) ## qq looks bad 
qqline(residuals, col = "green") ## putting a line there to understand it better

## Understanding the qqplot:  
## There are points in the data below where the data should fall?
## should be straight across if in ascending order? 

## Homogeneity
# easystats plot
performance::check_model(glm.bin.of.ovi.2choice , check = c("homogeneity")) ## the dots are not really lines up 
# first one should be homogeneity
plot(glm.bin.of.ovi.2choice) ## lines look sort of lined up but not the best

## Understanding homogeneity: 
# Assumptions: the level of variance for a particular variable is constant across the sample 
# Groups of data: the variance of the outcome variable should be the same in each group
# So, I think the dots should sort of be in line with eachother? 
# This doesn't really happen, so there isn't a great assumption of homogeneity? 



#### MORE ASSUMPTION CHECKS
# DHARMa 
testDispersion(glm.bin.of.ovi.2choice) # looks pretty poor
## easystats overdispersion check
check_overdispersion(glm.bin.of.ovi.2choice) ## overdispersion 

## Understanding overdispersion 
## There is quite a lot of overdispersion
## This means that the variance of the response variable (fly numbers) is greater than what is
## expected by the model

## Another qqplot 
simulationOutput <- simulateResiduals(fittedModel = glm.bin.of.ovi.2choice, plot = T)
## is predicted the same as fitted? 
## is the second plot homogeneity assumptions? 

## understanding the qqplot shows the points are off the usual data and the second plot shows there is not 
## really homogeneity of variance 

## Overall, this model is really not the best...




## Model 2 ##

## Binomial GLMM - to consider random effects 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_ovod1_oviposition)


# Assumption checking 
# easystats 
performance::check_model(glmm.bin.of.ovi.2choice, check = c("qq"))
## this qqplot looks pretty good, this means the data falls through quite a nice distribution
## data is expected to fall the way the data falls 

## different way of doing a qqplot
## Generating residuals 
residuals <- residuals(glmm.bin.of.ovi.2choice, type = "pearson")
qnorm(residuals)
## Will generate a qq 
qqnorm(residuals) ## qq looks a lot better 
qqline(residuals, col = "green") ## again, points seem to fall along the line 


## Homogeneity check 
performance::check_model(glmm.bin.of.ovi.2choice, check = c("homogeneity")) 
## There is still not great homogeneity assumptions 
## This means the level of variance is not really constant still 



## MORE ASSUMPTIONS CHECKS 
# DHARMa 

## Looking for overdispersion
testDispersion(glmm.bin.of.ovi.2choice) # looks a lot better I think 

# easystats check for overdispersion 
check_overdispersion(glmm.bin.of.ovi.2choice) ## there is still overdispersion detected 


## More DHARMa assumption checks 
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.of.ovi.2choice, plot = T)
## DHARMa assumption checks show the model to look a lot better 

## Doing an AIC check
AIC(glm.bin.of.ovi.2choice, glmm.bin.of.ovi.2choice)
## The model with random effects (Model 2) is a lot better, as expected from the other assumptions... 

## Using "Model 2" for now as I do not really know what else to do 

## Model choice: 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block + (1|plate/block)  , family = binomial, data = df2_ovod1_oviposition)

emmeans::emmeans(glmm.bin.of.ovi.2choice, ~ ratio, type = "response")

summary(glmm.bin.of.ovi.2choice)

## Looking for significance in block
drop1(glmm.bin.of.ovi.2choice, test = "Chisq") ## block is  significant with ratio

## Looking at results, keeping block in 

## Basic summary
summary(glmm.bin.of.ovi.2choice) 

## What I am confused about here: 
# If I am interpreting the results right, why is there no 1:4 conditioned/unconditioned block 2 results?? 









############################## --
## Virgin Female Oviposition #### 
############################## --

## Model 1 

# Binomial model 
glm.bin.vf.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_virgin_oviposition)

summary(glm.bin.vf.ovi.2choice)


# Assumption checking 

## qq checks 
performance::check_model(glm.bin.vf.ovi.2choice, check = c("qq"))
## the points do not line up greatly, but it is not the worst 
## dispersion at the beginning and the end 

## another qq
residuals_v_binom <- residuals(glm.bin.vf.ovi.2choice, type = "pearson")
qnorm(residuals_v_binom)
## Will generate a qq 
qqnorm(residuals_v_binom) 
qqline(residuals_v_binom, col = "green") 
## points sort of fall along the line, but again - not at the beginning and end 


## Testing homogeneity assumptions 
performance::check_model(glm.bin.vf.ovi.2choice, check = c("homogeneity"))
## there seems to be quite a lot of variation 

## more ways to test assumptions
plot(glm.bin.vf.ovi.2choice)
## the way that things line up isn't actually awful 


# DHARMa assumption checks 

## Looking for overdispersion 
testDispersion(glm.bin.vf.ovi.2choice) 
## I think this shows to be quite a lot of overdsispersion 

## easystats overdispersion checker 
check_overdispersion(glm.bin.vf.ovi.2choice)
## there is some overdispersion, quite a bit? 


## Doing more DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glm.bin.vf.ovi.2choice, plot = T)
## Things do not line up too great




## Model 2
## a glm mixed model
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_virgin_oviposition)

# Assumption checking 

# easystats
performance::check_model(glmm.bin.vf.ovi.2choice, check = c("qq"))
## this seems to line up better than the binomial model 
## but there are still some dispersed points at the beginning and the end

## another qq
residuals_v_mixed <- residuals(glmm.bin.vf.ovi.2choice, type = "pearson")
qnorm(residuals_v_mixed)
## Will generate a qq 
qqnorm(residuals_v_mixed) 
qqline(residuals_v_mixed , col = "green") 
## again, points sort of fall along the line but there is some dispersion at the beginning and end 


## Homogeneity assumption checks 
performance::check_model(glmm.bin.vf.ovi.2choice, check = c("homogeneity"))
## there seems to be lots of variation in the points 

# Trying more assumption checks
plot(glmm.bin.vf.ovi.2choice)
## Just shows lots of dispersion really


# DHARMa assumption checks
testDispersion(glmm.bin.vf.ovi.2choice) # quite overdispersed

## easystats dispersion check
check_overdispersion(glmm.bin.vf.ovi.2choice) 
## There is overdispersion check 

## More general assumption checks using DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.vf.ovi.2choice, plot = T)
# Some points fall together, but some points are still quite dispersed 


## Comparing models
AIC(glm.bin.vf.ovi.4choice, glmm.bin.vf.ovi.2choice)
## the mixed model has a slightly lower AIC 




## Using the mixed model for now 
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_virgin_oviposition)


## Looking for significance in block
drop1(glmm.bin.vf.ovi.2choice, test = "Chisq") ## block is significant


## Changing "ratio" to 4:1 
df2_virgin_oviposition$ratio <- as.factor(df2_virgin_oviposition$ratio)
df2_virgin_oviposition$ratio <- relevel(df2_virgin_oviposition$ratio, ref = "4:1")

## Changing block to "two" (as one will not be used for oviposition data)
df2_virgin_oviposition$block <- as.factor(df2_virgin_oviposition$block)
df2_virgin_oviposition$block <- relevel(df2_virgin_oviposition$block, ref = "two")

## Looking at results, keeping block in 
summary(glmm.bin.vf.ovi.2choice)
## block 3 sig with block 1
## block 2 not sig with block 1 


exp(confint(glmm.bin.vf.ovi.2choice, method = "Wald"))

drop1()


glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate/block)  , family = binomial, data = df2_virgin_oviposition)





emmeans::emmeans(glmm.bin.vf.ovi.2choice, pairwise ~ ratio, type = "response")

summary(glmer.virgin_f_egg )

emmeans::emmeans(glmer.virgin_f_egg, ~ ratio, type = "response")

tab_model(glmm.bin.vf.ovi.2choice)










