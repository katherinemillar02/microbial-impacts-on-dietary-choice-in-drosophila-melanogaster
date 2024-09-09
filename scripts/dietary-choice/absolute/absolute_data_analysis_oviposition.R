
#### Chapter 2 ####

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




#### READING THE DATA IN: 

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










######################################################################################################################## --
#### Data Analysis ####
# 4:1 and 1:4 Two-Choice Assays ####
######################################################################################################################## --


########################### --
#### Male Oviposition #####
########################### --


## Model 1 
# Binomial GLM model
glm.bin.m.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) 
                             ~ ratio * block, family = binomial, data = df2_male_oviposition)

## Assumption checking 

# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glm.bin.m.ovi.2choice, plot = T)
 # Model could be better 

## easystats
check_overdispersion(glm.bin.m.ovi.2choice)
 # overdispersion detected

check_zeroinflation(glm.bin.m.ovi.2choice)
 # cannot do zeroinflation


## Homogeneity 
performance::check_model(glm.bin.m.ovi.2choice, check = c("homogeneity")) 
## there is still some dispersion between variables




## Model 2
## Trying a Binomial GLMM to consider random effects. 
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                                ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_male_oviposition)


## Assumption Checking 

## DHARMa
simulation_Output <- simulateResiduals(fittedModel = glmm.bin.m.ovi.2choice, plot = T)
 # slighlty better, but there is deviation


# easystats dispersion check 
check_overdispersion(glmm.bin.m.ovi.2choice) 
  # No overdispersion detected 



## Note, the KS test is not great, not sure what other model could be used? 




## Comparing the two models 
AIC(glm.bin.m.ovi.2choice, glmm.bin.m.ovi.2choice)
## It says the mixed model has a much lower AIC,
## even though I think the assumptions look a bit worse? 




## Choosing the mixed model for now: 
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~
                                  ratio * block + (1|block/plate)  , family = binomial, data = df2_male_oviposition)

## Looking for the significance in Block
drop1(glmm.bin.m.ovi.2choice, test = "Chisq") 
## says block is quite significant, keeping it in the model






################################## --
### OvoD1 Female  Oviposition ####
################################## --


## Model 1
# GLM Binomial #
glm.bin.of.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_ovod1_oviposition)

# Assumption checking 

## Homogeneity
# easystats plot
performance::check_model(glm.bin.of.ovi.2choice , check = c("homogeneity")) ## the dots are not really lines up 
 # first one should be homogeneity, not sure it looks great 


## Understanding homogeneity: 
# Assumptions: the level of variance for a particular variable is constant across the sample 
# Groups of data: the variance of the outcome variable should be the same in each group
# So, I think the dots should sort of be in line with eachother? 
# This doesn't really happen, so there isn't a great assumption of homogeneity? 

# DHARMa 
simulationOutput <- simulateResiduals(fittedModel = glm.bin.of.ovi.2choice, plot = T)
  ## This model is worse, all tests are significant 






## Model 2 ##

## Binomial GLMM - to consider random effects 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) 
                                 ~ ratio * block + (1|block/plate)
                                 , family = binomial, data = df2_ovod1_oviposition)


# Assumption checking 


## Homogeneity check 
performance::check_model(glmm.bin.of.ovi.2choice, check = c("homogeneity")) 
## There is still not great homogeneity assumptions 
## This means the level of variance is not really constant still 



## MORE ASSUMPTIONS CHECKS 
# DHARMa 
## More DHARMa assumption checks 
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.of.ovi.2choice, plot = T)
## DHARMa assumption checks show the model to look a lot better 



# easystats check for overdispersion 
check_overdispersion(glmm.bin.of.ovi.2choice)
 ## No overdispersion detected 



## Doing an AIC check
AIC(glm.bin.of.ovi.2choice, glmm.bin.of.ovi.2choice)
 ## The model with random effects (Model 2) is a lot better, as expected from the other assumptions... 

## Using "Model 2" for now as I do not really know what else to do 


## Model choice: 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~ 
                                   ratio * block + (1|block/plate)  , family = binomial, data = df2_ovod1_oviposition)




# Testing for a 2-way interaction effect
drop1(glmm.bin.of.ovi.2choice, test = "Chisq")
 # There is a significant 2-way interaction effect



# The final chosen model... 
summary(glmm.bin.of.ovi.2choice)




############################## --
## Virgin Female Oviposition #### 
############################## --

## Model 1 

# Binomial model 
glm.bin.vf.ovi.2choice <- glm(cbind(Conditioned, Unconditioned) ~ 
                                ratio * block, family = binomial, data = df2_virgin_oviposition)




# Assumption checking 


## Testing homogeneity assumptions 
performance::check_model(glm.bin.vf.ovi.2choice, check = c("homogeneity"))
## there seems to be quite a lot of variation 


# DHARMa assumption checks 
## Doing more DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glm.bin.vf.ovi.2choice, plot = T)
 ## Things do not line up too great, model is pretty bad 


## easystats overdispersion checker 
check_overdispersion(glm.bin.vf.ovi.2choice)
## There is overdispersion





## Model 2
## a glm mixed model
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                                 ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_virgin_oviposition)

# Assumption checking 


## Homogeneity assumption checks 
performance::check_model(glmm.bin.vf.ovi.2choice, check = c("homogeneity"))
## there seems to be lots of variation in the points 


##  DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.vf.ovi.2choice, plot = T)
# The model checks look pretty good 


## easystats dispersion check
check_overdispersion(glmm.bin.vf.ovi.2choice) 
## No overdispersion detected 


## Comparing models
AIC(glm.bin.vf.ovi.2choice, glmm.bin.vf.ovi.2choice)
   ## The mixed model has lower AIC 



# Final chosen model: Binomial GLMM
## Using the mixed model for now 
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~
                                   ratio * block + (1|block/plate)  , family = binomial, data = df2_virgin_oviposition)


## Looking for significance in block
drop1(glmm.bin.vf.ovi.2choice, test = "Chisq") ## block is significant
 # Block is significant, keeping block in 



# Final chosen model:
summary(glmm.bin.vf.ovi.2choice)


