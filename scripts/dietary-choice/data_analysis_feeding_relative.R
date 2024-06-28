# Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
##################---



#### Uploading the data sets
#### Part 1 
#### Uploading the data this way works for 4:1 and 1:4 assays being combined ####-

####################################-
#### WILD TYPE MALE CONDITIONING ####
####################################-

## Creating a path to the scripts for treatment 2 condtioning
malepath <- "data/male_conditioning"


## This creates a function, that finds the path, excludes the 4-1 and 1-4 assay
# Mutates a variable for the data file name (id) 
# Mutates the data frame to have a variable for the amount of flies on a diet and the diet 

############################################################################################################---
read_raw_male <- function(path = malepath, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = malepath,
                              pattern = "rawdata", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}


############################################################################################################-






## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male(malepath) # this will show the new data set 


## creating an actual data set that will read the paths
# first data frame - purr package 
df_male <- malepath %>% 
  map_df(~read_raw_male(.x)) #.x is a string or NULL - only applies to dfr apparently
# this will actually give it a label 

# mutating a variable for block from the data id 
df_male <- df_male %>%
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    
  ))

# Separate diet column and group by relevant variables
df2_male <- df_male %>%
  separate(diet, into = c("ratio", "treatment"), sep = " ") %>%
  group_by(id, observation, plate, ratio, treatment, block) %>%
  summarise(count = sum(fly_numbers)) %>%
  pivot_wider(names_from = "treatment", values_from = "count") 

## test
df2_male_test <- df_male %>%
  separate(diet, into = c("ratio", "treatment"), sep = " ") %>%
  group_by(id, observation, plate, ratio, treatment, block) %>%
  summarise(count = sum(fly_numbers), .groups = 'drop') %>%
  mutate(condition = treatment) %>%
  pivot_wider(names_from = "treatment", values_from = "count")


## The data set 
df2_male


###########################################################################---
###  using the 4:1 and 1:4 dataset and making a dataset without 1:4 


# Use grepl to identify rows with "1-4" pattern in any column
one_four <- "1-4"
exclude_onefour <- grepl(one_four, df2_male$id)

# Subset the dataframe to exclude rows with the "1-4" pattern
df2_male_exclude_onefour <- df2_male[!exclude_onefour, ]


# Use grepl to identify rows with "1-4" pattern in any column
fourone <- "4-1"
exclude_fourone <- grepl(fourone, df2_male$id)

# Subset the dataframe to exclude rows with the "1-4" pattern
df2_male_exclude_fourone <- df2_male[!exclude_fourone, ]

###########################################################################---




####################################-
#### VIRGIN FEMALE CONDITIONING  ----
####################################-

## Creating a path to get to the Virgin Conditioning data files 
pathvirgin <- "data/female_conditioning/virgin"

## This creates a function ## 
# Reads into the path where the Virgin - Conditioning data files are 
# Creates a pattern to exclude data files that are 4:1 and 1:4 in a four choice assay 
# Pivots a data frame, to include fly numbers and diet, and excludes na where there would not be numbers in the data frame


############################################################################################################-
read_raw_virgin <-function(path = pathvirgin, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathvirgin,
                              pattern = "rawresults", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}
############################################################################################################-


## read_raw is the function created, and path shows the path made, so the list of files
read_raw_virgin(pathvirgin) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin <- pathvirgin %>% 
  map_df(~read_raw_virgin(.x)) #.x is a string or NULL - only applies to dfr apparently

# Mutating a variable for block, using the 'id' of the different datasets
df_virgin <- df_virgin %>% 
  mutate(block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two",
    str_detect(id, "b3") ~ "three",
    str_detect(id, "b4") ~ "four"
  ))


## This separatess diet into ratio and condition as these are currently joined 
df2_virgin <- df_virgin %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>% # separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition, block) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

# The data frame 
df2_virgin 








###################################-
#### OvoD1 FEMALE CONDITIONING ----
####################################-

pathovod1 <- "data/female_conditioning/ovod1"

## This creates  function
## Path is interchangeable with path 2 
read_raw_ovod1 <-function(path = pathovod1, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathovod1,
                              pattern = "rawresults", full.names = T)
  
  list_of_files <- list_of_files[!grepl(pattern_to_exclude, list_of_files)]
  
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovod1(pathovod1) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovod1 <- pathovod1 %>% 
  map_df(~read_raw_ovod1(.x)) #.x is a string or NULL - only applies to dfr apparently

# Adding block variables
df_ovod1 <- df_ovod1 %>% 
  mutate( block = case_when(
    str_detect(id, "b1") ~ "one",
    str_detect(id, "b2") ~ "two"
  ))

# uses what was generated with "df"
df2_ovod1 <- df_ovod1 %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition, block) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")



















########################## Data Analysis Part 1 (4:1 + 1:4) !!!! ############################

##########--
## MALE ####
##########--

## Creating a data column where flies are not on the plate - possibly analysis to consider at some point?
df2_male <- df2_male %>% mutate(no_flies = 10 - (Conditioned + Unconditioned)) ## This is currently not used in any of the models

## MODELS 

## MODEL 1
## A binomial model, not considering other "random" factors
## cbind() is used - the response variables are Conditioned and Unconditioned
glm.bin.m <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_male)

# ASSUMPTION CHECKING:
## Checking Residuals
glm.bin.m.residuals <- residuals(glm.bin_m, type = "pearson")

# pearson residual check
plot(glm.bin.m.residuals ~ fitted(glm.bin.m), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")

## MODEL CHECK for overdispersion
summary(glm.bin.m) # overdispersion


## MODEL 2 
# trying a mixed model, considers other "random" factors
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate) + (1|observation) , family = binomial, data = df2_male)


## Assumption checks 

# Performance check of data using "easystats":
performance::check_model(glmm.bin.m, check = c("qq")) # qq looks pretty good 
performance::check_model(glmm.bin.m, check = c("homogeneity")) #?? 

## DHARMa performance checks
testDispersion(glmm.bin.m) 

# Model is okay to be used? 

## AIC Check of models 
AIC(glm.bin.m, glmm.bin.m)

## Binomial GLMM has a slighty lower AIC

# Using MODEL 2 (Binomial GLMM) for analysis? 

# Looking for significance in "block" 
summary(glmm.bin.m) 
drop1(glmm.bin.m, test = "Chisq")
# Block is not significant, so dropping from the model 

# MODEL 2.1 
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block + (1|plate) + (1|observation) , family = binomial, data = df2_male)

## playing around
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * condition + block + (1|plate) + (1|observation) , family = binomial, data = df2_male_test)
  ## don't get the error, was not there before, and plate seems to be in the df. 


## Looking at the results of the model 
summary(glmm.bin.m.2) # only 4:1 is significant? 


## Trying to look at the results of the model using emmeans()
emmeans::emmeans(glmm.bin.m.2, pairwise ~ ratio , random = ~ (1|plate) + (1|observation))
# Really want to look at Conditioned vs Unconditioned?
## Need to know how to interpret this model



## TRYING MODELS THAT CONSIDER INTERACTION EFFECTS
glm.bin.m.01 <- glm(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned * block , family = binomial, data = df2_male)

glmm.bin.m.01 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned * block + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)


AIC(glm.bin.m.01, glmm.bin.m.01)
  ## It says Bin GLM is better than Bin GLMM? 

## Confused about what to pick, going for the mixed model for now? 




###################--
## VIRGIN FEMALE ####
###################--

## Creating a data column where flies are not on the plate 
df2_virgin <- df2_virgin %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))
## This has not been used yet, maybe need to include it in a model somehow?
# Is it relevant?

## MODEL 1 
## A binomial model, not considering other "random" factors
glm.bin.vf <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glm.bin.vf , check = c("qq")) # Can't do easystats assumption checks?? 
performance::check_model(glm.bin.vf , check = c("homogeneity")) 

# Assumption checking 
summary(glm.bin.vf) ## There is overdispersion


## MODEL 2 
# Mixed model, considers other "random" factors
glmm.bin.vf  <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate) + (1|observation), family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glmm.bin.vf, check = c("qq")) # qq looks pretty great
performance::check_model(glmm.bin.vf, check = c("homogeneity")) # what is going on? 

# DHARMa checks 
testDispersion(glmm.bin.vf) ## overdispersed?? 
simulationOutput_glmm.bin.vf <- simulateResiduals(fittedModel = glmm.bin.vf, plot = F)
plot(simulationOutput_glmm.bin.vf) 

## Doing AIC check 
AIC(glm.bin.vf,glmm.bin.vf) 
# The Binomial GLMM has slightly higher AIC, but choosing this anyway as considers random factors 



## Using Binomial GLMM for analysis
glmm.bin.vf  <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate) +(1|observation), family = binomial, data = df2_virgin)

# Looking for significance in block 
drop1(glmm.bin.vf, test = "Chisq") # No interaction effect between ratio and block


# Model 2.1 - where block has been removed...
glmm.bin.vf.2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block +  (1|plate) +(1|observation), family = binomial, data = df2_virgin)

# Finding results from the model
summary(glmm.bin.vf.2) 

## Finding response variables for written analysis 
emmeans::emmeans(glmer.mm_vf_2, ~ ratio, random = ~ 1 | plate + observation, type = "response")




#### TRYING MODELS THAT CONSIDER INTERACTION EFFECTS, AND COMPARING
glmm.bin.m.01 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned * block + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)









##################--
## OVOD1 FEMALE ####
##################--
## Creating a data column where flies are not on the plate 
df2_ovod1 <- df2_ovod1 %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))

## Model 1- Trying a binomial model
glm.bin.of <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block , family = binomial, data = df2_ovod1)

# Assumption checks for binomial model 
summary(glm.bin.of) # There is overdispersion 


## Model 2 
# Mixed model, considers other "random" factors
glmm.bin.of <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block  + (1|plate/block) + (1|observation), family = binomial, data = df2_ovod1)


# Assumption checking 

# easystats
performance::check_model(glmm.bin.of, check = c("qq")) # looks pretty good
performance::check_model(glmm.bin.of, check = c("homogeneity")) # I think looks okay 

# DHARMa checks 
testDispersion(glmm.bin.of) # not like the others? 

simulationOutput_glmm.bin.of <- simulateResiduals(fittedModel = glmm.bin.of, plot = F)
plot(simulationOutput_glmer.mm_of) # all looks the same to me? 

## Doing AIC checks 
AIC(glm.bin.of, glmm.bin.of)
## Mixed model has lower AIC, and it considers random effects, so stucking with this

# Using this model
glmm.bin.of <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block  + (1|plate/block) + (1|observation), family = binomial, data = df2_ovod1)


# Looking at the significance of block
drop1(glmm.bin.of, test = "Chisq") 
# block is significant, keep in the model


## Analysis of glmm.bin.of
summary(glmm.bin.of)


## Finding the response variables from emmeans 
emmeans::emmeans(glmm.bin.of, ~ ratio, type = "response")





## How to analyse when block is significant, can't get rid of interaction effect. 


