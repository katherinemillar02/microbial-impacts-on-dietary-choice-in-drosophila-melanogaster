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



#### Reading the data in: 
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

## The data set 
df2_male









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


## MODELS 

# MODEL 1
# Binomial GLM
## A binomial model, not considering other "random" factors
## cbind() is used - the response variables are Conditioned and Unconditioned
glm.bin.m <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_male)

# ASSUMPTION CHECKING:
## Checking Residuals
glm.bin.m.residuals <- residuals(glm.bin.m, type = "pearson")

# pearson residual check
plot(glm.bin.m.residuals ~ fitted(glm.bin.m), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")

## MODEL CHECK for overdispersion
summary(glm.bin.m) # overdispersion




## MODEL 2 
# Binomial GLMM
# trying a mixed model, considers other "random" factors
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) 
                    ~ ratio * block
                    + (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)


## Assumption checks 

# Performance check of data using "easystats":
performance::check_model(glmm.bin.m, check = c("homogeneity")) #?? 

## DHARMa performance checks
testDispersion(glmm.bin.m) 


# easystats checks 
check_overdispersion(glmm.bin.m)
  # No overdispersion


check_zeroinflation(glmm.bin.m)
 # cannot check zero inflation 


## zero inflation test 
fittedModel <- glmmTMB(cbind(Conditioned, Unconditioned)   ~ ratio * block + (1|block/plate) + (1|block/observation), ziformula = ~1 , family = "binomial", data = df2_virgin)
summary(fittedModel)
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") 
# No zero inflation? 


## zero inflation test 
fittedModel <- glmmTMB(cbind(Conditioned, Unconditioned)   ~ ratio * block + (1|block/plate) + (1|block/observation), ziformula = ~1 , family = "binomial", data = df2_male)
summary(fittedModel)
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") 
# No zero inflation? 



# DHARMa checks 
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.m, plot = T)
   # Model looks pretty good



 

## AIC Check of models 
AIC(glm.bin.m, glmm.bin.m)
    ## Binomial GLMM has a slighty lower AIC

# Using MODEL 2 (Binomial GLMM) for analysis? 
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) 
                    ~ ratio * block
                    + (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)


# two-way interaction test
drop1(glmm.bin.m, test = "Chisq")
   # No two-way interaction

# MODEL 2.1 
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                        ratio + block +
                         (1|block/plate) + (1|block/ observation) , family = binomial, data = df2_male)


## Looking at the results of the model 
summary(glmm.bin.m.2) 








###################--
## VIRGIN FEMALE ####
###################--

## MODEL 1 
## A binomial model, not considering other "random" factors
glm.bin.vf <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glm.bin.vf , check = c("homogeneity")) 

# Assumption checking 
summary(glm.bin.vf) ## There is overdispersion





## MODEL 2 
# Mixed model, considers other "random" factors
glmm.bin.vf  <- glmer(cbind(Conditioned, Unconditioned)
                      ~ ratio * block + (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glmm.bin.vf, check = c("homogeneity")) # what is going on? looks ok 

# DHARMa checks 
simulationOutput_glmm.bin.vf <- simulateResiduals(fittedModel = glmm.bin.vf, plot = T)
  # Model looks pretty good

# easystats checks
check_overdispersion(glmm.bin.vf)
  # No overdispersion detected 


check_zeroinflation(glmm.bin.vf)
 # cannot check... 


## Doing AIC check 
AIC(glm.bin.vf,glmm.bin.vf) 
# The Binomial GLMM has slightly higher AIC, choosing this  



## Using Binomial GLMM for analysis
glmm.bin.vf  <- glmer(cbind(Conditioned, Unconditioned) ~ 
                        ratio * block +
                        (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)

# Looking for two-way interaction
drop1(glmm.bin.vf, test = "Chisq") # No interaction effect between ratio and block
 # No two way interaction

# Model 2.1 - where block has been removed...
glmm.bin.vf.2 <- glmer(cbind(Conditioned, Unconditioned) 
                       ~ ratio + block +  (1|block/plate) + (1|block/observation), family = binomial, data = df2_virgin)


# Finding results from the model
summary(glmm.bin.vf.2) 














##################--
## OVOD1 FEMALE ####
##################--

## Model 1- Trying a binomial model
glm.bin.of <- glm(cbind(Conditioned, Unconditioned) ~
                    ratio * block , family = binomial, data = df2_ovod1)

# Assumption checks for binomial model 
summary(glm.bin.of) # There is overdispersion? 







## Model 2 
# Mixed model, considers other "random" factors
glmm.bin.of <- glmer(cbind(Conditioned, Unconditioned) 
                     ~ ratio * block  + (1|block/plate) + (1|block/observation), family = binomial, data = df2_ovod1)


# Assumption checking 

# easystats
performance::check_model(glmm.bin.of, check = c("homogeneity")) # I think looks okay, bit slopey down 

# DHARMa checks 
simulationOutput_glmm.bin.of <- simulateResiduals(fittedModel = glmm.bin.of, plot = T)
  # Assumptions look pretty good


## zero inflation test 
fittedModel <- glmmTMB(cbind(Conditioned, Unconditioned)   ~ ratio * block + (1|block/plate) + (1|block/observation), ziformula = ~1 , family = "binomial", data = df2_ovod1)
summary(fittedModel)
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes, alternative = "greater") 
 # No zero inflation? 



check_overdispersion(glmm.bin.of)
  # No overdispersion detected

## Doing AIC checks 
AIC(glm.bin.of, glmm.bin.of)
 ## Mixed model has lower AIC, and it considers random effects, so stucking with this

# Using this model
glmm.bin.of <- glmer(cbind(Conditioned, Unconditioned) ~
                       ratio * block  + (1|block/plate) + (1|block/observation), family = binomial, data = df2_ovod1)


# Looking at the significance of block
drop1(glmm.bin.of, test = "Chisq") 
 # block is significant, keep in the model


## Analysis of glmm.bin.of
summary(glmm.bin.of)



