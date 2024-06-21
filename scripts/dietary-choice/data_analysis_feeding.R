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
    str_detect(id, "t2b1") ~ "one",
    str_detect(id, "t2b2") ~ "two",
  
  ))


# Separate diet column and group by relevant variables
df2_male <- df_male %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%
  group_by(id, observation, plate, ratio, condition, block) %>%
  summarise(count = sum(fly_numbers)) %>%
  pivot_wider(names_from = "condition", values_from = "count")

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



## Looking at the results of the model 
summary(glmm.bin.m.2) # only 4:1 is significant? 


## Trying to look at the results of the model using emmeans()
emmeans::emmeans(glmm.bin.m.2, pairwise ~ ratio , random = ~ (1|plate) + (1|observation))
# Really want to look at Conditioned vs Unconditioned?
## Need to know how to interpret this model












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











################################################################################################################################################-- 



##### Data Analysis Part 2 (4:1/1:4) ####

## For the 4:1/1:4 Assay only 


## MALE ####


#### UPLOADING AND BINDING THE CORRECT DATA
# 4:1 + 1:4 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_t2b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_t2b2.xlsx")

# mutating a variable for block 
fourone_onefour_male_b1 <- fourone_onefour_male_b1  %>% mutate(block = "one")
fourone_onefour_male_b2 <- fourone_onefour_male_b2  %>% mutate(block = "two")

# Binding the data
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)


# 4:1 and 1:4 - making the data long 
combined_m <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")




## Data Analysis 

## Doing poisson to begin with 
glm.pois.m.4choice <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_m)

# Doing assumption checks before seeing if overdispersion can be checked
performance::check_model(glm.pois.m.4choice, check = c("qq")) # qq looks ok?
performance::check_model(glm.pois.m.4choice, check = c("outliers")) # weird 
performance::check_model(glm.pois.m.4choice, check = c("homogeneity")) # does this look okay maybe? 


# Checking for overdispersion
summary(glm.pois.m.4choice) # A bit overdispersed 

# Look for 0s 
check_zeroinflation(glm.pois.m.4)
# there is zero inflation 

# There is overdispersion, this could be independent of or because of the zero inflation
# So trying negative binomial family 
## Trying to look for significance of experiment with a negative binomial model




# Negative binomial model
glm.nb.m.4choice <- glm.nb(fly_numbers ~ diet * block, data = combined_m)

# assumption checks for negative binomial model
# easystats
performance::check_model(glm.nb.m.4choice, check = c("qq")) # looks the same as glm poisson. 
performance::check_model(glm.nb.m.4choice, check = c("outliers")) # weird 
performance::check_model(glm.nb.m.4choice, check = c("homogeneity")) # looks the same again 

# Checking for overdispersion 
summary(glm.nb.m.4choice)
   # No evidence of overdispersion but still highly skewed residuals, do random effects help?





    # Trying a mixed model with GLM and poisson 
glmm.m.4choice <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_m)

## Assumption checks
performance::check_model(glmm.m.4, check = c("qq")) # qq looks a lot better

## Looking for inflation of zeros
check_zeroinflation(glmm.m.4) # There is still zero inflation 

# using DHARMa to look at residuals of new model 
simulateResiduals(fittedModel = glmm.m.4, plot = T)

# Trying a zero inflated poisson model with poisson
zi.pois.m.4choice <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_m)

## Assumption checks 
sresid <- residuals(zi.pois.m.4, type = "pearson")
pred <- fitted(zi.pois.m.4)
hist(sresid)
plot(sresid ~  pred)


# Trying a zero inflated negative binomial model 
zi.nb.m.4choice <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_m)

 


# Comparing AIC of the different models 
AIC(glm.pois.m.4choice, glm.nb.mchoice, glmm.m.4choice, zi.pois.m.4choice, zi.nb.m.4choice)
# glm_mm_m has the lowest AIC, but AIC values close to each other are basically the same (like 5 values?)






# This model to be chosen:
glmm.m.4choice <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_m)

## Testing for an interaction effect 
drop1(glmm.m.4, test = "Chi") # Small interaction, but still exists 



# With the chosen model, splitting up "diet" into ratio and condition.

## Splitting up diet within the actual data frame
combined_m <- combined_m %>%
     separate(diet, into = c("ratio", "condition"), sep = " ")


## Testing for interaction effects with the new model
glmm.m.4choice.2 <- glmmTMB(fly_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m)

## This will show results for the 3-way interaction
drop1(glmm.m.4choice.2, test = "Chisq")

## This shows everything
summary(glmm.m.4choice.2)

## Results show there is no 3-way interaction, so this can be removed
glmm.m.4choice.3 <- glmmTMB(fly_numbers ~  ratio * condition + ratio * block + condition * block + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m)

drop1(glmm.m.4choice.2, test = "Chisq") ## An interaction between condition and block found? the others can be dropped

## Newest model - final model 
glmm.m.4choice.4 <- glmmTMB(fly_numbers ~  ratio + condition * block + (1 | factor(block) / plate) + (1 | observation), family = poisson, data = combined_m)


## Analysis of the results
summary(glmm.m.4choice.4)

## Tukey test pairwise 
emmeans::emmeans(glm_mm_m_3, pairwise ~  ratio + condition)






# VIRGIN FEMALE ####

#### Reading in the data
fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx")
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")
fourone_onefour_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx")


## Mutating a block variable 
fourone_onefour_virgin_b1 <- fourone_onefour_virgin_b1  %>% mutate(block = "one")
fourone_onefour_virgin_b2 <- fourone_onefour_virgin_b2  %>% mutate(block = "two")
fourone_onefour_virgin_b3 <- fourone_onefour_virgin_b3  %>% mutate(block = "three")
fourone_onefour_virgin_b4 <- fourone_onefour_virgin_b3  %>% mutate(block = "four")

# Binding the data
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2, fourone_onefour_virgin_b3, fourone_onefour_virgin_b4)


# Making the data long
combined_vf <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


#### DATA ANALYSIS 


# GLM with with poisson
glm.pois.vf.4choice <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_vf)


# Assumption checking
performance::check_model(glm.poisson.vf.4choice, check = c("qq")) # almost banana - not great 
performance::check_model(glm.poisson.vf.4choice, check = c("homogeneity")) # looks okay ? 

# looking for overdispersion in the model
summary(glm.poisson.vf.4choice) # shows slight overdispersion 

# overdispersion so checking for zero inflation
check_zeroinflation(glm.poisson.vf.4choice) # No zero inflation?? 


# Trying quasipoisson
glm.quasipoisson.vf.4choice <- glm(fly_numbers ~ diet * block, family = quasipoisson, data = combined_vf)

# Assumption checking
performance::check_model(glm.quasipoisson.vf.4choice, check = c("qq")) # almost banana - not great - looks same 
performance::check_model(glm.quasipoisson.vf.4choice, check = c("homogeneity")) # looks okay ? 




# Trying a negative binomial model to compare more models
glm.nb.vf.4choice <- glm.nb(fly_numbers ~ diet * block + (1|plate) + (1|observation), data = combined_vf)

## Assumption checks
performance::check_model(glm.nb.vf.4choice, check = c("qq")) # qq looks slighty better? still off 
performance::check_model(glm.nb.vf.4choice, check = c("homogeneity")) # maybe the same 

## Checking for overdispersion
summary(glm.nb.vf.4choice)  # a lot less overdispersion 



# Trying a generalised linear mixed model to compare more models
glmm.vf.4choice <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_vf)

## Assumption checks
performance::check_model(glmm.vf.4choice, check = c("qq")) # looks a lot better, slopes off at the end though


# Assumption checking with DHARMa
simulateResiduals(fittedModel = glmm.vf.4choice , plot = T)




## Checking the AIC of all the tested models 
AIC(glm.pois.vf.4choice, glm.quasipoisson.vf.4choice, glm.nb.vf.4choice, glmm.vf.4choice) 
    #### Negative Binomial GLM has lowest AIC (slightly), go with this for now as assumption checks were also ok? 


## Chosen model 
glm.nb.vf.4choice <- glm.nb(fly_numbers ~ diet * block + (1|plate) + (1|observation), data = combined_vf)


# Splitting diet up with chosen model

##  Using separate to split diet up into ratio and condition
combined_vf_split <- combined_vf %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


## The new model with diet split

## Trying with multiple, 3-way interactions
glm.nb.vf.4choice.2  <- glm.nb(fly_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block + (1|plate) + (1|observation), data = combined_vf_split)

## Testing for interaction
drop1(glm.nb.vf.4choice.2, test = "Chisq") 
   ## No interaction effect, 3-way interaction can be dropped from the model. 

## Tests two-way interactions 
glm.nb.vf.4choice.3  <- glm.nb(fly_numbers ~  ratio * condition + ratio * block + condition * block + (1|plate) + (1|observation), data = combined_vf_split)

## Finds interaction effects 
drop1(glm.nb.vf.4choice.3, test = "Chisq")  
     ## An interaction effect of condition and block found 

# Final model
glm.nb.vf.4choice.4  <- glm.nb(fly_numbers ~  ratio + condition * block + (1|plate) + (1|observation), data = combined_vf_split)

## To show interaction effects
drop1(glm.nb.vf.4choice.4, test = "Chisq")  


## Using the final model for analysis 
summary(glm.nb.vf.4choice.4)

## Two-way tukey test 
emmeans::emmeans(glm.nb_vf_2, pairwise ~ ratio + condition)

## Finding response variable for written analysis 
emmeans::emmeans(glm.nb_vf_2, ~ diet, type = "response")
# conditioning in 1:4 not significant. 












# OVOD1 (EGGLESS) FEMALE ####
 # Absolute Assay Analysis #


## Reading the data in
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")

# Mutating a block variable 
fourone_onefour_ovod1_b1 <- fourone_onefour_ovod1_b1  %>% mutate(block = "one")
fourone_onefour_ovod1_b2 <- fourone_onefour_ovod1_b2  %>% mutate(block = "two")

# Binding the separate blocks 
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)

## Cleaning the data, adding relevant variables
combined_of <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



#### DATA ANALYSIS


# GLM with poisson 
glm.pois.of.4choice <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_of)

# Assumption checks of model
performance::check_model(glm.pois.of.4choice, check = c("qq")) # doesn't look great - banana 
performance::check_model(glm.pois.of.4choice, check = c("homogeneity")) # slopey 

# Checking for overdispersion
summary(glm.pois.of.4choice) # There is overdispersion 

# Overdispersion is found
# Checking for zero inflation
check_zeroinflation(glm.pois.of.4choice) # There is zero inflation 




# Doing a negative binomial as there is overdispersion
glm.nb.of.4choice <- glm.nb(fly_numbers ~ diet * block, data = combined_of)

# Checking the Negative Binomial GLM 
performance::check_model(glm.nb.of.4choice, check = c("qq")) # still very slopey 

# using DHARMa to check 
testDispersion(glm.nb.of.4choice) # this is fairly dispersed 

simulationOutput_of  <- simulateResiduals(fittedModel = glm.nb.of.4choice, plot = F)

residuals(simulationOutput_of)
plot(simulationOutput_of)
testZeroInflation(simulationOutput_of)


# Checking for overdispersion 
summary(glm.nb.of.4choice) # very little overdispersion 





# Trying a mixed GLMM, to consider random effects
glmm.of.4choice <- glmmTMB(fly_numbers ~ diet + block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_of)

## Checking for overdispersion 
check_overdispersion(glmm.of.4choice)
   ## Overdispersion detected 

## More assumption checks 
performance::check_model(glmm.of.4choice, check = c("qq")) 
    # Currently won't run, 21/06/24



# using DHARMa for assumption checks
testDispersion(glmm.of.4choice) # This is fairly overdispersed 

simulationOutput_glmm.of.4choice <- simulateResiduals(fittedModel = glmm.of.4choice, plot = F)

residuals(simulationOutput_glmm.of.4choice)
plot(simulationOutput_glmm.of.4choice)
testZeroInflation(simulationOutput_glmm.of.4choice)




# ZERO INFLATED MODELS - zeroinflation has been found, so trying zeroinflated models.

# Trying a zero inflated poisson model 
zi.p.of.4choice <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_of)


# trying a zero inflated - negative binomial model 
zi.nb.of.4choice <- zeroinfl(fly_numbers ~ diet + block , dist = "negbin", link = "logit", data = combined_of )


    #### For the zeroinflated models, I don't know how to do performnance checks. 



# Comparing all the models together 
AIC(glm.pois.of.4choice, glm.nb.of.4choice, glmm.of.4choice, zi.p.of.4choice, zi.nb.of.4choice)

  #### Choosing Zero Inflatted Poisson for now 


#### Splitting variables in the model up: 
combined_of_split <- combined_of_split %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")


## New model with all interactions
zi.p.of.4choice.2 <- zeroinfl(fly_numbers ~ ratio * condition * block + ratio * condition + ratio * block + condition * block, dist = "negbin", link = "logit", data = combined_of )

# Testing for interaction effect
drop1(zi.p.of.4choice.2, test = "Chisq") ## No 3-way interaction effect 

# New model without a 3-way interaction
zi.p.of.4choice.3 <- zeroinfl(fly_numbers ~  ratio * condition + ratio * block + condition * block, dist = "negbin", link = "logit", data = combined_of )

## Testing for 2-way interaction effects
drop1(zi.p.of.4choice.3, test = "Chisq") 
 ## Interaction effect found between condition and block, and ratio and condition 

## Final model?
zi.p.of.4choice.4 <- zeroinfl(fly_numbers ~  ratio * condition  + condition * block , dist = "negbin", link = "logit", data = combined_of )

## Testing for remaining interaction effects 
drop1(zi.p.of.4choice.4, test = "Chisq") 

## Using model for analysis
summary(zi.p.of.4choice.4)

## Tukey test pairwise 
emmeans::emmeans(zi.p.of.4choice.4, pairwise ~ ratio + condition )

