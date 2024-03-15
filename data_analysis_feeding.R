# Packages 📦📦📦📦
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
##################



#### Uploading the data sets
#### Part 1 
#### Uploading the data this way works for 4:1 and 1:4 assays being combined ####-

####################################-
#### WILD TYPE MALE CONDITIONING ### 
####################################-

## Creating a path to the scripts for treatment 2 condtioning
malepath <- "data/male_conditioning/treatment_2"


## This creates a function, that finds the path, excludes the 4-1 and 1-4 assay
# Mutates a variable for the data file name (id) 
# Mutates the data frame to have a variable for the amount of flies on a diet and the diet 

############################################################################################################----
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



### making a dataset without 1:4 
# Assuming "df2_male" is your dataset

# Use grepl to identify rows with "1-4" pattern in any column
pattern <- "1-4"
exclude_rows <- grepl(pattern, df2_male$id)

# Subset the dataframe to exclude rows with the "1-4" pattern
df2_male_filtered <- df2_male[!exclude_rows, ]









-####################################-
  #### VIRGIN FEMALE CONDITIONING ### ----
-####################################-
  
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








-####################################-
  #### OvoD1 FEMALE CONDITIONING ### ----
-####################################-
  
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

df2_ovod1 # does it recognise condition from the long data? 










########################## Data Analysis!!!! ############################

##########
## MALE ##
##########

## Creating a data column where flies are not on the plate 
df2_male <- df2_male %>% mutate(no_flies = 10 - (Conditioned + Unconditioned)) ## This is currently not used in any of the models

## MODELS 

## MODEL 1
## A binomial model, not considering other "random" factors
## cbind() is used - the response variables are Conditioned and Unconditioned
glm.bin_m <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_male)

# ASSUMPTION CHECKING:
## Checking Residuals
glm.bin_m_residuals <- residuals(glm.bin_m, type = "pearson")

# pearson residual check
plot(glm.bin_m_residuals ~ fitted(glm.bin_m), ylab = "Pearson Residuals", xlab = "Fitted Values")
abline(h = 0, col = "red")

## MODEL CHECK for overdispersion
summary(glm.bin_m) # overdispersion


## MODEL 2 
# trying a mixed model, considers other "random" factors
glmer.mm_m <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate) + (1|observation) , family = binomial, data = df2_male)

## Assumption checks 

# Performance check of data using "easystats":
performance::check_model(glmer.mm_m, check = c("qq")) # qq looks pretty good 
performance::check_model(glmer.mm_m, check = c("homogeneity")) #?? 

## DHARMa performance checks
testDispersion(glmer.mm_m) 

# Model is okay to be used? 

# Using MODEL 2 for analysis? 

# Looking for significance in "block" 
summary(glmer.mm_m) 
drop1(glmer.mm_m, test = "Chisq")
# Block is not significant, so dropping from the model 

# MODEL 2.1
glmer.mm_m_2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate) + (1|observation) , family = binomial, data = df2_male)

## Looking at the results of the model 
summary(glmer.mm_m_2) # only 4:1 is significant? 

## Trying to look at the results of the model using emmeans()
emmeans::emmeans(glmer.mm_m_2, pairwise ~ ratio , random = ~ (1|plate) + (1|observation))
# Really want to look at Conditioned vs Unconditioned?
## Need to know how to interpret this model












###################
## VIRGIN FEMALE ##
###################

## Creating a data column where flies are not on the plate 
df2_virgin <- df2_virgin %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))
## This has not been used yet, maybe need to include it in a model somehow?
# Is it relevant?

## MODEL 1 
## A binomial model, not considering other "random" factors
glm.bin_vf <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glm.bin_vf, check = c("qq")) # Can't do easystats assumption checks?? 
performance::check_model(glm.bin_vf, check = c("homogeneity")) 

# Assumption checking 
summary(glm.bin_vf) # Model is overdispersed 

# Mixed model, considers other "random" factors
glmer.mm_vf <- glmer(cbind(Conditioned, Unconditioned) ~ ratio*block + (1|plate) +(1|observation), family = binomial, data = df2_virgin)

# Assumption checks 
performance::check_model(glmer.mm_vf, check = c("qq")) # qq looks pretty great
performance::check_model(glmer.mm_vf, check = c("homogeneity")) # what is going on? 

# DHARMa checks 
testDispersion(glmer.mm_vf) ## hmm - overdispersed?? 
simulationOutput_glmer.mm_vf <- simulateResiduals(fittedModel = glmer.mm_vf, plot = F)
plot(simulationOutput_glmer.mm_vf) # all these models look the samen to me 


# using this model 
glmer.mm_vf <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate) +(1|observation), family = binomial, data = df2_virgin)

# looking for significance in block 
drop1(glmer.mm_vf, test = "Chisq") # block is not significant, can be dropped from the model


# Model 2 - where block has been removed
glmer.mm_vf_2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate) +(1|observation), family = binomial, data = df2_virgin)


summary(glmer.mm_vf_2) # 4:1 is not significant?

emmeans::emmeans(glmer.mm_vf_2, pairwise ~ ratio, random = ~ 1 | plate + observation)







##################--
## OVOD1 FEMALE ##
##################--
## creating a data column where flies are not on the plate 
df2_ovod1 <- df2_ovod1 %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))

## trying a binomial model
glm.bin_of <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block , family = binomial, data = df2_ovod1)

# assumption checks for binomial model 
summary(glm.bin_of) # there is overdispersion 


# mixed model, considers other "random" factors
glmer.mm_of <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block  + (1|plate) + (1|observation), family = binomial, data = df2_ovod1)


# assumption checking 

# easystats
performance::check_model(glmer.mm_of, check = c("qq")) # looks pretty good
performance::check_model(glmer.mm_of, check = c("homogeneity")) # I think looks okay 

# DHARMa checks 
testDispersion(glmer.mm_of) # not like the others? 

simulationOutput_glmer.mm_of <- simulateResiduals(fittedModel = glmer.mm_of, plot = F)
plot(simulationOutput_glmer.mm_of) # all looks the same to me? 



# using this model
glmer.mm_of <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block  + (1|plate) + (1|observation), family = binomial, data = df2_ovod1)

# looking at the significance of block
drop1(glmer.mm_of, test = "Chisq") # block is significant, keep in the model

glmer.mm_of_2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio   + (1|plate) + (1|observation), family = binomial, data = df2_ovod1)

summary(glmer.mm_of_2)

# checking out the model
summary(glmer.mm_of) # says block is not significant here? 





glmer.mm_of <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block  + (1|plate) + (1|observation), family = binomial, data = df2_ovod1)






################################################################################################################################################ 









#### Data Analysis Feeding 

## For the 4:1/1:4 Assay only 


## MALE ## 
#### UPLOADING AND BINDING THE CORRECT DATA
# 4:1 + 1:4 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b2.xlsx")

# mutating a variable for block 
fourone_onefour_male_b1 <- fourone_onefour_male_b1  %>% mutate(block = "one")
fourone_onefour_male_b2 <- fourone_onefour_male_b2  %>% mutate(block = "two")

# binding the data
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)


# 4:1 and 1:4 - making the data long 
combined_m <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Doing poisson to begin with 
glm_poisson_m <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_m)

# doing assumption checks before seeing if overdispersion can be checked
performance::check_model(glm_poisson_m, check = c("qq")) # qq looks ok?
performance::check_model(glm_poisson_m, check = c("outliers")) # weird 
performance::check_model(glm_poisson_m, check = c("homogeneity")) # does this look okay maybe? 


# checking for overdispersion
summary(glm_poisson_m) # A bit overdispersed 

# Look for 0s 
check_zeroinflation(glm_poisson_m)
# there is zero inflation 

# There is overdispersion, this could be independent of or because of the zero inflation
# so trying negative binomial family 
## trying to look for significance of experiment with a negative binomial model

# negative binomial model
glm.nb_m <- glm.nb(fly_numbers ~ diet * block, data = combined_m)

# assumption checks for negative binomial model
# easystats
performance::check_model(glm.nb_m, check = c("qq")) # looks the same as glm poisson. 
performance::check_model(glm.nb_m, check = c("outliers")) # weird 
performance::check_model(glm.nb_m, check = c("homogeneity")) # looks the same again 

# checking for overdispersion 
summary(glm.nb_m)

# no evidence of overdispersion but still highly skewed residuals do random effects help?

# trying a mixed model with GLM and poisson 
glm_mm_m <- glmmTMB(fly_numbers ~ diet * block +(1|factor(block)/plate) + (1|observation), family = poisson, data = combined_m)

performance::check_model(glm_mm_m, check = c("qq")) # qq looks a lot better

check_zeroinflation(glm_mm_m) # there is still zero inflation 

# usind DHARMa to look at residuals of new model 
simulateResiduals(fittedModel = glm_mm_m, plot = T)


# trying a zero inflated poisson model with poisson, and mixed model 
zi.p_m <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_m)


# trying a zero inflated negative binomial model 
zi.nb_m <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_m)

# comparing AIC of the different models 
AIC(glm_poisson_m, glm.nb_m, glm_mm_m, zi.p_m, zi.nb_m)
# glm_mm_m has the lowest AIC, but AIC values close to each other are basically the same (like 5 values?)

# check residuals of zip

sresid <- residuals(zi.p_m, type = "pearson")
pred <- fitted(zi.p_m)
hist(sresid)
plot(sresid ~  pred)




# looking at the results
summary(zi.p_m) # says block is no significant 
summary(glm_mm_m) # says block is not significant 

# dropping block from the model (zero inflation poisson)
zi.p_m_2 <- zeroinfl(fly_numbers ~ diet | diet , dist = "poisson", link = "logit", data = combined_m)

# dropping block from the model (mixed model)
glm_mm_m_2 <- glmmTMB(fly_numbers ~ diet  + (1|plate) + (1|observation), family = poisson, data = combined_m)


# checking the results of the model
summary(glm_mm_m_2) # everything is significant? 
emmeans::emmeans(glm_mm_m_2, pairwise ~ diet, random = ~ (1|plate) + (1|observation))



# Virgin Female Assay 

fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin.xlsx")
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")
fourone_onefour_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx")


# mutating a block variable 
fourone_onefour_virgin_b1 <- fourone_onefour_virgin_b1  %>% mutate(block = "one")
fourone_onefour_virgin_b2 <- fourone_onefour_virgin_b2  %>% mutate(block = "two")
fourone_onefour_virgin_b3 <- fourone_onefour_virgin_b3  %>% mutate(block = "three")
fourone_onefour_virgin_b4 <- fourone_onefour_virgin_b3  %>% mutate(block = "four")

# Binding the data
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2, fourone_onefour_virgin_b3, fourone_onefour_virgin_b4 )
# Making the data long
combined_vf <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# glm with poisson
glm_poisson_vf <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_vf)


# assumption checking
performance::check_model(glm_poisson_vf, check = c("qq")) # almost banana - not great 
performance::check_model(glm_poisson_vf, check = c("homogeneity")) # looks okay ? 

# looking for overdispersion in the model
summary(glm_poisson_vf) # shows slight overdispersion 

# overdispersion so checking for zero inflation
check_zeroinflation(glm_poisson_vf) # no zero inflation?? 




# trying quasipoisson
glm_quasipoisson_vf <- glm(fly_numbers ~ diet * block, family = quasipoisson, data = combined_vf)

# assumption checking
performance::check_model(glm_quasipoisson_vf, check = c("qq")) # almost banana - not great - looks same 
performance::check_model(glm_quasipoisson_vf, check = c("homogeneity")) # looks okay ? 



# trying a negative binomial model
glm.nb_vf <- glm.nb(fly_numbers ~ diet * block, data = combined_vf)


performance::check_model(glm.nb_vf, check = c("qq")) # qq looks slighty better? still off 
performance::check_model(glm.nb_vf, check = c("homogeneity")) # maybe the same 

summary(glm.nb_vf)  # a lot less overdispersion 

# trying a mixed model 
glm_mm_vf <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_vf)

performance::check_model(glm_mm_vf, check = c("qq")) # looks a lot better, slopes off at the end though

# performance checking with DHARMa
simulateResiduals(fittedModel = glm_mm_vf, plot = T)






# Trying zero inflated models - don't need as do not have zero inflation? 

# zero inflated poisson 
zi.p_vf <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_vf)

# assumption checks?? 


# zero inflated negative binomia; 
zi.nb_vf <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_vf)

# assumption checks?? 

AIC(glm_poisson_vf, glm_quasipoisson_vf, glm.nb_vf, glm_mm_vf) # maybe choose glm with negative binomial? 

# choosing glm with nb for now 
glm.nb_vf <- glm.nb(fly_numbers ~ diet * block, data = combined_vf)

# looking at significance of block 
drop1(glm.nb_vf, test = "F")
summary(glm.nb_vf)

# block is not significant, can be dropped 


glm.nb_vf_2  <- glm.nb(fly_numbers ~ diet, data = combined_vf)

summary(glm.nb_vf_2)
emmeans::emmeans(glm.nb_vf_2, pairwise ~ diet)
# conditioning in 1:4 not significant. 










# OvoD1 Conditioning 4:1-1:4 analysis -- 
# mutating a block variable 
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")


fourone_onefour_ovod1_b1 <- fourone_onefour_ovod1_b1  %>% mutate(block = "one")
fourone_onefour_ovod1_b2 <- fourone_onefour_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)

# Making the data long
combined_of <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# GLM with poisson 
glm_poisson_of <- glm(fly_numbers ~ diet * block, family = poisson, data = combined_of)

# doing model checks 
performance::check_model(glm_poisson_of, check = c("qq")) # doesn't look great - banana 
performance::check_model(glm_poisson_of, check = c("homogeneity")) # slopey 


# checking for overdispersion
summary(glm_poisson_of) # there is overdispersion 

# overdispersion so checking for zero inflation
check_zeroinflation(glm_poisson_of) # there is zero inflation 

# Doing a negative binomial as there is zero inflation
glm.nb_of <- glm.nb(fly_numbers ~ diet * block, data = combined_of)

# checking the negative binomial glm 
performance::check_model(glm.nb_of, check = c("qq")) # still very slopey 

# using DHARMa to check 
testDispersion(glm.nb_of) # this is fairly dispersed 

simulationOutput_of  <- simulateResiduals(fittedModel = glm.nb_of, plot = F)

residuals(simulationOutput_of)
plot(simulationOutput_of)
testZeroInflation(simulationOutput_of)


# checking for overdispersion 
summary(glm.nb_of) # very little overdispersion 



# trying a mixed GLM 
glm_mm_of <- glmmTMB(fly_numbers ~ diet * block + (1|factor(block)/plate) + (1|observation), family = poisson, data = combined_of)

# performance checks 
performance::check_model(glm_mm_of, check = c("qq")) # qq looks a lot better, goes off at the end 

# using DHARMa to check 
testDispersion(glm_mm_of) # this is fairly dispersed 

simulationOutput_ofmm  <- simulateResiduals(fittedModel = glm_mm_of, plot = F)

residuals(simulationOutput_ofmm)
plot(simulationOutput_ofmm)
testZeroInflation(simulationOutput_ofmm)




# ZERO INFLATED MODELS 

# trying a zero inflated poisson model 
zi.p_of <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = combined_of)

# Using DHARMa to check 
# cannot run? 




# trying a zero inflated negative binomial model 
zi.nb_of <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = combined_of )

# need to do performance checks? 




# comparing models 
AIC(glm_poisson_of, glm.nb_of, glm_mm_of, zi.p_of, zi.nb_of)

# choosing negative binomial? 

# looking for significance in block 
summary(zi.nb_of) # not significant? 


# dropping block from the model 
zi.p_of <- zeroinfl(fly_numbers ~ diet  | diet, dist = "negbin", link = "logit", data = combined_of )

# looking at the results 
summary(zi.p_of)

emmeans::emmeans(zi.p_of, pairwise ~ diet )

