# Packages
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)


#### DATA UPLOAD 

#### THIS DATA UPLOAD WORKS FOR 4:1 AND 1:4 COMBINING ####


#### MALE ----####----
# Analysing the raw data
## Creating a path to the scripts within block 1 and block 2 paths
malepath <- "data/male_conditioning/treatment_2"


## This creates  function
## Path is interchangeable with path 2 
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

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male(malepath) 



## creating an actual data set that will read the paths
# first data frame - purr package 
df_male <- malepath %>% 
  map_df(~read_raw_male(.x)) #.x is a string or NULL - only applies to dfr apparently

## "second" data frame
# uses what was generated with "df"
df2_male <- df_male %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

df2_male # does it recognise condition from the long data? 





#### VIRGIN FEMALE  ----
## Creating a path to the scripts within block 1 and block 2 paths
## use for when you get both blocks
pathvirgin <- "data/female_conditioning/virgin"

## This creates  function
## Path is interchangeable with path 2 
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

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_virgin(pathvirgin) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin <- pathvirgin %>% 
  map_df(~read_raw_virgin(.x)) #.x is a string or NULL - only applies to dfr apparently

# uses what was generated with "df"
df2_virgin <- df_virgin %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

df2_virgin # does it recognise condition from the long data? 

## Important to remember line of code to exclude 4-1_1-4 !!!!




#### OVOD1 FEMALE  ----
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

# uses what was generated with "df"
df2_ovod1 <- df_ovod1 %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

df2_ovod1 # does it recognise condition from the long data? 



## Data Analysis!!!! ----


## MALE
## creating a data column where flies are not on the plate 
df2_male <- df2_male %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))
## a binomial model, not considering other "random" factors
binomial_model_male <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_male)
## looking at model 
summary(binomial_model_male) # 4:1 Conditioned is significant?


# mixed model, considers other "random" factors
mixed_model_male <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate) + (1|observation) , family = binomial, data = df2_male)
## looking at model 
summary(mixed_model_male) # 4:1 Conditioned is NOT significant?

## how to look at this model in emmeans? 
emmeans::emmeans(mixed_model_male, pairwise ~ ratio)
# really want to look at conditioned vs unconditioned






# VIRGIN FEMALE
## creating a data column where flies are not on the plate 
df2_virgin <- df2_virgin %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))
## a binomial model, not considering other "random" factors
binomial_model_virgin <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_virgin)
## looking at model 
summary(binomial_model_virgin) # 4:1 Conditioned is significant?
# mixed model, considers other "random" factors
mixed_model_virgin <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + id + (1|plate) +(1|observation), family = binomial, data = df2_virgin)
mixed_model_virgin_2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate) +(1|observation), family = binomial, data = df2_virgin)

## looking at model 
summary(mixed_model_virgin) # 4:1 Conditioned IS in virgin analysis # Getting some very weird results
summary(mixed_model_virgin_2) # Results look more better here




## OVOD1 FEMALE 
## creating a data column where flies are not on the plate 
df2_ovod1 <- df2_ovod1 %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))
## a binomial model, not considering other "random" factors
binomial_model_ovod1 <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_ovod1)
## looking at model 
summary(binomial_model_ovod1) # 4:1 Conditioned is significant?
# mixed model, considers other "random" factors
mixed_model_ovod1 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate) (1|observation), family = binomial, data = df2_ovod1)
## looking at model 
summary(mixed_model_ovod1) # 4:1 Conditioned IS not, don't know about other ratios though? 










## using the DHARMa package 
library(DHARMa)
#resid.mm<-simulateResiduals(mixed_model_male)
#plot(resid.mm)










## creating a function to pull the median
fly_numbers_summary()<- function(data, group_col) {
  summary <- data %>%
    group_by(plate, {{ group_col }}) %>%
    summarise(median = median(fly_numbers))
  return(summary)
}



four_one_virgin_summary <- fly_numbers_summary(four_to_one_virgin_long, diet)
one_four_virgin_summary <- fly_numbers_summary(one_to_four_virgin_long, diet)
fourone_onefour_virgin_summary <- fly_numbers_summary(fourone_onefour_virgin_long, diet)




#### Data Analysis Feeding 

## For the 4:1/1:4 Assay only 


## MALE ## 
#### UPLOADING AND BINDING THE CORRECT DATA
# 4:1 + 1:4 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b2.xlsx")

fourone_onefour_male_b1 <- fourone_onefour_male_b1  %>% mutate(block = "one")
fourone_onefour_male_b2 <- fourone_onefour_male_b2  %>% mutate(block = "two")

# binding the data
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)
# 4:1 and 1:4 
fourone_onefour_male_long <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")




## Doing poisson to begin with 
male_all_assay <- glm(fly_numbers ~ diet * block, family = poisson, data = fourone_onefour_male_long)

# doing assumption checks 
performance::check_model(male_all_assay, check = c("qq")) # qq looks ok?
performance::check_model(male_all_assay, check = c("outliers")) # weird 

# should I not have block in their yet?

# checking for overdispersion
summary(male_all_assay) # A bit overdispersed 

# Look for 0s 
check_zeroinflation(male_all_assay)
# there is zero inflation 

# There is overdispersion, this could be independent of or because of the zero inflation
# so trying negative binomial family 
## trying to look for significance of experiment with a negative binomial model

# negative binomial model
male_all_assay_nb_2 <- glm.nb(fly_numbers ~ diet * block, data = fourone_onefour_male_long)


# zero inflated poisson

library(pscl)

zip1 <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "poisson", link = "logit", data = fourone_onefour_male_long)

# zero inflated negative binomial

zinb1 <- zeroinfl(fly_numbers ~ diet * block | diet * block, dist = "negbin", link = "logit", data = fourone_onefour_male_long)

AIC(male_all_assay, male_all_assay_nb_2, zip1, zinb1)

# check residuals of zip

sresid <- residuals(zip1, type = "pearson")

pred <- fitted(zip1)

hist(sresid)

plot(sresid ~  pred)

# no evidence of overdispersion but still highly skewed residuals do random effects help?
library(glmmTMB)

male_all_assay <- glmmTMB(fly_numbers ~ diet * block +(1|factor(block)/plate) + (1|observation), family = poisson, data = fourone_onefour_male_long)

simulateResiduals(fittedModel = male_all_assay, plot = T)

# using drop1 to look for any interaction effect of block
drop1(male_all_assay, test = "F") # no interaction effect

# dropping block from the model 
male_all_assay_nb <- glm.nb(fly_numbers ~ diet, data = fourone_onefour_male_long)


# looking at the analysis results from this model 
summary(male_all_assay_nb )

## assumption checks from new model
performance::check_model(male_all_assay_nb, check = c("qq")) # qq doesn't look great
performance::check_model(male_all_assay_nb, check = c("outliers"))

## Checking the AIC of the model 
model_performance(male_all_assay_nb, check = c("AIC")) # AIC quite high

## Using the DHARMa package to check the model 
testDispersion(male_all_assay_nb) # shows how powerful the test is?

# simulating tandom residuals? 
simulationOutput_male <- simulateResiduals(fittedModel = male_all_assay_nb, plot = F)


plotQQunif(simulationOutput_male)
plotResiduals(simulationOutput_male) 
testQuantiles(simulationOutput_male) # why NULL?


## Randomising quantile residuals
residuals(simulationOutput)
# calculates randomised quantile residuals  

residuals(simulationOutput_male, quantileFunction = qnorm, outlierValues = c(-7,7))






## Other option is zero inflated model? 

# trying a zero inflated model
male_all_assay_zi <- zeroinfl(fly_numbers ~ diet, data = fourone_onefour_male_long)


summary(male_all_assay_zi) # completely changes results?? 


# Compare Performance 
compare_performance(male_all_assay, male_all_assay_nb, male_all_assay_zi, rank = TRUE, verbose = FALSE)

# Is negative binomial the best? 
emmeans::emmeans(male_all_assay_nb, pairwise ~ diet)





# Virgin Female Assay 

# mutating a block variable 
fourone_onefour_virgin_b1 <- fourone_onefour_virgin_b1  %>% mutate(block = "one")
fourone_onefour_virgin_b2 <- fourone_onefour_virgin_b2  %>% mutate(block = "two")

# Binding the data
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2)
# Making the data long
fourone_onefour_virgin_long <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# glm with poisson
virgin_all_assay <- glm(fly_numbers ~ diet * block, family = poisson, data = fourone_onefour_virgin_long)

# looking for overdispersion in the model
summary(virgin_all_assay) # shows overdispersion 

# overdispersion so checking for zero inflation
check_zeroinflation(virgin_all_assay) 

# trying a negative binomial model
virgin_all_assay_nb_2 <- glm.nb(fly_numbers ~ diet * block, data = fourone_onefour_virgin_long)

# Looking for interaction effecr with block
drop1(virgin_all_assay_nb_2, test = "F") # diet and block are significant here, so keep in the model?

# Doing model checks 

# easystats/performance
performance::check_model(virgin_all_assay_nb_2, check = c("qq")) # I am not sure how okay this looks 


# Using the DHARMa package for model checks
testDispersion(virgin_all_assay_nb_2) # residual variables?
# does this create random data?

# Using the model 
summary(virgin_all_assay_nb_2)

# More in depth analysis?
emmeans::emmeans(virgin_all_assay_nb_2, pairwise ~ diet * block) 


# Trying a zero inflation model 
virgin_all_assay_zi <- zeroinfl(fly_numbers ~ diet * block, data = fourone_onefour_male_long)

# looking at the model 
summary(virgin_all_assay_zi)

## Comparing all 3x models ??
compare_performance()

# OvoD1 Conditioning 4:1-1:4 analysis -- 
# mutating a block variable 
fourone_onefour_ovod1_b1 <- fourone_onefour_ovod1_b1  %>% mutate(block = "one")
fourone_onefour_ovod1_b2 <- fourone_onefour_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)

# Making the data long
fourone_onefour_ovod1_long <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



# First model 
ovod1_all_assay <- glm(fly_numbers ~ diet * block, family = poisson, data = fourone_onefour_ovod1_long)
# looking for overdispersion
summary(ovod1_all_assay) 

# overdispersion so checking for zero inflation
check_zeroinflation(ovod1_all_assay)

# Doing a negative binomial as there is zero inflation
ovod1_all_assay_2 <- glm.nb(fly_numbers ~ diet * block, data = fourone_onefour_ovod1_long)

# Checking for interaction effect with block 
drop1(ovod1_all_assay_2, test = "F") # not significant so can drop it from the model 

# New model without block
ovod1_all_assay_3 <- glm.nb(fly_numbers ~ diet, data = fourone_onefour_ovod1_long)

# Doing model checks 

# easystats checks 
performance::check_model(ovod1_all_assay_3, check = c("qq"))

# DHARMa checks
testDispersion(ovod1_all_assay_3) # residual points at 0.6?

# Using the model
summary(ovod1_all_assay_3)
emmeans::emmeans(ovod1_all_assay_3, pairwise ~ diet)



