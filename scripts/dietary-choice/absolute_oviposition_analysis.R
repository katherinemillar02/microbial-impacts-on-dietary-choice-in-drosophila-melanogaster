#### Chapter 2 ####

# Analyses for write up 

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


#### Male #### 

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



#### Data Analysis ####

## Testing for a two-way interaction effect
glmm.bin.m.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned)
                                ~ ratio * block + (1|block/plate)  , family = binomial, data = df2_male_oviposition)



## Checking for significance in the two-way interaction effect:
drop1(glmm.bin.m.ovi.2choice, test = "Chisq")
  # Significant two way interaction effect between ratio and block


#### Data Analysis #### 

# Basic analysis
summary(glmm.bin.m.ovi.2choice)

# Confidence intervals
exp(confint(glmm.bin.m.ovi.2choice))

# Values for analysis write-up
emmeans::emmeans(glmm.bin.m.ovi.2choice, specs = ~ ratio, type = "response")

# Table for write-up
tab_model(glmm.bin.m.ovi.2choice, CSS = list(css.table = '+font-family: Arial;'))







#### Virgin Female ####

## Creating a path to virgin conditioning oviposition data: 
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




#### Data Analysis ####

# Final chosen model: Binomial GLMM
 # Testing for two-way interaction effect 
glmm.bin.vf.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) ~
                                   ratio * block + (1|block/plate)  , family = binomial, data = df2_virgin_oviposition)


# Looking for significance in the two-way interaction effect 
drop1(glmm.bin.vf.ovi.2choice, test = "Chisq")
 # A significant two-way interaction effect 




#### Data Analysis for write-up #### 

# Basic analysis
summary(glmm.bin.vf.ovi.2choice)

# Confidence intervals
exp(confint(glmm.bin.vf.ovi.2choice))

# Values for analysis write-up
emmeans::emmeans(glmm.bin.vf.ovi.2choice, specs = ~ ratio, type = "response")

# Table for write-up
tab_model(glmm.bin.vf.ovi.2choice, CSS = list(css.table = '+font-family: Arial;'))





#### OvoD1 Female #### 

#### Reading the data in ####


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





#### Data Analysis ####

## Binomial GLMM - to consider random effects 
 # Testing for the significance of a two-way interaction 
glmm.bin.of.ovi.2choice <- glmer(cbind(Conditioned, Unconditioned) 
                                 ~ ratio * block + (1|block/plate)
                                 , family = binomial, data = df2_ovod1_oviposition)


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









