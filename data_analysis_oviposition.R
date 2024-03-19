## Packages ##
library(tidyverse)
library(lmerTest)
library(readxl)
library(DHARMa)
library(glmmTMB)
library(lme4)
###############




## OVIPOSITION ##

#######################
#### VIRGIN FEMALE ####
#######################

## Creating a path to Virgin Conditioning Oviposition data
pathvirginoviposition <- "data/female_conditioning/virgin"

# reading in oviposition data for virgin 
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


### making a dataset without block 1




## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin_oviposition <- pathvirginoviposition %>% 
  map_df(~read_raw_virgin_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently

# Use grepl to identify rows with "1-4" pattern in any column
pattern <- "b1"
exclude_rows <- grepl(pattern, df_virgin_oviposition$id)

# Subset the dataframe to exclude rows with the "1-4" pattern
df_virgin_oviposition <- df_virgin_oviposition[!exclude_rows, ]


## Mutating a variable for block 
df_virgin_oviposition <- df_virgin_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "b2") ~ "two",
    str_detect(id, "b3") ~ "three",
    str_detect(id, "b4") ~ "four",
  ))


# uses what was generated with "df"
df2_virgin_oviposition <- df_virgin_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition, block) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_virgin_oviposition # does it recognise condition from the long data









#### OVOD1 FEMALE ----
pathovod1oviposition <- "data/female_conditioning/ovod1"

## This creates  function

######################################################################################################################## 
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
########################################################################################################################





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








################ 
#### MALE ####
################ 

## Creating a path to get to the data
pathmaleoviposition <- "data/male_conditioning/treatment_2"


################################################################################################################
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
################################################################################################################


## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male_oviposition(read_raw_male_oviposition)



## creating an actual data set that will read the paths
# first data frame - purr package 
df_male_oviposition <- pathmaleoviposition %>% 
  map_df(~read_raw_male_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently



## Mutating a variable for block 
df_male_oviposition <- df_male_oviposition %>% 
  mutate(block = case_when(
    str_detect(id, "t2b1") ~ "one",
    str_detect(id, "t2b2") ~ "two"
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


############################################################
 
  






                                         #### DATA ANALYSIS ####

###########################
#### MALE OVIPOSITION #####
###########################


## Male Oviposition Analysis
binom_m_egg <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_male_oviposition)

## Assumption checking
performance::check_model(binom_m_egg, check = c("qq")) ## performance not working right now

## Using DHARMa
testDispersion(binom_m_egg) ## This is quite bad? 

simulation_Output <- simulateResiduals(fittedModel = binom_m_egg, plot = F)
plot(simulation_Output) ## Think this shows to be quite a bad model 

## looking at model 
summary(binom_m_egg) # VERY OVERDISPERSED 


## Trying a new model with random effects 
glmer.mm_m_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_male_oviposition)


## checking this new model 
testDispersion(glmer.mm_m_egg) ## This is quite bad? 

simulation_Output <- simulateResiduals(fittedModel = glmer.mm_m_egg, plot = F)
plot(simulation_Output) ## Think this shows to be quite a bad mod

## Tring to generate an actual qqplot

## Generating residuals 
residuals <- residuals(glmer.mm_m_egg, type = "pearson")

qnorm(residuals)

## Will generate a qq 
qqnorm(residuals)


drop1(glmer.mm_m_egg, tets = "Chisq") ##?? 


## Using this model for now 
summary(glmer.mm_m_egg) ## says block is significant 











##################################
### OvoD1 FEMALE OVIPOSITION ####
##################################


## Model 1 

# Binomial model 
binom_od1_egg <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_ovod1_oviposition)

# Assumption checking 

# easystats does not work at the moment 

# DHARMa 
testDispersion(binom_od1_egg) # looks pretty poor

simulationOutput <- simulateResiduals(fittedModel = binom_od1_egg, plot = F)
plot(simulation_Output) # don't think it looks great 

## Tring to generate an actual qqplot

## Generating residuals 
residuals <- residuals(binom_od1_egg, type = "pearson")

qnorm(residuals)

## Will generate a qq 
qqnorm(residuals) ## qq looks bad 

## Using this model for now 
summary(glmer.mm_m_egg)


## Looking at model 
summary(binom_od1_egg) # 4:1 Conditioned is significant?


## Trying a new model 
glmer.ovod1_f_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_ovod1_oviposition)

# Assumption checking 

# easystats does not work at the moment 

# DHARMa 
testDispersion(glmer.ovod1_f_egg) # looks a lot better I think 

simulationOutput <- simulateResiduals(fittedModel = glmer.ovod1_f_egg, plot = F)
plot(simulation_Output) # I don't know 

## qq plot 
## Tring to generate an actual qqplot

## Generating residuals 
residuals <- residuals(glmer.ovod1_f_egg, type = "pearson")
qnorm(residuals)
## Will generate a qq 
qqnorm(residuals) ## qq looks a lot better 

## Looking for significance in block
drop1(glmer.ovod1_f_egg, test = "Chisq") ## block is very significant 

## Looking at results, keeping block in 
summary(glmer.ovod1_f_egg)






##############################
## VIRGIN Female Oviposition ##
##############################

## Model 1 

# Binomial model 
binom_virgin_egg <- glm(cbind(Conditioned, Unconditioned) ~ ratio * block, family = binomial, data = df2_virgin_oviposition)

# Assumption checking 

# easystats does not work at the moment 

# DHARMa 
testDispersion(binom_virgin_egg) # looks pretty poor

simulationOutput <- simulateResiduals(fittedModel = binom_virgin_egg, plot = F)
plot(simulation_Output) # don't think it looks great 

## Tring to generate an actual qqplot

## Generating residuals 
residuals <- residuals(binom_virgin_egg, type = "pearson")

qnorm(residuals)

## Will generate a qq 
qqnorm(residuals) ## qq looks sort of okay - could be better  


summary(binom_virgin_egg) ## very overdispersed 



## Trying a new model 
glmer.virgin_f_egg <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate)  , family = binomial, data = df2_virgin_oviposition)

# Assumption checking 

# easystats does not work at the moment 

# DHARMa 
testDispersion(glmer.virgin_f_egg) # looks a lot better I think 

simulationOutput <- simulateResiduals(fittedModel = glmer.virgin_f_egg, plot = F)
plot(simulation_Output) # I don't know 

## qq plot 
## Tring to generate an actual qqplot

## Generating residuals 
residuals <- residuals(glmer.virgin_f_egg, type = "pearson")
qnorm(residuals)
## Will generate a qq 
qqnorm(residuals) ## qq looks pretty much the same

## Looking for significance in block
drop1(glmer.virgin_f_egg, test = "Chisq") ## block is overall sig 

## Looking at results, keeping block in 
summary(glmer.virgin_f_egg)
## block 3 sig with block 1
## block 2 not sig with block 1 





























##### The Combined 4:1 and 1:4 Assay 



# OvoD1 Conditioning 4:1-1:4 analysis -- 
# mutating a block variable 
combined_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
combined_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")


combined_ovod1_b1 <- combined_ovod1_b1  %>% mutate(block = "one")
combined_ovod1_b2 <- combined_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
combined_ovod1 <- rbind(combined_ovod1_b1, combined_ovod1_b2)

# Making the data long
combined_of_egg  <- combined_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


glm_poisson_of_egg <- glm(egg_numbers ~ diet , family = quasipoisson, data = combined_of_egg)
glm_poisson_of_egg <- glm(egg_numbers ~ diet , family = quasipoisson, data = df2_ovod1_oviposition)


summary(glm_poisson_of_egg)
emmeans::emmeans(glm_poisson_of_egg , pairwise ~ diet)




performance::check_model(glm_poisson_of_egg, check = c("qq")) # doesn't look great - banana 
performance::check_model(glm_poisson_of_egg, check = c("homogeneity")) # slopey 




