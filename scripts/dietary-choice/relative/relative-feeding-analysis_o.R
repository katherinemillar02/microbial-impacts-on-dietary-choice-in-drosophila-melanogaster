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


## Path for reading Male Data in 

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


# df2_male_test <- df_male %>%
#   separate(diet, into = c("ratio", "treatment"), sep = " ") %>%
#   group_by(id, observation, plate, ratio, treatment, block) %>%
#   summarise(count = sum(fly_numbers), .groups = 'drop') %>%
#   mutate(condition = treatment) %>%
#   pivot_wider(names_from = "treatment", values_from = "count") %>% 
#   mutate(plate = as.factor(plate), observation = as.factor(observation))
# 
# df2_male_test <- df2_male_test %>%
#   drop_na(plate, observation, Conditioned, Unconditioned)


# Male - Chosen Model 
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned)
                    
                    ~ ratio  * Conditioned * Unconditioned * block 
                    
                    + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)



## Assumption checks 


# DHARMa
simulationOutput <- simulateResiduals(fittedModel = glmm.bin.m, plot = T)
## I think model is good fit

testDispersion(glmm.bin.m) 
# Looks like overdispersion

# Performance check of data using "easystats":
performance::check_model(glmm.bin.m, check = c("homogeneity")) 
## Looks a bit weird 
performance::check_model(glmm.bin.m)

check_overdispersion(glmm.bin.m) 
# No overdispersion detected 

check_zeroinflation(glmm.bin.m) 
# Doesn't work with binomial family 


drop1(glmm.bin.m, test = "Chisq")
 ## No 4 -way 



# three way 
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned)
                    
                    ~ ratio  * Conditioned * Unconditioned +
                      block * Conditioned * Unconditioned + 
                      Conditioned * block * ratio + 
                      Unconditioned * block * ratio 
                    
                    + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)


drop1(glmm.bin.m.2, test = "Chisq")
 # No 3-way interactions 


## No 3-way interactions - looking for 2-way interactions... 
glmm.bin.m.3 <- glmer(cbind(Conditioned, Unconditioned)
                      
                      ~ ratio  * Conditioned  +
                        ratio * Unconditioned +
                       ratio * block + 
                        Conditioned * block  + 
                        Unconditioned * block 
                        
                      
                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)



drop1(glmm.bin.m.3, test = "Chisq")
 # ratio:conditioned interaction effect


# final model
glmm.bin.m.4 <- glmer(cbind(Conditioned, Unconditioned) 
                      
                     ~ ratio + Conditioned + Unconditioned + block + 
                       ratio  : Conditioned  

                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)

## Data Analysis 
summary(glmm.bin.m.4)











#### VIRGIN CONDITIONING 



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
  pivot_wider(names_from = "condition", values_from = "count") %>%  
  mutate(Conditioned = as.integer(Conditioned), Unconditioned = as.integer(Unconditioned))



## Analysis 

## Using Bin GLMM for now 

# First, testing a four-way test
glmm.bin.v.1 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         
                         ratio  * Conditioned * Unconditioned * block
                       
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)

## Looking for interaction in 4-way
drop1(glmm.bin.v.011, test = "Chisq")
 # No interaction found

## Looking for 3-way interactions 
glmm.bin.v.2 <- glmer(cbind(Conditioned, Unconditioned) 
                        
                        ~ ratio  * Conditioned * Unconditioned +
                          block * Conditioned * Unconditioned + 
                          Conditioned * block * ratio + 
                          Unconditioned * block * ratio 
                        
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)



## Testing for sig in 3-way interactions 
drop1(glmm.bin.v.2, test = "Chisq")
 # A 3-way interaction found between ratio, unconditioned, and block... 



## After finding a significant 3-way interaction effect, I have two ways
## (I don't know which one is the proper) - for testing the 2-way interaction effects first 


## The first is just testing the 2 way interaction effects WITH the 3-way interaction effects 
glmm.bin.v.3 <- glmer(cbind(Conditioned, Unconditioned) 
                      
                      ~ ratio + block + Conditioned + Unconditioned + 
                        
                        ratio : Conditioned +
                        ratio : block + 
                        ratio : Unconditioned + 
                        block : Conditioned + 
                        block : Unconditioned + 
                        Conditioned : Unconditioned + 

                        Unconditioned : block : ratio 
                        
                      
                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)


drop1(glmm.bin.v.3, test = "Chisq")
## The issue here, is it only shows me results from 3 two-way interaction effects... 
## Would I then just use summary to test, and which would show them all? 
##  2-way interaction effect between ratio and conditioned 
##  the same 3-way interaction effect between ratio, unconditioned, and block... 

## seeing if i can find more results
summary(glmm.bin.v.3)
## suggests a ratio and block interaction effect? 
## doesnt look like there is conditioned/unconditioned and block effects
## which would track with drop 1



## If this is the case, then the final model: 
glmm.bin.v.4 <- glmer(cbind(Conditioned, Unconditioned) 
                      
                      ~ ratio + block + Conditioned + Unconditioned + 
                        
                        ratio : Conditioned +
                        ratio : block + 
                       
                        Unconditioned : block : ratio 
                      
                      
                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)


drop1(glmm.bin.v.4, test = "Chisq")
## doesnt show ratio and block?? 

# final model results? 
summary(glmm.bin.v.4)



#### OR THE ALTERNATIVE - DO NOT INCLUDE THE SIG THREE WAY INTERACTION EFFECT IN
## THE MODEL, WHEN TESTING FOR THE TWO WAY INTERACTION EFFECTS


## Alternative... 
glmm.bin.v.32  <- glmer(cbind(Conditioned, Unconditioned) 
                      
                      ~ ratio * Conditioned +
                        ratio * block + 
                        ratio * Unconditioned + 
                        block * Conditioned + 
                        block * Unconditioned + 
                        Conditioned * Unconditioned 
                        

                      
                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)



drop1(glmm.bin.v.32, test = "Chisq")
## shows all but says only ratio and cond is sig, which is contrary to other model technique? 
summary(glmm.bin.v.32)
## says more is significant? 



# Very Confused...




## OvoD1 Conditioning 


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


## Analysis, choosing Bin GLMM for now 


glmm.bin.o.01 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned * block + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

drop1(glmm.bin.o.01, test = 'Chisq')
# No 4-way interaction effect 



# Model 
glmm.bin.o.02 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned + block + ratio  + Conditioned * Unconditioned * block + ratio  * Conditioned + Unconditioned * block  + block * ratio * Conditioned + Unconditioned + block : ratio : Unconditioned + Conditioned +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

drop1(glmm.bin.o.02, test = 'Chisq')


glmm.bin.o.03 <- glmer(cbind(Conditioned, Unconditioned) ~ Unconditioned * block * ratio + 
                         Conditioned * Unconditioned +   
                         Conditioned * block + 
                         Conditioned * ratio +
                         Unconditioned * block + 
                         Unconditioned * ratio +
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


drop1(glmm.bin.o.03, test = 'Chisq')


## Final Model? 
glmm.bin.o.04 <- glmer(cbind(Conditioned, Unconditioned) ~ Unconditioned : block : ratio + 
                         Conditioned : ratio +   
                         ratio + block + Conditioned + Unconditioned + 
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


## Using drop1 to find the approprirate interaction effects 
drop1(glmm.bin.o.04, test = 'Chisq')
 ## Confused as to why I am getting different results.




