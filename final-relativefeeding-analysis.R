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
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned * block + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)



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

## Model checks seem ok 
# Using the model 
glmm.bin.m <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate) + (1|observation) , family = binomial, data = df2_male)


# Looking for interaction effect between ratio and block 
drop1(glmm.bin.m, test = "Chisq")
 # No interaction effect found 

# Final model 
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block + (1|plate) + (1|observation) , family = binomial, data = df2_male)



## PLAYING AROUND
glmm.bin.m.01 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned * block + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)

drop1(glmm.bin.m.01, test = "Chisq")
 # No four way interaction effect 



glmm.bin.m.02 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned * Unconditioned + block + ratio  + Conditioned * Unconditioned * block + ratio  * Conditioned + Unconditioned * block  + block * ratio * Conditioned + Unconditioned + block : ratio : Unconditioned + Conditioned  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)


drop1(glmm.bin.m.02, test = "Chisq")


glmm.bin.m.03 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned + Unconditioned + block + ratio  * Unconditioned + block + Conditioned  + Conditioned * Unconditioned + block  +  ratio + Conditioned * block + ratio + Unconditioned + Unconditioned * block + ratio + Conditioned +  ratio * block + Conditioned + Unconditioned + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)

drop1(glmm.bin.m.03, test = "Chisq")

glmm.bin.m.04 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio  * Conditioned + Unconditioned + block + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_male)

drop1(glmm.bin.m.04, test = "Chisq")

glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + block + (1|plate) + (1|observation) , family = binomial, data = df2_male)