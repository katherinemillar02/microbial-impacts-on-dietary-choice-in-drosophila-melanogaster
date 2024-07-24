## OvoD1 Conditioning 
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


## Analysis, choosing Bin GLMM for now: 












## Using : as an interaction effect 


## First, testing for a four-way interaction effect. 
## Unlike *, this will just test for the four-way interaction effect? 
glmm.bin.o.001 <- glmer(cbind(Conditioned, Unconditioned) ~
                          ratio  : Conditioned : Unconditioned : block
                        + ratio  + Conditioned + Unconditioned + block
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

# Tests for the four-way interaction effect. 
drop1(glmm.bin.o.001, test = 'Chisq')
  # No 4-way interaction effect found. 



# Testing for 3-way interaction effects
glmm.bin.o.002 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                          
                          ratio  : Conditioned : Unconditioned 
                        + ratio   : Unconditioned : block 
                        + ratio  : Conditioned : block  
                        + block : ratio : Conditioned  
                        + block : ratio : Unconditioned 
                        + block : Conditioned : Unconditioned
                        + Conditioned + Unconditioned + ratio + block
                        
                        +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


## Test should show 3-way interaction effects 
drop1(glmm.bin.o.002, test = 'Chisq')
  ## Results only show sig 3-4 out of 6  results 
  ## Interaction effect between ratio, Unconditioned and block, and ratio Conditioned and block


## Testing for 2-way interaction effects, this model includes the previously significant 3-way interaction effects 
glmm.bin.o.003 <- glmer(cbind(Conditioned, Unconditioned)
                        ~ Unconditioned : block : ratio + 
                          + ratio : Conditioned : block +
                          Conditioned : Unconditioned +   
                          Conditioned : block + 
                          Conditioned : ratio +
                          Unconditioned : block + 
                          Unconditioned : ratio +
                          + Conditioned + Unconditioned + ratio + block
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)




## Testing for the two-way interaction effects
drop1(glmm.bin.o.003, test = 'Chisq')
  ## drop1 only shows one 2-way interaction effect 
  ## Interaction effect between Unconditioned and Conditioned




## Testing for 2-way interaction effects
# but this model includes DOES NOT INCLUDE the previously significant 3-way interaction effects 
glmm.bin.o.0031 <- glmer(cbind(Conditioned, Unconditioned)
                         ~  
                           Conditioned : Unconditioned +   
                           Conditioned : block + 
                           Conditioned : ratio +
                           Unconditioned : block + 
                           Unconditioned : ratio +
                           + Conditioned + Unconditioned + ratio + block
                         
                         + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

## Testing for interactions 
drop1(glmm.bin.o.0031, test = 'Chisq')
  # No 2- way interactions? I think this tracks with what was in "*" though! 




## Final Model? 
glmm.bin.o.004 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                          Unconditioned : block : ratio 
                        + Conditioned : block : ratio 
                        + Conditioned + Unconditioned + ratio + block
                        
                        + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


## Using drop1 to find the appropriate interaction effects 
drop1(glmm.bin.o.004, test = 'Chisq')




















#### USING * IN THE MODELS: 

## Looking for a four-way interaction effect.
## Although this model will interact everything with everything. 
glmm.bin.o.01 <- glmer(cbind(Conditioned, Unconditioned) ~
                         ratio  * Conditioned * Unconditioned * block 
                       + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

## Tests for just the four-way interaction effect
drop1(glmm.bin.o.01, test = 'Chisq')
   # No 4-way interaction effect. 



# Testing for three-way interaction effects. 
## Although this model will interact everything with everything, so will also look for two-way interaction effects. 
glmm.bin.o.02 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         
                         ratio  * Conditioned * Unconditioned 
                       + ratio   * Unconditioned * block 
                       + ratio  * Conditioned * block  
                       + block * ratio * Conditioned  
                       + block * ratio * Unconditioned 
                       + block * Conditioned * Unconditioned
                       
                       +  (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

## Looking for 3-way interaction effect, shows those first and only shows 4/6
drop1(glmm.bin.o.02, test = 'Chisq')
   ## A 3-way interaction effect between ratio, Unconditioned and block found


## Testing for 2-way interaction effects 



## This model tests for two-way interaction effects, but includes a previously significant 3-way interaction effect 
glmm.bin.o.03 <- glmer(cbind(Conditioned, Unconditioned)
                       ~ Unconditioned * block * ratio + 
                         Conditioned * Unconditioned +   
                         Conditioned * block + 
                         Conditioned * ratio +
                         Unconditioned * block + 
                         Unconditioned * ratio +
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)

# Testing for the two-way interaction effect. 
drop1(glmm.bin.o.03, test = 'Chisq')
 ## Still shows 3 - way interaction effect between ratio, Unconditioned and block. 
 ## Now shows 2-way interaction effect between ratio and Conditioned, but only shows 3/5 possible 2-way combinations. 



## This model is the same as above and tests for two-way interaction effects
## but DOES NOT include a previously significant 3-way interaction effect 
glmm.bin.o.031 <- glmer(cbind(Conditioned, Unconditioned)
                       ~  
                         Conditioned * Unconditioned +   
                         Conditioned * block + 
                         Conditioned * ratio +
                         Unconditioned * block + 
                         Unconditioned * ratio +
                         
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


## Testing for the two-way interaction effects 
drop1(glmm.bin.o.031, test = 'Chisq')
 # No 2- way interactions? but shows all 5 possible 2-way combos written






## Final Model? 
glmm.bin.o.04 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                         Unconditioned * block * ratio + Conditioned
                   
                         
                         (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)


## Using drop1 to find the approprirate interaction effects 
drop1(glmm.bin.o.04, test = 'Chisq')
  ## Confused, I did not put an interaction effect between ratio and conditioned in the model so why is it doing it
  # Is it because of the *? 
  















