# Packages 📦📦📦📦####
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

## NEW ANALYSIS FOR 2 - CHOICE ####






## Path for reading Male Data in !! 

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


#### models ####

# with interaction
glmm.bin.m.1 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio * block + (1|plate) + (1|observation) , family = binomial, data = df2_male)

# looking for interaction
drop1(glmm.bin.m.1, test = "Chisq")



# interaction removed
glmm.bin.m.2 <- glmer(cbind(Conditioned, Unconditioned) ~
                        ratio + block + (1|plate) + (1|observation) ,
                      family = binomial, data = df2_male)



#### data analysis ####
summary(glmm.bin.m.2)

# CI
exp(confint(glmm.bin.m.2))

# proportional 
emmeans(glmm.bin.m.2, ~ ratio, type = "response")
# table 
tab_model(glmm.bin.m.2)






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


df2_virgin$ratio <- as.factor(df2_virgin$ratio)
df2_virgin$ratio <- relevel(df2_virgin$ratio, ref = "4:1")

## Changing the block to: one. 
df2_virgin$block <- as.factor(df2_virgin$block)
# Reorder the levels of the factor `block` in df2_virgin
df2_virgin$block <- factor(df2_virgin$block, levels = c("one", "two", "three", "four"))




## Intercept as Conditioned  
glmm.bin.v.1 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                        
                        ratio  * block
                      
                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)



drop1(glmm.bin.v.1, test = "Chisq")


# Without interaction
glmm.bin.v.2 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                        
                        ratio  + block
                      
                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_virgin)


#### data analysis ####
summary(glmm.bin.v.2)

# CI
exp(confint(glmm.bin.v.2, method = "Wald"))

# proportional  
emmeans(glmm.bin.v.2, ~ ratio , type = "response")


# table 
tab_model(glmm.bin.v.2)







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




df2_ovod1$ratio <- as.factor(df2_ovod1$ratio)
df2_ovod1$ratio <- relevel(df2_ovod1$ratio, ref = "4:1")

## Changing the block to: one. 
df2_ovod1$block <- as.factor(df2_ovod1$block)
# Reorder the levels of the factor `block` in df2_virgin
df2_ovod1$block <- factor(df2_ovod1$block, levels = c("one", "two"))




## Intercept as Conditioned  
glmm.bin.o.1 <- glmer(cbind(Conditioned, Unconditioned) ~ 
                        
                        ratio  * block
                      
                      + (1|block/plate) + (1|block/observation) , family = binomial, data = df2_ovod1)



drop1(glmm.bin.o.1, test = "Chisq")





#### data analysis ####
summary(glmm.bin.o.1)

# CI
exp(confint(glmm.bin.o.1))

# proportional 
emmeans(glmm.bin.o.1, ~ ratio + block , type = "response")


# table 
tab_model(glmm.bin.o.1)


