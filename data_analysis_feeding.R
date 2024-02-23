# Packages
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)

#### DATA UPLOAD 
#### MALE ----####----
# Analysing the raw data
## Creating a path to the scripts within block 1 and block 2 paths
malepath <- "data/male_conditioning/treatment_2/block_1"
malepath2 <-"data/male_conditioning/treatment_2/block_2"

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
read_raw_male(malepath2)

## this creates a list were both data from path 1 and path 2 
malepaths <- list(malepath, malepath2)

## creating an actual data set that will read the paths
# first data frame - purr package 
df_male <- malepaths %>% 
  map_df(~read_raw_male(.x)) #.x is a string or NULL - only applies to dfr apparently

## "second" data frame
# uses what was generated with "df"
df2_male <- df_male %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id,observation, plate, ratio,condition) %>% ## group by what is split
  summarise(count = sum(fly_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count")

df2_male # does it recognise condition from the long data? 

## should now not include 4:1_1:4 assay but I don't know if this worked
head(df2_male)

#### VIRGIN FEMALE  ----
## Creating a path to the scripts within block 1 and block 2 paths
## use for when you get both blocks
pathvirgin <- "data/female_conditioning/virgin"

## This creates  function
## Path is interchangeable with path 2 
read_raw_virgin <-function(path = pathvirgin, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathvirgin,
                              pattern = "rawresults", full.names = T)
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

#### OVOD1 FEMALE  ----
pathovod1 <- "data/female_conditioning/ovod1/block_1"

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


mixed_model_male <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate/observation) + (1|id), family = binomial, data = df2_male)
## looking at model 
summary(mixed_model_male) # 4:1 Conditioned is NOT significant?



# VIRGIN FEMALE
## creating a data column where flies are not on the plate 
df2_virgin <- df2_virgin %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))
## a binomial model, not considering other "random" factors
binomial_model_virgin <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_virgin)
## looking at model 
summary(binomial_model_virgin) # 4:1 Conditioned is significant?
# mixed model, considers other "random" factors
mixed_model_virgin <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + id + (1|plate) +(1|observation), family = binomial, data = df2_virgin)
## looking at model 
summary(mixed_model_virgin) # 4:1 Conditioned IS in virgin analysis 



## OVOD1 FEMALE 
## creating a data column where flies are not on the plate 
df2_ovod1 <- df2_ovod1 %>% mutate(no_flies = 10 - (Conditioned + Unconditioned))
## a binomial model, not considering other "random" factors
binomial_model_ovod1 <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_ovod1)
## looking at model 
summary(binomial_model_ovod1) # 4:1 Conditioned is significant?
# mixed model, considers other "random" factors
mixed_model_ovod1 <- glmer(cbind(Conditioned, Unconditioned) ~ ratio + (1|plate/observation) + (1|id), family = binomial, data = df2_ovod1)
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
# 4:1 + 1:4 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/treatment_2/block_1/rawdata_m4-1_1-4_t2b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/treatment_2/block_2/rawdata_m4-1_1-4_t2b2.xlsx")
# binding the data
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)
# 4:1 and 1:4 
fourone_onefour_male_long <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Doing poisson 
male_all_assay <- glm(fly_numbers ~ diet, family = poisson, data = fourone_onefour_male_long)


summary(male_all_assay)

# A bit overdispersed 


# Look for 0s 


check_zeroinflation(male_all_assay)
# there is zero inflation 

# There is both zero inflation and overdispersion

# trying negative binomial family 
male_all_assay_nb <- glm.nb(fly_numbers ~ diet, data = fourone_onefour_male_long)


summary(male_all_assay_nb)

## assumption checks 
performance::check_model(male_all_assay_nb, check = c("qq"))
performance::check_model(male_all_assay_nb, check = c("outliers"))


model_performance(male_all_assay_nb) # AIC quite high


# Compare Performance 
compare_performance(male_all_assay, male_all_assay_nb, rank = TRUE, verbose = FALSE)


