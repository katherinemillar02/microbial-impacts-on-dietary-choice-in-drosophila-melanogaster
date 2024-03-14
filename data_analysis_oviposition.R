library(tidyverse)
library(lmerTest)
library(readxl)

## OVIPOSITION

#### VIRGIN FEMALE  ----
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


## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin_oviposition <- pathvirginoviposition %>% 
  map_df(~read_raw_virgin_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently

# uses what was generated with "df"
df2_virgin_oviposition <- df_virgin_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_virgin_oviposition # does it recognise condition from the long data






#### OVOD1 FEMALE ----
pathovod1oviposition <- "data/female_conditioning/ovod1"

## This creates  function
## Path is interchangeable with path 2 
read_raw_ovod1_oviposition <-function(path = pathovod1oviposition, pattern_to_exclude = "4-1_1-4_oviposition"){
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

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_ovod1_oviposition(pathovod1oviposition) 


## creating an actual data set that will read the paths
# first data frame - purr package 
df_ovod1_oviposition <- pathovod1oviposition %>% 
  map_df(~read_raw_ovod1_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently

# uses what was generated with "df"
df2_ovod1_oviposition <- df_ovod1_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_ovod1_oviposition # does it recognise condition from the long data? 

#### OVOD1 MALE ----
pathmaleoviposition <- "data/male_conditioning/treatment_2"


## This creates  function
## Path is interchangeable with path 2 
read_raw_male_oviposition <-function(path = pathmaleoviposition, pattern_to_exclude = "4-1_1-4_oviposition"){
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

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_male_oviposition(read_raw_male_oviposition)



## Putting the two male oviposition blocks together 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_male_oviposition <- pathmaleoviposition %>% 
  map_df(~read_raw_male_oviposition(.x)) #.x is a string or NULL - only applies to dfr apparently

# uses what was generated with "df"
df2_male_oviposition <- df_male_oviposition %>%
  separate(diet, into = c("ratio", "condition"), sep = " ") %>%#separate will turn a single factor column into multiple columns
  group_by(id, plate, ratio, condition) %>% ## group by what is split
  summarise(count = sum(egg_numbers)) %>% 
  pivot_wider(names_from = "condition", values_from = "count") 

## new data 
df2_male_oviposition # does it recognise condition from the long data? 



#### DATA ANALYSIS 
# OvoD1 Oviposition
binomial_model_ovod1_oviposition <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_ovod1_oviposition)
## looking at model 
summary(binomial_model_ovod1_oviposition) # 4:1 Conditioned is significant?

emmeans::emmeans(binomial_model_ovod1_oviposition, pairwise ~ ratio)

# Male Oviposition
binomial_model_male_oviposition <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_male_oviposition)
## looking at model 
summary(binomial_model_male_oviposition) # 4:1 Conditioned is very significant?


# Female Oviposition
binomial_model_virgin_oviposition <- glm(cbind(Conditioned, Unconditioned) ~ ratio, family = binomial, data = df2_virgin_oviposition)

summary(binomial_model_virgin_oviposition)








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




