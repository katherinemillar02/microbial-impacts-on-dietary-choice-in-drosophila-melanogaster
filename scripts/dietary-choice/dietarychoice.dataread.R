source("packages.R")




############## The Effects of Diet Conditioning on Female  Dietary Choice ################### 
############################### Edited by: Katie Millar,
##################### This script is used as the FINAL PATCH, at which 
############################   the data can be read from ...  
##### this is confusing as has some visualisation then there is a separate script for visualisation


#### RELATIVE (FOUR CHOICE) DATA #### 

#### #### #### #### #### #### #### #### #### #### #### OVIPOSITION #### #### #### #### #### #### #### #### #### 


### DIETS CONDITIONED BY MALES #### 

# Block 1 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_1-4_b1_oviposition.xlsx")
# Block 1 - Long 
fourone_onefour_male_oviposition_b1_long <- fourone_onefour_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_1-4_b2_oviposition.xlsx")
# Block 2 - Long 
fourone_onefour_male_oviposition_b2_long <- fourone_onefour_male_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Mutating a block variable
fourone_onefour_male_oviposition_b1 <- fourone_onefour_male_oviposition_b1 %>% mutate(block = "one")
fourone_onefour_male_oviposition_b2 <- fourone_onefour_male_oviposition_b2%>% mutate(block = "two")

# Binding the data for 4:1/1:4 
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)

## Making the data different dataframes
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Splitting "diet" up
combined_ovi_m_split <- combined_ovi_m %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")







### DIETS CONDITIONED BY VIRGIN FEMALES #### 

# Block 2 
fourone_onefour_oviposition_virgin_b2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx")
# Block 2 - Long 
fourone_onefour_oviposition_virgin_b2_long <- fourone_onefour_oviposition_virgin_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 3 
fourone_onefour_oviposition_virgin_b3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx")
# Block 3 - Long
fourone_onefour_oviposition_virgin_b3_long <- fourone_onefour_oviposition_virgin_b3  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 4 
fourone_onefour_oviposition_virgin_b4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx")
# Block 4 - Long 
fourone_onefour_oviposition_virgin_b4_long <- fourone_onefour_oviposition_virgin_b4  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Mutating a block variable to the data-sets
fourone_onefour_oviposition_virgin_b2 <- fourone_onefour_oviposition_virgin_b2 %>% mutate(block = "two")
fourone_onefour_oviposition_virgin_b3 <- fourone_onefour_oviposition_virgin_b3 %>% mutate(block = "three")
fourone_onefour_oviposition_virgin_b4 <- fourone_onefour_oviposition_virgin_b4 %>% mutate(block = "four")

## Binding the different data-sets
fourone_onefour_oviposition_virgin <- rbind(fourone_onefour_oviposition_virgin_b2, fourone_onefour_oviposition_virgin_b3, fourone_onefour_oviposition_virgin_b4)

## adding some data names
combined_ovi_v <- fourone_onefour_oviposition_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Using separate to split the diet into rartio and condition 
combined_ovi_v_split <- combined_ovi_v %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")







### DIETS CONDITIONED BY OVOD1 FEMALES #### 

#### Reading, binding, cleaning data 📖 
## Reading the data in 

# Block 1 
fourone_onefour_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
# Block 1 - Long 
fourone_onefour_oviposition_ovod1_b1_long <- fourone_onefour_oviposition_ovod1_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2
fourone_onefour_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")
# Block 2 - Long 
fourone_onefour_oviposition_ovod1_b2_long <- fourone_onefour_oviposition_ovod1_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Mutating a block variable 
fourone_onefour_oviposition_ovod1_b1 <- fourone_onefour_oviposition_ovod1_b1  %>% mutate(block = "one")
fourone_onefour_oviposition_ovod1_b2 <- fourone_onefour_oviposition_ovod1_b2  %>% mutate(block = "two")

# Binding the data 
fourone_onefour_oviposition_ovod1 <- rbind(fourone_onefour_oviposition_ovod1_b1, fourone_onefour_oviposition_ovod1_b2)

# Making the data long
fourone_onefour_oviposition_ovod1_long  <- fourone_onefour_oviposition_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Splitting up diet variable 
fourone_onefour_oviposition_ovod1_long_split <- fourone_onefour_oviposition_ovod1_long %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")








#### #### #### #### #### #### #### #### #### #### #### FEEDING #### #### #### #### #### #### #### #### #### 



#### DIETS CONDITIONED BY MALES ####

#### Reading, binding and cleaning the data 

## Using read excel to upload the data (block one and block two)

# Block 1 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_b1.xlsx")
# Block 1 - Long
fourone_onefour_male_b1_long <- fourone_onefour_male_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 2 
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_b2.xlsx")
# Block 2 - Long 
fourone_onefour_male_b2_long <- fourone_onefour_male_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Mutating an additional variable for "block" 
fourone_onefour_male_b1 <- fourone_onefour_male_b1  %>% mutate(block = "one")
fourone_onefour_male_b2 <- fourone_onefour_male_b2  %>% mutate(block = "two")

# Using rbind() to bind the block 1 and block 2 data frames into one data frame.
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)

## Using pivot longer to add additional variables to the dataframe, and change variable names 
fourone_onefour_male_long <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  mutate(fly_numbers = as.integer(fly_numbers))  


## Splitting up diet within the actual data frame
combined_m_split <- fourone_onefour_male_long %>%
  separate(diet, into = c("ratio", "condition"), sep = " ")







#### DIETS CONDITIONED BY VIRGIN FEMALES ####

#### Reading, cleaning and binding the data 

#### Reading in the data

# Block 1 
fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx")
# Block 1 - Long
fourone_onefour_virgin_b1_long <- fourone_onefour_virgin_b1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 2 
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")
# Block 2 - Long 
fourone_onefour_virgin_b2_long <- fourone_onefour_virgin_b2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 3 
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")
# Block 3 - Long
fourone_onefour_virgin_b3_long <- fourone_onefour_virgin_b3 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 4 
fourone_onefour_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx")
# Block 4 - Long 
fourone_onefour_virgin_b4_long <- fourone_onefour_virgin_b4 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Mutating a block variable 
fourone_onefour_virgin_b1 <- fourone_onefour_virgin_b1  %>% mutate(block = "one")
fourone_onefour_virgin_b2 <- fourone_onefour_virgin_b2  %>% mutate(block = "two")
fourone_onefour_virgin_b3 <- fourone_onefour_virgin_b3  %>% mutate(block = "three")
fourone_onefour_virgin_b4 <- fourone_onefour_virgin_b3  %>% mutate(block = "four")

# Binding the data
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2, fourone_onefour_virgin_b3, fourone_onefour_virgin_b4)

# Making the data long
combined_vf <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Splitting diet up into ratio and condition
combined_vf_split <- combined_vf %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")







#### DIETS CONDITIONED BY OVOD1 FEMALES ####

#### Reading, binding and cleaning the data 

## Reading the data in

# Block 1 
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")
# Block 1 - Long
fourone_onefour_ovod1_b1_long <- fourone_onefour_ovod1_b1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



# Block 2 
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")
# Block 1 - Long
fourone_onefour_ovod1_b2_long <- fourone_onefour_ovod1_b2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Mutating a block variable 
fourone_onefour_ovod1_b1 <- fourone_onefour_ovod1_b1  %>% mutate(block = "one")
fourone_onefour_ovod1_b2 <- fourone_onefour_ovod1_b2  %>% mutate(block = "two")

# Binding the separate blocks 
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)

## Cleaning the data, adding relevant variables
combined_of <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Splitting the data up 
combined_of_split <- combined_of %>% 
  separate(diet, into = c("ratio", "condition"), sep = " ")











#### ////////////// ABSOLUTE (TWO CHOICE) ASSAYS ////////////// ####


#### #### #### #### #### #### #### #### #### #### #### OVIPOSITION #### #### #### #### #### #### #### #### #### 



#### DIETS CONDITIONED BY MALES ####

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




#### MALE DATA READ FOR VISUALISATION #### 


# 1:4 

# Block 1 
onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/m_1-4_b1_oviposition.xlsx")
# Block 1 - Long 
onefour_male_oviposition_b1_long <- onefour_male_oviposition_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2 
onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/m_1-4_b2_oviposition.xlsx")
# Block 2 - Long 
onefour_male_oviposition_b2_long <- onefour_male_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Mutating a block variable
onefour_male_oviposition_b1 <- onefour_male_oviposition_b1 %>% mutate(block = "one")
onefour_male_oviposition_b2 <- onefour_male_oviposition_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
onefour_male_oviposition <- rbind(onefour_male_oviposition_b1, onefour_male_oviposition_b2)

## Making the data different dataframes
onefour_male_oviposition_long <- onefour_male_oviposition %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")



# 4:1 
# Block 1 
fourone_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_b1_oviposition.xlsx")
# Block 1 - Long 
fourone_male_oviposition_b1_long <- fourone_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2 
fourone_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_b2_oviposition.xlsx")
# Block 2 - Long 
fourone_male_oviposition_b2_long <- fourone_male_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Mutating a block variable
fourone_male_oviposition_b1 <- fourone_male_oviposition_b1 %>% mutate(block = "one")
fourone_male_oviposition_b2 <- fourone_male_oviposition_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
fourone_male_oviposition <- rbind(fourone_male_oviposition_b1, fourone_male_oviposition_b2)

## Making the data different dataframes
fourone_male_oviposition_long <- fourone_male_oviposition %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")
















#### DIETS CONDITIONED BY VIRGIN FEMALES ####

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







#### VIRGIN FEMALE DATA READ FOR VISUALISATION ####

# 1:4 

# Block 1
onefour_virgin_oviposition_b1 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b1.xlsx")
# Block 1 - Long
onefour_virgin_oviposition_b1_long <- onefour_virgin_oviposition_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2 
onefour_virgin_oviposition_b2 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b2.xlsx")
# Block 2 - Long
onefour_virgin_oviposition_b2_long <- onefour_virgin_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Block 3
onefour_virgin_oviposition_b3 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b3.xlsx")
# Block 1 - Long
onefour_virgin_oviposition_b3_long <- onefour_virgin_oviposition_b3  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 4 
onefour_virgin_oviposition_b4 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b4.xlsx")
# Block 2 - Long
onefour_virgin_oviposition_b4_long <- onefour_virgin_oviposition_b4  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")




# Mutating a block variable
onefour_virgin_oviposition_b1 <- onefour_virgin_oviposition_b1 %>% mutate(block = "one")
onefour_virgin_oviposition_b2 <- onefour_virgin_oviposition_b2 %>% mutate(block = "two")
onefour_virgin_oviposition_b3 <- onefour_virgin_oviposition_b1 %>% mutate(block = "three")
onefour_virgin_oviposition_b4 <- onefour_virgin_oviposition_b2 %>% mutate(block = "four")

# Binding the data for 4:1/1:4 
onefour_virgin_oviposition <- rbind(onefour_virgin_oviposition_b1, onefour_virgin_oviposition_b2, onefour_virgin_oviposition_b3, onefour_virgin_oviposition_b4)

## Making the data different dataframes
onefour_virgin_oviposition_long <- onefour_virgin_oviposition  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")






# 4:1 

# Block 1 
fourone_virgin_oviposition_b1 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b1.xlsx")
# Block 1 - Long 
fourone_virgin_oviposition_b1_long <- fourone_virgin_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2 
fourone_virgin_oviposition_b2 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b2.xlsx")
# Block 2 - Long 
fourone_virgin_oviposition_b2_long <- fourone_virgin_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Block 3
fourone_virgin_oviposition_b3 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b3.xlsx")
# Block 3 - Long 
fourone_virgin_oviposition_b3_long <- fourone_virgin_oviposition_b3  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 4
fourone_virgin_oviposition_b4 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b4.xlsx")
# Block 4 - Long 
fourone_virgin_oviposition_b4_long <- fourone_virgin_oviposition_b4  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")






# Mutating a block variable
fourone_virgin_oviposition_b1 <- fourone_virgin_oviposition_b1 %>% mutate(block = "one")
fourone_virgin_oviposition_b2 <- fourone_virgin_oviposition_b2 %>% mutate(block = "two")
fourone_virgin_oviposition_b3 <- fourone_virgin_oviposition_b3 %>% mutate(block = "three")
fourone_virgin_oviposition_b4 <- fourone_virgin_oviposition_b4 %>% mutate(block = "four")

# Binding the data for 4:1/1:4 
fourone_virgin_oviposition <- rbind(fourone_virgin_oviposition_b1, fourone_virgin_oviposition_b2, fourone_virgin_oviposition_b3, fourone_virgin_oviposition_b4)

## Making the data different dataframes
fourone_virgin_oviposition_long <- fourone_virgin_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")









#### DIETS CONDITIONED BY OVOD1 FEMALES ####

#### Reading the data in


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



#### OVOD1 FEMALE DATA READ FOR VISUALISATION ####

# 1:4 
# Block 1 
onefour_ovod1_oviposition_b1 <- read_excel("data/female_conditioning/ovod1/1-4_oviposition_ovod1_b1.xlsx")
# Block 1 - Long 
onefour_ovod1_oviposition_b1_long <- onefour_ovod1_oviposition_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2
onefour_ovod1_oviposition_b2 <- read_excel("data/female_conditioning/ovod1/1-4_oviposition_ovod1_b2.xlsx")
# Block 2 - Long 
onefour_ovod1_oviposition_b2_long <- onefour_ovod1_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")



# Mutating a block variable
onefour_ovod1_oviposition_b1 <- onefour_ovod1_oviposition_b1 %>% mutate(block = "one")
onefour_ovod1_oviposition_b2 <- onefour_ovod1_oviposition_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
onefour_ovod1_oviposition <- rbind(onefour_ovod1_oviposition_b1, onefour_ovod1_oviposition_b1)

## Making the data different dataframes
onefour_ovod1_oviposition_long <- onefour_ovod1_oviposition  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# 4:1 

# Block 1 
fourone_ovod1_oviposition_b1 <- read_excel("data/female_conditioning/ovod1/4-1_oviposition_ovod1_b1.xlsx")
# Block 1 - Long 
fourone_ovod1_oviposition_b1_long <- fourone_ovod1_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Block 2 
fourone_ovod1_oviposition_b2 <- read_excel("data/female_conditioning/ovod1/4-1_oviposition_ovod1_b2.xlsx")
# Block 2 - Long 
fourone_ovod1_oviposition_b2_long <- fourone_ovod1_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Mutating a block variable
fourone_ovod1_oviposition_b1 <- fourone_ovod1_oviposition_b1 %>% mutate(block = "one")
fourone_ovod1_oviposition_b2 <- fourone_ovod1_oviposition_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
fourone_ovod1_oviposition <- rbind(fourone_ovod1_oviposition_b1, fourone_ovod1_oviposition_b2)

## Making the data different dataframes
fourone_ovod1_oviposition_long <- fourone_ovod1_oviposition %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")




















######################################## FEEDING ////// ################################################################



#### DIETS CONDITIONED BY MALES ####

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

## The data set 
df2_male



#### MALE DATA READ FOR VISUALISATION ####

# 1:4 

# Block 1 
onefour_male_feeding_b1 <- read_excel("data/male_conditioning/rawdata_m1-4_b1.xlsx")
# Block 1 - Long
onefour_male_feeding_b1_long <- onefour_male_feeding_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 2
onefour_male_feeding_b2 <- read_excel("data/male_conditioning/rawdata_m1-4_b2.xlsx")
# Block 1 - Long
onefour_male_feeding_b2_long <- onefour_male_feeding_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Mutating a block variable
onefour_male_feeding_b1 <- onefour_male_feeding_b1 %>% mutate(block = "one")
onefour_male_feeding_b2 <- onefour_male_feeding_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
onefour_male_feeding <- rbind(onefour_male_feeding_b1, onefour_male_feeding_b2)

## Making the data different dataframes
onefour_male_feeding_long <- onefour_male_feeding  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")






# 4:1 

# Block 1 
fourone_male_feeding_b1 <- read_excel("data/male_conditioning/rawdata_m4-1_b1.xlsx")
# Block 1 - Long 
fourone_male_feeding_b1_long <- fourone_male_feeding_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 2
fourone_male_feeding_b2 <- read_excel("data/male_conditioning/rawdata_m4-1_b2.xlsx")
# Block 2 - Long
fourone_male_feeding_b2_long <- fourone_male_feeding_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



# Mutating a block variable
fourone_male_feeding_b1 <- fourone_male_feeding_b1 %>% mutate(block = "one")
fourone_male_feeding_b2 <- fourone_male_feeding_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
fourone_male_feeding <- rbind(fourone_male_feeding_b1, fourone_male_feeding_b2)

## Making the data different dataframes
fourone_male_feeding_long <- fourone_male_feeding  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")










#### DIETS CONDITIONED BY VIRGIN FEMALES ####


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



#### VIRGIN DEMLAE DATA READ FOR VISUALISATION ####

# 1:4 

# Block 1 
onefour_virgin_feeding_b1 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b1.xlsx")
# Block 1 - Long
onefour_virgin_feeding_b1_long <- onefour_virgin_feeding_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 2 
onefour_virgin_feeding_b2 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b2.xlsx")
# Block 2 - Long 
onefour_virgin_feeding_b2_long <- onefour_virgin_feeding_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 3
onefour_virgin_feeding_b3 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b3.xlsx")
# Block 1 - Long
onefour_virgin_feeding_b3_long <- onefour_virgin_feeding_b3  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 2 
onefour_virgin_feeding_b4 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b4.xlsx")
# Block 2 - Long 
onefour_virgin_feeding_b4_long <- onefour_virgin_feeding_b4  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")





# Mutating a block variable
onefour_virgin_feeding_b1 <- onefour_virgin_feeding_b1 %>% mutate(block = "one")
onefour_virgin_feeding_b2 <- onefour_virgin_feeding_b2 %>% mutate(block = "two")
onefour_virgin_feeding_b3 <- onefour_virgin_feeding_b3 %>% mutate(block = "three")
onefour_virgin_feeding_b4 <- onefour_virgin_feeding_b4 %>% mutate(block = "four")


# Binding the data for 4:1/1:4 
onefour_virgin_feeding <- rbind(onefour_virgin_feeding_b1, onefour_virgin_feeding_b2, onefour_virgin_feeding_b3, onefour_virgin_feeding_b4)

## Making the data different dataframes
onefour_virgin_feeding_long <- onefour_virgin_feeding  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")





# 4:1 
# Block 1 
fourone_virgin_feeding_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b1.xlsx")
# Block 1 - Long 
fourone_virgin_feeding_b1_long <- fourone_virgin_feeding_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 2 
fourone_virgin_feeding_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b2.xlsx")
# Block 2 - Long 
fourone_virgin_feeding_b2_long <- fourone_virgin_feeding_b2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 3
fourone_virgin_feeding_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b3.xlsx")
# Block 1 - Long 
fourone_virgin_feeding_b3_long <- fourone_virgin_feeding_b3  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 4 
fourone_virgin_feeding_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b4.xlsx")
# Block 4 - Long 
fourone_virgin_feeding_b4_long <- fourone_virgin_feeding_b4 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")




# Mutating a block variable
fourone_virgin_feeding_b1 <- fourone_virgin_feeding_b1 %>% mutate(block = "one")
fourone_virgin_feeding_b2 <- fourone_virgin_feeding_b2 %>% mutate(block = "two")
fourone_virgin_feeding_b3 <- fourone_virgin_feeding_b1 %>% mutate(block = "three")
fourone_virgin_feeding_b4 <- fourone_virgin_feeding_b2 %>% mutate(block = "four")

# Binding the data for 4:1/1:4 
fourone_virgin_feeding <- rbind(fourone_virgin_feeding_b1, fourone_virgin_feeding_b2, fourone_virgin_feeding_b3, fourone_virgin_feeding_b4)

## Making the data different dataframes
fourone_virgin_feeding_long <- fourone_virgin_feeding  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")








#### DIETS CONDITIONED BY OVOD1 FEMALES ####

#### Reading data in 

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




#### OVOD11 FEMALE DATA READ FOR VISUALISATION ####

# 1:4 

# Block 1 
onefour_ovod1_feeding_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b1.xlsx")
# Block 1 - Long 
onefour_ovod1_feeding_b1_long <- onefour_ovod1_feeding_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 2
onefour_ovod1_feeding_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b2.xlsx")
# Block 2 - Long 
onefour_ovod1_feeding_b2_long <- onefour_ovod1_feeding_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Mutating a block variable
onefour_ovod1_feeding_b1 <- onefour_ovod1_feeding_b1 %>% mutate(block = "one")
onefour_ovod1_feeding_b2 <- onefour_ovod1_feeding_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
onefour_ovod1_feeding <- rbind(onefour_ovod1_feeding_b1, onefour_ovod1_feeding_b2)

## Making the data different dataframes
onefour_ovod1_feeding_long <- onefour_ovod1_feeding  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")





# 4:1 
# Block 1 
fourone_ovod1_feeding_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b1.xlsx")
# Block 1 - Long 
fourone_ovod1_feeding_b1_long <- fourone_ovod1_feeding_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

# Block 2 
fourone_ovod1_feeding_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b2.xlsx")
# Block 2 - Long 
fourone_ovod1_feeding_b2_long <- fourone_ovod1_feeding_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Mutating a block variable
fourone_ovod1_feeding_b1 <- fourone_ovod1_feeding_b1 %>% mutate(block = "one")
fourone_ovod1_feeding_b2 <- fourone_ovod1_feeding_b2 %>% mutate(block = "two")

# Binding the data for 4:1/1:4 
fourone_ovod1_feeding <- rbind(fourone_ovod1_feeding_b1, fourone_ovod1_feeding_b2)

## Making the data different dataframes
fourone_ovod1_feeding_long <- fourone_ovod1_feeding  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")






