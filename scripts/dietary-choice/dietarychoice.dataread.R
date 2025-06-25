## Reading in the packages from the packages script.
source("packages.R")


############################# FINAL DATA PREPARATION SCRIPT #############################
# Title: The Effects of Diet Conditioning on Female Dietary Choice
# Authors: Philip Leftwich, Katie Millar
# Purpose: This script is the final version used for data loading, cleaning, and preparation.
# Note: This script includes essential visualisation checks for data integrity, 
#       but full analysis and figures are handled in a separate visualisation script.
##########################################################################################



#### RELATIVE (FOUR CHOICE) DATA — OVIPOSITION EXPERIMENTS ####

# This section processes the full dataset for all three two-choice oviposition data treatments (Male, Virgin female and OvoD1 female)
# It includes data from all experimental blocks and is used to analyse relative
# oviposition preferences in the four-choice setup.
 
read_diet_data <- function(path, blocks, condition_type, cols = "plate", value = "egg_numbers"){
  read_excel(path)  %>% 
  pivot_longer(cols = -all_of(cols), 
               names_to = "diet", values_to = value) %>% 
    mutate(block = blocks) %>% 
    mutate(treatment = condition_type) %>% 
    separate(diet, into = c("ratio", "condition"), sep = " ") %>% 
    separate(treatment, into = c("sex", "treatment"))
}


## FOUR CHOICE
paths_ovi_4 <- c("data/male_conditioning/m_4-1_1-4_b1_oviposition.xlsx",
           "data/male_conditioning/m_4-1_1-4_b2_oviposition.xlsx",
           "data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx",
           "data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx",
           "data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx",
           "data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx",
           "data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx"
           )
blocks_ovi_4 <- c("one", "two", "two", "three", "four", "one", "two")
condition_type_ovi_4 <- c(rep("male_mated", 2), rep("female_virgin", 3), rep("female_ovod1", 2))


## TWO CHOICE 
paths_ovi_2 <- c("data/male_conditioning/m_4-1_b1_oviposition.xlsx",
           "data/male_conditioning/m_4-1_b2_oviposition.xlsx",
           "data/male_conditioning/m_1-4_b1_oviposition.xlsx",
           "data/male_conditioning/m_1-4_b2_oviposition.xlsx",
           "data/female_conditioning/virgin/1-4_oviposition_virgin_b2.xlsx",
           "data/female_conditioning/virgin/1-4_oviposition_virgin_b3.xlsx",
           "data/female_conditioning/virgin/1-4_oviposition_virgin_b4.xlsx",
           "data/female_conditioning/virgin/4-1_oviposition_virgin_b2.xlsx",
           "data/female_conditioning/virgin/4-1_oviposition_virgin_b3.xlsx",
           "data/female_conditioning/virgin/4-1_oviposition_virgin_b4.xlsx",
           "data/female_conditioning/ovod1/1-4_oviposition_ovod1_b1.xlsx",
           "data/female_conditioning/ovod1/1-4_oviposition_ovod1_b2.xlsx",
           "data/female_conditioning/ovod1/4-1_oviposition_ovod1_b1.xlsx",
           "data/female_conditioning/ovod1/4-1_oviposition_ovod1_b2.xlsx"
)
blocks_ovi_2 <- c("one", "two","one", "two", "two", "three", "four","two", "three", "four", "one", "two", "one", "two")
condition_type_ovi_2 <- c(rep("male_mated", 4), rep("female_virgin", 6), rep("female_ovod1", 4))



## DATA FRAMES - OVIPOSITION
four_choice_oviposition <- list_rbind(pmap(list(paths_ovi_4,blocks_ovi_4,condition_type_ovi_4), 
                                           read_diet_data))

two_choice_oviposition <- list_rbind(pmap(list(paths_ovi_2,blocks_ovi_2,condition_type_ovi_2), 
                                           read_diet_data))


####################################################################################################################################














### NEED TO WORK ON HOW I CAN COMBINE FEEDING  


#### FEEDING EXPERIMENTS 

# This section processes the dataset for all three feeding experiments,
# including all experimental blocks. 


read_diet_data <- function(path, blocks, condition_type, cols = c("plate", "observation"), value = "fly_numbers"){
  read_excel(path)  %>% 
    pivot_longer(cols = -all_of(cols), 
                 names_to = "diet", values_to = value) %>% 
    mutate(block = blocks) %>% 
    mutate(treatment = condition_type) %>%
    separate(diet, into = c("ratio", "condition"), sep = " ") %>% 
    separate(treatment, into = c("sex", "treatment"))
}

# make into a iteration
paths_feed4 <- c("data/male_conditioning/rawdata_m4-1_1-4_b1.xlsx",
           "data/male_conditioning/rawdata_m4-1_1-4_b2.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx",
           "data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx",
           "data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx"
)

blocks <- c("one", "two", "one", "two", "three", "four", "one", "two")
condition_type <- c(rep("male_mated", 2), rep("female_virgin", 4), rep("female_ovod1", 2))

four_choice_feeding <- list_rbind(pmap(list(paths_feed4,blocks,condition_type), 
                                       read_diet_data_feeding))










#### FEEDING EXPERIMENTS — TWO CHOICE ASSAYS #### - still need to work on combining feeding and oviposition. 

# This section processes data from feeding experiments using two-choice assays,
# assessing absolute dietary preferences based on paired diet presentations.

read_diet_data_feeding_2 <- function(path, blocks, condition_type, cols = c("plate", "observation"), value = "fly_numbers"){
  read_excel(path)  %>% 
    pivot_longer(cols = -all_of(cols), 
                 names_to = "diet", values_to = value) %>% 
    mutate(block = blocks) %>% 
    mutate(treatment = condition_type) %>%
    separate(diet, into = c("ratio", "condition"), sep = " ") %>% 
    separate(treatment, into = c("sex", "treatment"))
}

# make into a iteration
paths <- c("data/male_conditioning/rawdata_m4-1_b1.xlsx",
           "data/male_conditioning/rawdata_m4-1_b2.xlsx",
           "data/male_conditioning/rawdata_m1-4_b1.xlsx",
           "data/male_conditioning/rawdata_m1-4_b2.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_virgin_b1.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_virgin_b2.xlsx",
           "data/female_conditioning/virgin/rawresults_1-4_virgin_b1.xlsx",
           "data/female_conditioning/virgin/rawresults_1-4_virgin_b2.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_virgin_b3.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_virgin_b4.xlsx",
           "data/female_conditioning/virgin/rawresults_1-4_virgin_b3.xlsx",
           "data/female_conditioning/virgin/rawresults_1-4_virgin_b4.xlsx",
           "data/female_conditioning/ovod1/rawresults_1-4_ovod1_b1.xlsx",
           "data/female_conditioning/ovod1/rawresults_1-4_ovod1_b2.xlsx",
           "data/female_conditioning/ovod1/rawresults_4-1_ovod1_b1.xlsx",
           "data/female_conditioning/ovod1/rawresults_4-1_ovod1_b2.xlsx"
)

blocks <- c("one", "two","one", "two", "one", "two", "three", "four","one", "two", "three", "four","one", "two","one", "two")
condition_type <- c(rep("male_mated", 4), rep("female_virgin", 8), rep("female_ovod1", 4))



two_choice_feeding <- list_rbind(pmap(list(paths,blocks,condition_type), 
                                       read_diet_data_feeding))






########## ALTERNATE CODE BY PHIL, to use? ########## ########## ########## ########## ########## ########## ########## ########## ########## 


feeding <- read_diet_data("data/male_conditioning/rawdata_m4-1_1-4_b1.xlsx", "one", "male virgin", 
                          cols = c("plate", "observation"), value = "fly_numbers")


########## ########## ########## ########## ########## ########## ########## ########## ##########  ########## ########## ########## 
