
## Reading in the packages from the packages script.
source("packages.R")

# Function: read diet data====
# Description: Reads the dietary-choice feeding data into the R environment
#
# Input:
#   paths - file paths of csv formatted data
#   blocks - metadata on replication
#   condition_type - metadata on sex and genotype
#   cols: columns to be left out of pivot tables
#   value: response variable - egg numbers or fly counts
# Output:
#   R tibble
#

read_diet_data <- function(path, blocks, condition_type, value, cols = c("plate", "observation")){
  read_excel(path)  %>% 
  pivot_longer(cols = -tidyselect::all_of(cols), 
               names_to = "diet", values_to = value) %>% 
    mutate(block = blocks) %>% 
    mutate(treatment = condition_type) %>% 
    separate(diet, into = c("ratio", "condition"), sep = " ") %>% 
    separate(treatment, into = c("sex", "treatment"))
}


#### Improve code further 

# Function: read many data files ====
# Description: Generates an iterative use of the read data files function
#
# Input:
#   paths - file paths of csv formatted data
#   blocks - metadata on replication
#   condition_type - metadata on sex and genotype
#   value: response variable - egg numbers or fly counts
#   cols: columns to be left out of pivot tables
# Output:
#   Combined csv files into one tibble
#

read_many_diet_files <- function(paths, blocks, conditions, 
                                 value, cols) {
  list_rbind(
    pmap(list(paths, blocks, conditions), 
         function(path, block, condition) {
           read_diet_data(path, block, condition, value, cols)
         })
  )
}


## FOUR CHOICE====
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



## TWO CHOICE====
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


## FEEDING EXPERIMENTS==== 

## FOUR CHOICE====
paths_feed4 <- c("data/male_conditioning/rawdata_m4-1_1-4_b1.xlsx",
           "data/male_conditioning/rawdata_m4-1_1-4_b2.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx",
           "data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx",
           "data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx",
           "data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx"
)
blocks4 <- c("one", "two", "one", "two", "three", "four", "one", "two")
condition_type4 <- c(rep("male_mated", 2), rep("female_virgin", 4), rep("female_ovod1", 2))




### TWO CHOICE====
paths_feed2 <- c("data/male_conditioning/rawdata_m4-1_b1.xlsx",
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
blocks2 <- c("one", "two","one", "two", "one", "two", "three", "four","one", "two", "three", "four","one", "two","one", "two")
condition_type2 <- c(rep("male_mated", 4), rep("female_virgin", 8), rep("female_ovod1", 4))




###
data_configs <- list(
  four_choice_oviposition = list(paths = paths_ovi_4,
       blocks = blocks_ovi_4,
       conditions = condition_type_ovi_4,
       value = "egg_numbers",
       cols = "plate"
       ),
  
  two_choice_oviposition = list(paths = paths_ovi_2,
       blocks = blocks_ovi_2,
       conditions = condition_type_ovi_2,
       value = "egg_numbers",
       cols = "plate"
       ),
  
  four_choice_feeding = list(paths = paths_feed4,
       blocks = blocks4,
       conditions = condition_type4,
       value = "fly_numbers",
       cols = c("plate", "observation")),
  
  two_choice_feeding = list(paths = paths_feed2,
       blocks = blocks2,
       conditions = condition_type2,
       value = "fly_numbers",
       cols = c("plate", "observation"))
)

results <- map(data_configs, function(cfg) {
  read_many_diet_files(
    paths = cfg$paths,
    blocks = cfg$blocks,
    conditions = cfg$conditions,
    value = cfg$value,
    cols = cfg$cols
  )
})

names(results) <- names(data_configs)
