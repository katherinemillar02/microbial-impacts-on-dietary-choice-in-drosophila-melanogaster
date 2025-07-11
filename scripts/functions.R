
## Reading in the packages from the packages script.
source("scripts/packages.R")

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
