
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






 ## Development 
timecourse_data <- function(path, count_cols, group_col = "category", cols = "vial") {
  read_excel(path) %>%
    pivot_longer(cols = all_of(count_cols),
                 names_to = group_col,
                 values_to = "count") %>%
    drop_na(count) %>%
    uncount(count)
}



## Total count 

totalcount_data <- function(path, count_cols, group_col = "category", cols = "vial") {
  group_vars <- c(group_col, cols)  # define what to group by
  
  read_excel(path) %>%
    pivot_longer(cols = all_of(count_cols),
                 names_to = group_col,
                 values_to = "count") %>%
    drop_na(count) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")
}
