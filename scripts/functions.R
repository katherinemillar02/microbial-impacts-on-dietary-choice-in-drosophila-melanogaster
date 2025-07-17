
## Reading in the packages from the packages script.
source("scripts/packages.R")


## DIETARY CHOICE EXPERIMENTS 

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





## FITNESS EXPERIMENTS ====================================


# === FUNCTIONAL GLOSSARY ===

#
# all_of:
#   - Used with tidyselect to safely refer to column names passed as character vectors
#


#read_fitness_data():
#   - Custom function to compute total counts from specified columns
#



read_fitness_data <- function(path, count_cols, group_col = "category", cols = "vial", summarise_total = FALSE) { # summarise added 
  fitness_data <- read_excel(path) %>% # read the excel file from paths which have already been established. 
    pivot_longer(cols = all_of(count_cols), # Putting the columns in a long format. 
                 names_to = group_col,# Naming the column of the names data
                 values_to = "count") %>% # Naming the column of the values data 
    drop_na(count) # Dropping all NAs so functions work, there is some missing data 
  
  if (summarise_total) { # this means that if summarise_total is used, 
    group_vars <- c(group_col, "treatment", cols) #  the variables will be the group and treatment
    fitness_data %>% # using the established fitness data 
      group_by(across(all_of(group_vars))) %>%
      summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") # and summarised? 
  } else { # otherwise 
    fitness_data %>% uncount(count) # the fitness data will not be summarised, and uncount will be used - for development
  }
}




