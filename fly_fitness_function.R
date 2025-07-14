
# Function to read and tidy a single diet data file
read_diet_data <- function(path, treatment, value, cols = c("vial")) {
  read_excel(path) %>%
    pivot_longer(cols = -tidyselect::all_of(cols), values_to = value, names_to = "variable") %>%
    mutate(treatment = treatment)
}

# Function to read and combine multiple diet data files
read_many_diet_files <- function(paths, treatments, value, cols) {
  list_rbind(
    pmap(list(paths, treatments, value),
         function(path, treatment, value) {
           read_diet_data(path, treatment, value, cols)
         })
  )
}


# File paths
paths_uncontrolled_dev <- c("data/fitness_development/pupae_data.xlsx",
                            "data/fitness_development/fly_data.xlsx")

# Corresponding treatment/condition labels
condition_uc <- c("conditioned", "unconditioned")

# Read and combine all files
dev_uc <- read_many_diet_files(paths = paths_uncontrolled_dev,
                               treatments = condition_uc,
                               value = "count",
                               cols = "vial")
