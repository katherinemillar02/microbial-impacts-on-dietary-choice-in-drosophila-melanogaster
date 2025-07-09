
read_development_data <- function(path, density, condition_type, value, cols = c("vial")){
  read_excel(path)  %>% 
    pivot_longer(cols = -tidyselect::all_of(cols), 
                  values_to = value) %>% 
    mutate(density = densities) %>% 
    mutate(treatment = condition_type) %>% 
    separate(treatment, into = c("sex", "treatment"))
}


read_many_diet_files <- function(paths, densities, conditions, 
                                 value, cols) {
  list_rbind(
    pmap(list(paths, blocks, conditions), 
         function(path, block, condition) {
           read_development_data(path, density, condition, value, cols)
         })
  )
}



paths_pupae_count <- c("data/fitness_development/fly_data.xlsx",
                       )



