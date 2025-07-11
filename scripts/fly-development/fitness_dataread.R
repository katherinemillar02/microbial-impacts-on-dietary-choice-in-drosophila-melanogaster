# Load the packages script. 
source("scripts/packages.R")



 # FUNCTIONS ====
#### Total counts adult =================================

paths_adult <- c(uncontrolled = "data/fitness_development/fly_data.xlsx", 
                 controlled = "data/fitness_development/MFE_flies.xlsx")

timecourse_data <- function(path, sex_cols = c("females", "males"), cols = "vial") {
  read_excel(path) %>% 
    pivot_longer(cols = all_of(sex_cols),
                 names_to = "sex",
                 values_to = "count") %>% 
    drop_na(count) %>% 
    uncount(count)
}


all_timecourse_data <- lapply(paths_adult, timecourse_data)

all_timecourse_data <- purrr::imap_dfr(paths_adult, ~ 
                                   timecourse_data(.x) %>% 
                                   mutate(density_condition = .y))


#### Total counts pupa =================================


paths_pupa <- c(uncontrolled = "data/fitness_development/pupae_data.xlsx", 
                 controlled = "data/fitness_development/MFE_pupae.xlsx")

timecourse_data_p <- function(path) {
  read_excel(path) %>% 
    pivot_longer(values_to = "count") %>% 
    drop_na(count) %>% 
    uncount(count)
}



all_timecourse_data <- purrr::imap_dfr(paths_pupa,  ~ 
                                         timecourse_data_p(.x) %>% 
                                         mutate(density_condition = .y))




  
##### UNCONTROLLED DATA ============================================

## Load data 
pupa_uc <- read_excel("data/fitness_development/pupae_data.xlsx") # Pupae
adult_uc <- read_excel("data/fitness_development/fly_data.xlsx") # Flies
 



## Pupae - Development and Total Count ====

# Development with count
pupa_timecourse_uc <- pupa_uc %>%
  rename(count = pupae) %>%
  drop_na(count)

# Development with uncount
pupa_timecourse_uc_uncount <- uncount(pupa_timecourse_uc, count)

# Total count 
pupa_total_uc <- pupa_timecourse_uc %>%
  group_by(vial, treatment) %>%
  summarise(total_count = sum(count, na.rm = FALSE)) %>%
  ungroup()




## Adults - Development and Total Count ====

# Development with count
adult_timecourse_uc <- adult_uc %>%
  pivot_longer(cols = c(females, males),
               names_to = "sex",
               values_to = "count" )%>%
  drop_na(count)

# Development with uncount
pupa_timecourse_uc_uncount  <- uncount(adult_timecourse_uc, count)

# Total count 
adult_total_uc <- adult_timecourse_uc %>% 
  group_by(vial, treatment, sex) %>%
  summarise(total_count = sum(count, na.rm = TRUE))



## Body Weight of Adults ==  
adult_bodyweight_uc <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")

# Convert weight to micrograms
adult_bodyweight_uc$weight_mg <- adult_bodyweight_uc$weight_mg * 1000








##### CONTROLLED DATA ============================================

## Load data 
pupa_c <- read_excel("data/fitness_development/MFE_pupae.xlsx") # Pupae 
adult_c <- read_excel("data/fitness_development/MFE_flies.xlsx") # Flies


## Pupae - Development and Total Count ====

# Development with count
pupa_timecourse_c <- pupa_c %>%
  rename(count = pupae) %>%
  drop_na(count)

# Development with uncount
pupa_timecourse_c_uncount <- uncount(pupa_timecourse_c, count)

# Total count 
pupa_total_c <- pupa_timecourse_c %>%
  group_by(vial, treatment) %>%
  summarise(total_pupa = sum(count, na.rm = FALSE)) %>%
  ungroup()








## Adults - Development and Total Count ====

# Development with count
adult_timecourse_c <- adult_c %>%
  pivot_longer(cols = c(females, males),
               names_to = "sex",
               values_to = "count" )%>%
  drop_na(count)

# Development with uncount
pupa_timecourse_c_uncount  <- uncount(adult_timecourse_c, count)

# Total count 
adult_total_c <- adult_timecourse_c %>% 
  group_by(vial, treatment, sex) %>%
  summarise(total_adult = sum(count, na.rm = TRUE))


## Adult Body Weight
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000






## Survivabiliy ====

# Larvae - Pupae 
lv_pup_c <- pupa_total_c %>%
  mutate(fixed_total = 63,
    survivability = (total_pupa / fixed_total) * 100)

# Pupae - Adult 
pup_adult <- pupa_total_c %>%
  left_join(adult_total_c, by = c("vial", "treatment"))

pup_adult_c <- pup_adult %>%
  mutate(survivability = (total_adult / total_pupa) * 100)

# Larvae to Fly 
fly_survivability <- adult_total_c %>%
  mutate(
    fixed_total = 63,
    survivability = (total_count / fixed_total) * 100)







