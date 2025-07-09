# Load the packages script. 
source("packages.R")




##### UNCONTROLLED DATA ============================================

## Load data 
pupae_fitness_UMFE <- read_excel("data/fitness_development/pupae_data.xlsx") # Pupae
fly_fitness_UMFE <- read_excel("data/fitness_development/fly_data.xlsx") # Flies
 

## Total Counts == 

## Pupae
overallpupae_UMFE <- pupae_fitness_UMFE %>%
  group_by(vial, treatment) %>%
  summarise(total_count = sum(pupae, na.rm = FALSE)) %>%
  ungroup()

## Flies 
overallflies_UMFE <- fly_fitness_UMFE %>%
  pivot_longer(cols = c(females, males),
               names_to = "sex",
               values_to = "count") %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment) %>%
  summarise(total_count = sum(count, na.rm = FALSE), .groups = "drop")


            

            
## Development ==       
            
# Pupae
pupae_fitness_tidy_filled <- pupae_fitness_UMFE %>%
  drop_na(pupae)
pupae_fitness_UMFE_2 <- uncount(pupae_fitness_tidy_filled, pupae)


# Flies 
fly_fitness_tidy <- fly_fitness_tidy_UMFE %>%
  drop_na(count)
fly_fitness_tidy_2 <- uncount(fly_fitness_tidy, count)






## Body Weight ==  
bodyweight_2 <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")

# Convert weight to micrograms
bodyweight_2$weight_mg <- bodyweight_2$weight_mg * 1000








##### CONTROLLED DATA ============================================

## Load data 
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx") # Pupae 
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx") # Flies








## Pupae
total_pupae <- pupae_fitness_MFE %>%
  group_by(id, vial, treatment) %>%
  summarise(total_pupae = sum(pupae, na.rm = FALSE))


## Flies
fly_fitness_tidy_MFE <- fly_fitness_MFE %>%
  pivot_longer(cols = c(females, males),
               names_to = "sex",
               values_to = "count")
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment, id) %>%
  summarise(total_count = sum(count, na.rm = TRUE))






## Development Data ====

# Pupae 
pupae_fitness_MFE <- pupae_fitness_MFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae))
pupae_fitness_MFE_2 <- uncount(pupae_fitness_MFE, pupae)

# Flies 
fly_fitness_tidy_MFE_filled <- fly_fitness_tidy_MFE %>%
  mutate(count = ifelse(is.na(count), 0, count))
fly_fitness_tidy_MFE_2 <- uncount(fly_fitness_tidy_MFE_filled, count)








## Survivabiliy ====

# Larvae to Pupae 
survivability_pupae <- total_pupae %>%
  mutate(
    fixed_total = 63,
    survivability = (total_pupae / fixed_total) * 100)

# Pupae to Fly 
flies_and_pupae <- overallflies_MFE %>%
  left_join(total_pupae, by = c("id", "vial", "treatment"))
survivability_between <- flies_and_pupae %>%
  mutate(survivability = (total_count / total_pupae) * 100)

# Larvae to Fly 
fly_survivability <- overallflies_MFE %>%
  mutate(
    fixed_total = 63,
    survivability = (total_count / fixed_total) * 100)






## Fly Body Weight ====
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000
