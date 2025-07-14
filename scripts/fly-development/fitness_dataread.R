
##### DATA READ - FOR FLY FITNESS 
## Controlled and Uncontrolled 


###### ###### ###### ###### ###### ###### ###### ###### ######  Uncontrolled ========================


####### ###### ###### ###### ##### Total Counts ===
### Pupae
pupae_fitness_UMFE <- read_excel("data/fitness_development/pupae_data.xlsx")
## Sum of total
overallpupae_UMFE <- pupae_fitness_UMFE %>%
  group_by(vial, treatment) %>%
  summarise(total_count = sum(pupae, na.rm = FALSE)) %>%
  ungroup() 

### Fly 
fly_fitness_UMFE <- read_excel("data/fitness_development/fly_data.xlsx")

## Sum of total
fly_fitness_tidy_UMFE <- tidyr::pivot_longer(data = fly_fitness_UMFE ,
                                             cols = c( females, males),
                                             names_to = "sex",
                                             values_to = "count") 

# Load the packages script. 
source("scripts/packages.R")






 # FUNCTIONS ====

# Adult Development for both controlled and uncontrolled densities

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


all_timecourse_data <- purrr::imap_dfr(paths_adult, ~ 
                                   timecourse_data(.x) %>% 
                                   mutate(density_condition = .y))






# Pupa Development for both controlled and uncontrolled densities

paths_pupa <- c(uncontrolled = "data/fitness_development/pupae_data.xlsx", ## There are two pupa data files, (2) is for visualisation purposes.
                 controlled = "data/fitness_development/MFE_pupae.xlsx")



timecourse_data_p <- function(path) {
  read_excel(path) %>%
    drop_na(pupae) %>%
    uncount(pupae)
}


all_timecourse_data_p <- purrr::imap_dfr(paths_pupa,  ~ 
                                         timecourse_data_p(.x) %>% 
                                         mutate(density_condition = .y))


# END OF FUNCTIONS === ==== ==== ==== =====










<<<<<<< HEAD
=======

######## ###### ###### ###### ### Development ====
### Pupae
pupae_fitness_tidy_filled <- pupae_fitness_UMFE %>%
  drop_na(pupae)
## uncount
pupae_fitness_UMFE_2 <- uncount(pupae_fitness_tidy_filled, pupae)

### Fly
fly_fitness_tidy  <- fly_fitness_tidy_UMFE %>%
  drop_na(count)
## uncount
fly_fitness_tidy_2  <- uncount(fly_fitness_tidy, count)


######## ###### ###### ###### ### Body Weight ====
#### Reading data in: ####
bodyweight_2 <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")
# Multiplying by 1000 to get consistent with visualisation
bodyweight_2$weight_mg <- bodyweight_2$weight_mg * 1000

>>>>>>> origin/project-refinement
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
>>>>>>> eaf73f101abff3bb66bdbc8a96c75ba6af3d8a26











###### ###### ###### ###### ###### ###### ###### ###### ######  Controlled ================================================
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")

####### ###### ###### ###### ##### Total Counts ===

## Pupae
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = FALSE))

#### Fly   

fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 

overallflies_MFE <- fly_fitness_tidy_MFE %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment, id) %>%
  summarise(total_count = sum(count, na.rm = TRUE))


####### ###### ###### ###### ##### Development ===

## Pupae 
pupae_fitness_MFE <- pupae_fitness_MFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae))
# uncount
pupae_fitness_MFE_2 <- uncount(pupae_fitness_MFE, pupae)


## Fly
fly_fitness_tidy_MFE_filled <- fly_fitness_tidy_MFE %>%
  mutate(count = ifelse(is.na(count), 0, count))
# uncount
fly_fitness_tidy_MFE_2 <- uncount(fly_fitness_tidy_MFE_filled, count)

## Adults - Development and Total Count ====

# Development with count
adult_timecourse_uc <- adult_uc %>%
  pivot_longer(cols = c(females, males),
               names_to = "sex",
               values_to = "count" )%>%
  drop_na(count)

# Development with uncount
adult_timecourse_uc_uncount  <- uncount(adult_timecourse_uc, count)

# Total count 
adult_total_uc <- adult_timecourse_uc %>% 
  group_by(vial, treatment, sex) %>%
  summarise(total_count = sum(count, na.rm = TRUE))



## Body Weight of Adults ==  
adult_bodyweight_uc <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")

# Convert weight to micrograms
adult_bodyweight_uc$weight_mg <- adult_bodyweight_uc$weight_mg * 1000






####### ###### ###### ###### ##### Survivability ===

#### Larvae - Pupae 
survivability_pupae <- total_pupae %>%
  mutate(fixed_total = 63, 
         survivability = (total_pupae / fixed_total) * 100)


#### Pupae - Fly Survivability  
flies_and_pupae <- overallflies_MFE %>%
  left_join(total_pupae, by = c("id", "vial", "treatment"))

survivability_between <- flies_and_pupae %>%
  mutate(survivability = (total_count / total_pupae) * 100)



#### Larvae - Fly Survivability 
fly_survivability <- overallflies_MFE %>%
  mutate(fixed_total = 63,  
         survivability = (total_count / fixed_total)*100 )


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






######## ###### ###### ###### ### Body Weight ====
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")
## Multiplying the values by 1000, for better analysis and to get the plots to work


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







