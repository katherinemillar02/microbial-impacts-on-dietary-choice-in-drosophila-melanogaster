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






######## ###### ###### ###### ### Body Weight ====
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")
## Multiplying the values by 1000, for better analysis and to get the plots to work
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000












