# Load the packages script. 
source("scripts/packages.R")

##### DATA READ - FOR FLY FITNESS 
## Controlled and Uncontrolled density experiments. 

            # FUNCTIONS ====

## GLOSSARY: 

# R functions used 
# purrr::imap_dfr - 
   # purr - a package that provides tools for working with functions and vectors. 
   # imap_dfr - imap will give both the values and names, looped together and dfr will combine everything into a single data frame 
# all_of - will make sex into one column? 
# path
# timecourse_data()
# al_timecourse_data()


## Setting file paths of adult data 
paths_adult <- c(uncontrolled = "data/fitness_development/fly_data.xlsx", 
                 controlled = "data/fitness_development/MFE_flies.xlsx")

## Setting file paths of pupa data
paths_pupa <- c(uncontrolled = "data/fitness_development/pupae_data.xlsx", ## There are two pupa data files, (2) is for visualisation purposes.
                controlled = "data/fitness_development/MFE_pupae.xlsx")



# adults
timecourse_adult <- imap_dfr(paths_adult, ~ timecourse_data(.x, count_cols = c("females", "males")) %>%
                               mutate(condition = .y))

#  pupa
timecourse_pupae <- imap_dfr(paths_pupa, ~ timecourse_data(.x, count_cols = c("pupae")) %>% 
                               mutate(condition = .y))


# adult
totalcount_adult <- imap_dfr(paths_adult, ~ totalcount_data(.x, count_cols = c("females", "males")) %>%
                               mutate(condition = .y))

# pupa
totalcount_pupa <- imap_dfr(paths_adult, ~ totalcount_data(.x, count_cols = c("females", "males")) %>%
                               mutate(condition = .y))





## Body Weight of Adults ==  
# Uncontrolled
adult_bodyweight_uc <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")
adult_bodyweight_uc$weight_mg <- adult_bodyweight_uc$weight_mg * 1000
# Controlled 
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







