## FLY FITNESS EXPERIMENTS ==== 

# Load functions (also loads required packages) 
source("scripts/functions.R")



# === FILE PATH SETUP ===

# File paths for adult data 
paths_adult <- c(uncontrolled = "data/fitness_development/fly_data.xlsx", 
                 controlled = "data/fitness_development/MFE_flies.xlsx")

# File paths for pupa data
paths_pupa <- c(uncontrolled = "data/fitness_development/pupae_data.xlsx", ## There are two pupa data files, (2) is for visualisation purposes.
                controlled = "data/fitness_development/MFE_pupae.xlsx")





# === READ TIMECOURSE DATA ===

# Read adult timecourse data, tagging each row with its condition ("controlled"/"uncontrolled")
timecourse_adult <- imap_dfr(paths_adult, ~ read_fitness_data(.x, count_cols = c("females", "males"), summarise_total = FALSE) %>%
                               mutate(condition = .y))

# Read pupa timecourse data
timecourse_pupae <- imap_dfr(paths_pupa, ~ read_fitness_data(.x, count_cols = c("pupae"), summarise_total = FALSE) %>%
                               mutate(condition = .y))


# === READ TOTAL COUNTS ===

# Total count of adults per vial/treatment/etc.
timecourse_adult <- imap_dfr(paths_adult, ~ read_fitness_data(.x, count_cols = c("females", "males"), summarise_total = TRUE) %>%
                               mutate(condition = .y))

# Total count of pupae per vial/treatment/etc.
timecourse_pupae <- imap_dfr(paths_pupa, ~ read_fitness_data(.x, count_cols = c("pupae"), summarise_total = TRUE) %>%
                               mutate(condition = .y))








#### ===== Left to fix 
## Body Weight of Adults ==  No point turning this into a function? Code would become longer?
# Uncontrolled
adult_bodyweight_uc <- read_excel("data/fitness_development/bodyweight_flies2.xlsx")
adult_bodyweight_uc$weight_mg <- adult_bodyweight_uc$weight_mg * 1000
# Controlled 
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000





#### ===== Left to fix ---------
## Survivabiliy-----Currently does not work because of files / function.

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






