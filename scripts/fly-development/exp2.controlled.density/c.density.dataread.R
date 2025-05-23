#### A script for reading data in:





#### Pupae Development #### 
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")

## This code does something with 0s - not sure if it needed... 
pupae_fitness_MFE <- pupae_fitness_MFE %>%
  mutate(pupae = ifelse(is.na(pupae), 0, pupae))

## The uncount code will simply clean the data
pupae_fitness_MFE_2 <- uncount(pupae_fitness_MFE, pupae)






#### Fly Development #### 
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")

## Separating the data into female and male columns 
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 

    # Making NAs 0, so this code works 
       fly_fitness_tidy_MFE_filled <- fly_fitness_tidy_MFE %>%
       mutate(count = ifelse(is.na(count), 0, count))

    # Changing count to uncount, so the format is better 
       fly_fitness_tidy_MFE_2 <- uncount(fly_fitness_tidy_MFE_filled, count)





 #### Overall Flies Analysis  ####     
       overallflies_MFE <- fly_fitness_tidy_MFE %>%
         filter(sex %in% c("females", "males")) %>%
         group_by(vial, sex, treatment, id) %>%
         summarise(total_count = sum(count, na.rm = TRUE))
         # ungroup() %>%
         # mutate(sex_treatment = paste(treatment, sex, sep = " ")) %>%
         # mutate(sex_treatment = factor(sex_treatment,
         #                               levels = c("conditioned females", "unconditioned females",
         #                                          "conditioned males", "unconditioned males")))
       
       
#### Overall Pupae Analysis #### 
       total_pupae <- pupae_fitness_MFE %>% 
         group_by(id, vial, treatment) %>% 
         summarise(total_pupae = sum(pupae, na.rm = FALSE))
       
  
       
       

       
#### Larvae - Pupae Survivability #### 
       survivability_pupae <- total_pupae %>%
         mutate(fixed_total = 63, 
                survivability = (total_pupae / fixed_total) * 100)
       
     
#### Pupae - Fly Survivability #### 
       flies_and_pupae <- overallflies_MFE %>%
         left_join(total_pupae, by = c("id", "vial", "treatment"))
       
       ## Calculating the survivability percentages of flies to pupae 
       survivability_between <- flies_and_pupae %>%
         mutate(survivability = (total_count / total_pupae) * 100)
       
       
       
#### Larvae - Fly Survivability #### 
       fly_survivability <- overallflies_MFE %>%
         mutate(fixed_total = 63,  
                survivability = (total_count / fixed_total)*100 )
             
       

#### Body weight ####
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")
  ## Multiplying the values by 1000, for better analysis and to get the plots to work
  bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000
  
  

  
  

