source("packages.R") 


#### Reading data in, sorting data ####

# Using readexcel to get the dataframe 
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Separating the data into female and male columns 
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 


# As DF
#fly_fitness_tidy_MFE <- as.data.frame(fly_fitness_tidy_MFE)


## Cleaning the data into a tidier format, for correct data analysis

# Making NAs 0, so this code works 
fly_fitness_tidy_MFE_filled <- fly_fitness_tidy_MFE %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Changing count to uncount, so the format is better 
fly_fitness_tidy_MFE_2 <- uncount(fly_fitness_tidy_MFE_filled, count)


#### Chosen model: NegBin ####
glm.nb.MFE.fly.2 <- glm.nb(time_hours ~
                           
                           treatment + sex ,
                         
                         data = fly_fitness_tidy_MFE_2)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glm.nb.MFE.fly.2)

# Real values for write-up
emmeans::emmeans(glm.nb.MFE.fly.2, specs = ~ sex + treatment, type = "response")

# Confidence intervals
exp(confint(glm.nb.MFE.fly.2))

# Table for write-up 
tab_model(glm.nb.MFE.fly.2, CSS = list(css.table = '+font-family: Arial;'))
