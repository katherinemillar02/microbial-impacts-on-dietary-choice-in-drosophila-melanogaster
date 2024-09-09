#### Packages #### 
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)



#### Reading data in: ####
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")


## Making a tidy version of the data,
#### Puts males and females together into a "sex" column.
fly_fitness_tidy_MFE <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                            cols = c( females, males),
                                            names_to = "sex",
                                            values_to = "count") 



#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallflies_MFE <- fly_fitness_tidy_MFE %>%
  group_by(id, vial, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) 


## making it a dataframe 
overallflies_MFE <- as.data.frame(overallflies_MFE)


## making a surviability data frame 
fly_survivability <- overallflies_MFE %>%
  mutate(fixed_total = 63,  # Add a fixed total of 63
         survivability = (total_count / fixed_total)*100 )




## Plot for larvae - fly 
larvae_fly <- ggplot(fly_survivability, aes(x = treatment, y = survivability, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.75)) + 
  geom_point(aes(fill = treatment), 
             size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  scale_x_discrete(labels = c("Conditioned", "Unconditioned")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Treatment", 
       y = "Survivability (%) of flies from larval stage", 
       fill = "Treatment") +
  ylim(0, 100)


## Saving a plot
ggsave(filename = "larvae_fly.png", 
       plot = larvae_fly, 
       width = 10, 
       height = 6, 
       dpi = 300)




