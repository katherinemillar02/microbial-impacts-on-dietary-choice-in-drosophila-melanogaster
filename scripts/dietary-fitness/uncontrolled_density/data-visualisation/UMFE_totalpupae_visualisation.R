
#### Packages 📦📦📦📦####
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(viridisLite)


#### Reading data in: ####
pupae_fitness_UMFE <- read_excel("data/fitness_development/pupae_data.xlsx")


#### This code shows each vial, for each sex, and for each treatment
### this shows a TOTAL count for each vial, males and females in each vial over all the counts
### adds a column that looks at treatment and sex 
### EMERGENCE BY SEX
overallpupae_UMFE <- pupae_fitness_UMFE %>%
  group_by(vial, treatment) %>%
  summarise(total_count = sum(pupae, na.rm = FALSE)) %>%
  ungroup() 



## Code to make nice plot 
viridis_colors <- viridis(10)



                                                 ## THE PLOT ## 

overall_emergence_pupae_treatment <- ggplot(overallpupae_UMFE, aes(x = treatment, y = total_count, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.8)) + 
  geom_point(aes(fill = treatment), 
             size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Treatment", 
       y = "Number of Pupae Emerged", 
       fill = "Treatment") +
  ylim(0, 150) +
  scale_x_discrete(labels = c("Conditioned", "Unconditioned"))

# Run plot 
overall_emergence_pupae_treatment



# The code to save the plot as an image
ggsave(filename = "overall_emergence_pupae_treatment.png", 
       plot = overall_emergence_pupae_treatment, 
       width = 10, 
       height = 6, 
       dpi = 300)
