
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


## Reading pupae data in
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

#### Pupae data check. 
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = TRUE))


## Changing it to a dataframe
total_pupae <- as.data.frame(total_pupae)


## working pupae out 
survivability_pupae <- total_pupae %>%
  mutate(fixed_total = 63,  # Add a fixed total of 63
         survivability = (total_pupae / fixed_total) * 100)


## Larvae - pupae plot
larvae_pupae <- ggplot(survivability_pupae, aes(x = treatment, y = survivability, fill = treatment)) +
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
       y = "Survivability (%) of pupae from larval stage", 
       fill = "Treatment") +
  ylim(0, 100)


## Saving a plot
ggsave(filename = "larvae_pupae.png", 
       plot = larvae_pupae, 
       width = 10, 
       height = 6, 
       dpi = 300)


