## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)


## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)

################################################ PUPAE ANALYSIS ####


## Reading pupae data in
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

## Boxplot ##
pupae_boxplot_MFE <- ggplot(pupae_fitness_MFE, aes(x = factor(time_hours), y = pupae, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(pupae_fitness_MFE$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of pupae emerged",
       fill = "Treatment")


## Display the plot 
pupae_boxplot_2



#### Pupae data check. 
pupae_fitness <- as.data.frame(pupae_fitness)

total_pupae <- pupae_fitness %>% 
  group_by(treatment, vial) %>% 
  summarise(total_pupae = sum(pupae, na.rm = TRUE))

524  / 14 
555 / 15