## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)


## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)

################################################ PUPAE ANALYSIS ####


## Reading pupae data in
pupae_fitness_adultstraits <- read_excel("data/fitness_development/adulttraits_pupaedev.xlsx")
pupae_fitness_adultstraits <- as.data.frame(pupae_fitness_adultstraits)

## Boxplot ##
pupae_boxplot_adultstraits <- ggplot(pupae_fitness_adultstraits, aes(x = factor(time_hours), y = pupae, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(pupae_fitness_adultstraits$time_hours)) +
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
pupae_boxplot_adultstraits

## Saving a plot
ggsave(filename = "pupae_boxplot_adultstraits.png", 
       plot = pupae_boxplot_adultstraits, 
       width = 10, 
       height = 6, 
       dpi = 300)


