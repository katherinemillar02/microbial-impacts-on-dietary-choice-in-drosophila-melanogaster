## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)


## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)

################################################ PUPAE ANALYSIS ####


## Reading pupae data in
pupae_fitness <- read_excel("data/fitness_development/puape_data.xlsx")


################################################ PUPAE DATA VISUALISATION ####

## Reading the second pupae data set in: 

# This is the same data, but only shows one collection per day 
# The middle hour point has been found, and the counts of both have been summed 
pupae_fitness_2 <- read_excel("data/fitness_development/pupae_data_2.xlsx")

## Boxplot ##
pupae_boxplot_2 <- ggplot(pupae_fitness_2, aes(x = time_hours, y = pupae, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha =.4, position = position_dodge (width = 20)) + #width =10 ) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 6, dodge.width = 20)) +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  scale_x_continuous(breaks = unique(pupae_fitness_2$time_hours), labels = unique(pupae_fitness_2$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank())+
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of pupae emerged",
       fill = "Treatment") +
  facet_grid(~time_hours, scales = "free_x")



## Saving a plot
ggsave(filename = "pupae_boxplot_2.png", 
       plot = pupae_boxplot_2, 
       width = 10, 
       height = 6, 
       dpi = 300)




