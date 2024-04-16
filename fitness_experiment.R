## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)

## Read data in
pupae_fitness <- read_excel("data/fitness_experiment/pupae_data.xlsx")

## choosing colours from virids to use
viridis_colors <- viridis(10)

## Creating a plot (bar plot for now)
pupae_fitness_plot <- ggplot(pupae_fitness, aes(x = `time (hours)`, y = pupae, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_line(aes(col = treatment), position = position_dodge(width = 1)) +
  scale_fill_manual(values = viridis_colors[c(1,8)]) +
  theme_classic() +
  labs(x = "Time (hours) since eggs laid", y = "Pupae", title = "Pupae count over time by treatment") 
  
  






