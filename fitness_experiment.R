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
  labs(x = "Time (hours) since eggs laid", 
       y = "Pupae") 


## Read data in
fly_fitness <- read_excel("data/fitness_experiment/fly_data.xlsx")


fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                 cols = c(females, males),
                                 names_to = "sex",
                                 values_to = "count")


fly_fitness_tidy$sex_treatment <- paste(fly_fitness_tidy$sex, fly_fitness_tidy$treatment, sep = "_")


library(ggplot2)

ggplot(fly_fitness_tidy, aes(x = time_hours, y = count, fill = sex_treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Time (hours)", y = "Count", title = "Counts over time by vial and sex-treatment combination") + 
  theme_classic()



ggplot(fly_fitness_tidy, aes(x = time_hours, y = count, fill = sex_treatment)) +
  geom_bar(position="dodge", stat="identity")



