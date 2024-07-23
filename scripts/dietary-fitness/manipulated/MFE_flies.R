## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)
library(patchwork)


## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)

################################################ PUPAE ANALYSIS ####


## Reading pupae data in
fly_fitness_MFE <- read_excel("data/fitness_development/MFE_flies.xlsx")

#### better arranging data

## Separating the data into female and male columns 
fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness_MFE ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 



females_data <- subset(fly_fitness_MFE, select = c(time_hours, females, treatment))


## Boxplot ##
female_boxplot_2 <- ggplot(females_data, aes(x = factor(time_hours), y = females, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(females_data$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since L1 on diets", 
       y = "Number of Females Emerged",
       fill = "Treatment")


males_data <- subset(fly_fitness_MFE, select = c(time_hours, males, treatment))

## Boxplot ##
male_boxplot_2 <- ggplot(males_data, aes(x = factor(time_hours), y = males, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(males_data$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since L1 on diets", 
       y = "Number of Males Emerged",
       fill = "Treatment")



female_boxplot_2 / 
  male_boxplot_2



#### Pupae data check... 
fly_fitness <- as.data.frame(fly_fitness)

total_flies <- fly_fitness_MFE %>% 
  group_by(treatment, id, sex) %>% 
  summarise(total_flies = sum(males, females, na.rm = TRUE))


total_flies <- as.data.frame(total_flies)
## more conditioned than unconditioned have come out now? 



#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

overall_emergence_treatment <- ggplot(total_flies, aes(x = treatment, y = total_flies, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.75)) + 
  geom_point(aes(fill = treatment), 
             size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Treatment", 
       y = "Number of Flies Emerged", 
       fill = "Treatment") +
  ylim(0, 200)
