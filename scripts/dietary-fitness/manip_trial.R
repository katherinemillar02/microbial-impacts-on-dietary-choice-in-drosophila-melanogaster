library(tidyverse)
library(readxl)
library(viridisLite)


pupae_fitness_part2 <- read_excel("data/fitness_development/pupae-data-manipulated.xlsx")

viridis_colours <- viridis(10)


pupae_boxplot_p2 <- ggplot(pupae_fitness_part2, aes(x = as.factor(time_hours), y = pupae, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  scale_fill_manual(values = viridis_colours[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(pupae_fitness_part2$time_hours)) +
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

fly_fitness_part2 <- read_excel("data//fitness_development/fly_part2.xlsx")

females_data_2 <- subset(fly_fitness_part2, select = c(time_hours, females, treatment))

fly_females_boxplot_part2 <- ggplot(females_data_2, aes(x = factor(time_hours), y = females, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.75)) +
  geom_point(aes(fill = treatment),
             size = 1, shape = 21,
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_fill_manual(values = viridis_colours[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(breaks = unique(females_data_2$time_hours), labels = unique(females_data_2$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged",
       fill = "Treatment") +
  facet_grid(~ time_hours, scales = "free_x")

males_data_2 <- subset(fly_fitness_part2, select = c(time_hours, males, treatment))




fly_males_boxplot_part2 <- ggplot(males_data_2, aes(x = factor(time_hours), y = males, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.75)) +
  geom_point(aes(fill = treatment),
             size = 1, shape = 21,
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_fill_manual(values = viridis_colours[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(breaks = unique(males_data_2$time_hours), labels = unique(males_data_2$time_hours)) + # Ensure correct scale
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Males Emerged",
       fill = "Treatment") +
  facet_grid(~ factor(time_hours), scales = "free_x")


fly_females_boxplot_part2 / 
  fly_males_boxplot_part2



## Playing around with data 

pupae_totals <- pupae_fitness_part2 %>%
  group_by(vial, treatment) %>%
  summarise(total_pupae = sum(pupae))

pupae_totals <- pupae_fitness_part2 %>%
  group_by(treatment) %>%
  summarise(total_pupae = sum(pupae))

   ## Over populated the vials 



fly_totals <- fly_fitness_part2 %>%
  group_by(treatment) %>%
  summarise(total_flies = sum(flies))
pupae_totals <- pupae_fitness_part2 %>%
  group_by(treatment) %>%
  summarise(total_pupae = sum(pupae))


fly_fitness_part2$flies <- fly_fitness_part2$males + fly_fitness_part2$females
