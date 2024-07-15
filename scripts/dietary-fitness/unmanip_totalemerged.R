
##### PUTTING OVERALL EMERGENCE DATA OF FLIES ACROSS VIALS TOGETHER

#### This code shows each vial, for each sex, and for each treatment 
fly_emergence_sex <- fly_fitness_tidy %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, sex, sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                levels = c("conditioned females", "unconditioned females",
                                           "conditioned males", "unconditioned males")))



#### CALCULATIONS 
## Calculating median emergence by vial
## This code combines sex and shows overall emergence
fly_emergence_overall <- fly_fitness_tidy %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, treatment) %>%
  summarise(overall_emergence = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, "overall", sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                levels = c("conditioned overall", "unconditioned overall")))



glm_model <- glm(overall_emergence ~ sex_treatment, family = poisson(link = "log"), data = fly_emergence_overall)

summary(glm_model)

## This code shows the median overall emergence (males and females combined) 
vial_overall_emergence_median <- vial_overall_emergence  %>%
  group_by(treatment) %>%
  summarise(median_count = median(overall_emergence, na.rm = TRUE))



## DATA VISUALISA5ION - Visualising overall emergence across vials 
overall_emergence_sex_treatment <- ggplot(fly_emergence_sex, aes(x = sex, y = total_count, fill = treatment)) +
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
  labs(x = "Sex", 
       y = "Number of Flies Emerged", 
       fill = "Treatment") +
  ylim(0, 80)




## An overall code of emergence per time 
emergence_per_time <- fly_fitness %>%
  group_by(treatment, time_hours) %>%
  summarize(total_females = sum(females, na.rm = TRUE),
            total_males = sum(males, na.rm = TRUE)) %>%
  ungroup()












