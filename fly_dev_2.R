
pupae_fitness_part2 <- read_excel("data/fitness_development/pupae_data_part2.xlsx")


pupae_boxplot_2 <- ggplot(pupae_fitness_part2, aes(x = time_hours, y = pupae, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha = 0.4, position = position_dodge(width = 20)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 6, dodge.width = 20)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_continuous(breaks = unique(pupae_fitness_part2$time_hours), labels = unique(pupae_fitness_part2$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of pupae emerged",
       fill = "Treatment") +
  facet_grid(~ time_hours, scales = "free_x")




