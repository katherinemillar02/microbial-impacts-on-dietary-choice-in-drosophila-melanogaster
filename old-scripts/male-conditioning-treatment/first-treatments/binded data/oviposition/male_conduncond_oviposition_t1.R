
#### Adding a variable 
maleconditioned_t1_bind_1_ovi <- conditionedunconditioned_b1.2_median_egg_long   %>% mutate(experiment = "one")
maleconditioned_t1_bind_2_ovi <- conduncond_b2_eggs_long %>% mutate(experiment = "two")


## binding the data
male_t1_bind_conditioned_ovi <- rbind(maleconditioned_t1_bind_1_ovi, maleconditioned_t1_bind_2_ovi)

# visualising the data
male_t1_bind_conditioned_ovi_plot <- 
  male_t1_bind_conditioned_ovi  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "BuPu")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male Conditioned and Unconditioned Treatment")+
  theme(legend.position="none")+ 
  ylim(0,125)+
  geom_jitter(data = male_t1_bind_conditioned_ovi,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
