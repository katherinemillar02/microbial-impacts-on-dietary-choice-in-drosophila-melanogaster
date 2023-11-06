
## Combining the data
unconditioned_conditioned_combined <- rbind(unconditioned_and_conditioned_rep1, conditioned_and_unconditioned_diets_2)


## Making the data long 
unconditioned_conditioned_long <- unconditioned_conditioned_combined %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Summarising the data; mean, sd, se
unconditioned_conditioned_summary  <- unconditioned_conditioned_long %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))



## Visualising the data
# Creating a boxplot with the data that has been summarised
unconditioned_conditioned_plot <- unconditioned_conditioned_summary   %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#E57157",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#E57157",
                width = 0.2)+
  geom_jitter(data = unconditioned_conditioned_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2.5)+
  labs(x = "Diet Condition",
       y = "Mean +/- S.E. Number of flies per patch",
       title = "Conditioned and Unconditioned Diet Patches Combined")+
  theme_classic() 
