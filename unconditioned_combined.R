
## Combining the data sets
unconditioned_combined <- rbind(unconditioned_diets_rep1, unconditioned_rep2)


## Making the data long 
unconditioned_combined_long <- unconditioned_combined %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
unconditioned_combined_summary <- unconditioned_combined_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# creating a boxplot with the data that has been summarised
unconditioned_combined_plot <- unconditioned_combined_summary  %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = unconditioned_combined_long ,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 2)+
  labs(x = "Diet condition",
       y = "Mean +/- S.E. Number of flies per diet patch",
       title = "Unconditioned Diet Patches Combined")+
  theme_classic() 
