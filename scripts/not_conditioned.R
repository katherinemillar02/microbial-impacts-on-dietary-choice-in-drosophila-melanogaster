#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)


## Import Data
not_conditioned <- read_excel("data/not_conditioned.xlsx")

## Making the data long 
not_conditioned_long <- not_conditioned %>% 
  pivot_longer(cols = ("4;1nc":"1;4nc"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
not_conditioned_summary <- not_conditioned_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# creating a boxplot with the data that has been summarised
not_conditioned_plot <- not_conditioned_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = not_conditioned_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)+
  ylim(0.0, 9.0)+
  labs(x = "",
       y = "")+
  theme_classic() 

## Combining the two plots 
conditionedandnot_plot <- male_conditioned_diets_plot + not_conditioned_plot
 
 
 ggsave("plots/conditionedandnot_plot.png", dpi=300)
 