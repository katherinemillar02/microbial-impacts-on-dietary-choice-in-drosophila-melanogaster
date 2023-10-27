#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)


## Import Data
male_conditioned_diets <- read_excel("data/male_conditioning.xlsx")

## Making the data long 
male_conditioned_diets_long <- male_conditioned_diets %>% 
  pivot_longer(cols = ("4;1":"1;4"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
male_conditioned_diets_summary <- male_conditioned_diets_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# creating a boxplot with the data that has been summarised
male_conditioned_diets_plot <- male_conditioned_diets_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = male_conditioned_diets_long,
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
