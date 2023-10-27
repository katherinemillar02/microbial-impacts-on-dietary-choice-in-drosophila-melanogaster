#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)


## Import Data
both_conditioned <- read_excel("data/both_conditioned.xlsx")

## Making the data long 
both_conditioned_long <- both_conditioned %>% 
  pivot_longer(cols = ("4;1m":"1;4nm"), names_to = "diet", values_to = "fly_numbers")

## Summarising the data; mean, sd, se
both_conditioned_summary <- both_conditioned_long  %>%  
  group_by(diet) %>% 
  summarise(mean = mean(fly_numbers),
            sd = sd(fly_numbers),
            n = n(),
            se = sd/sqrt(n))

## Visualising the data
# creating a boxplot with the data that has been summarised
both_conditioned_plot <- both_conditioned_summary %>% 
  ggplot(aes(x = diet, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "#FF6863",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "#FF6863",
                width = 0.2)+
  geom_jitter(data = both_conditioned_long,
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
