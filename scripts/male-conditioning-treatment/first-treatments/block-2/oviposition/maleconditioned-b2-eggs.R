#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditioned_b2_eggs <- read_excel("data/maleconditioned-oviposition-b2.xlsx")

## Making the data long 
conditioned_b2_eggs_long <- conditioned_b2_eggs %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
conditioned_b2_eggs_plot <- conditioned_b2_eggs_long   %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = " Male Conditioned Treatment")+
  theme(legend.position="none")+ 
  ylim(-0.01,200)+
  geom_jitter(data =  conditioned_b2_eggs_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
