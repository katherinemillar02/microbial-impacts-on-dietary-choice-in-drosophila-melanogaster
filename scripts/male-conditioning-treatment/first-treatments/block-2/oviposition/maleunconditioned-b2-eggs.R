#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
unconditioned_b2_eggs <- read_excel("data/maleunconditioned-oviposition-b2.xlsx")

## Making the data long 
unconditioned_b2_eggs_long <- unconditioned_b2_eggs %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
unconditioned_b2_eggs_plot <- unconditioned_b2_eggs_long %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       
       y = "Median number of eggs per diet patch", 
       title = " Male Conditioned Treatment")+
  theme(legend.position="none")+ 
  ylim(-0.01,200)+
  geom_jitter(data =  unconditioned_b2_eggs_long,
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


conditioned_b2_eggs_plot + unconditioned_b2_eggs_plot

