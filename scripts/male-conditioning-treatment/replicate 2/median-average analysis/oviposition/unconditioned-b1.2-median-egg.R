#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
unconditioned_b1.2_median_egg <- read_excel("data/Unconditioned_Treatment_Egg_B1.2.xlsx")



## Making the data long 
unconditioned_b1.2_median_egg_long  <- unconditioned_b1.2_median_egg  %>% 
  pivot_longer(cols = ("1:4 Unconditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
unconditioned_b1.2_median_egg_plot <- unconditioned_b1.2_median_egg_long %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Male Unconditioned Egg Treatment")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data = unconditioned_b1.2_median_egg_long, 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
