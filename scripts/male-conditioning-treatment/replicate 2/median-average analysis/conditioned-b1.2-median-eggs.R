#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)



#### Upload data
conditioned_b1.2_median_egg <- read_excel("data/Conditioned_Treatment_Egg_B1.2.xlsx")



## Making the data long 
conditioned_b1.2_median_egg_long  <-conditioned_b1.2_median_egg %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
conditioned_b1.2_median_egg_plot <- conditioned_b1.2_median_egg_long  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Male Unconditioned Treatment")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data =  conditioned_b1.2_median_egg_long, 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


