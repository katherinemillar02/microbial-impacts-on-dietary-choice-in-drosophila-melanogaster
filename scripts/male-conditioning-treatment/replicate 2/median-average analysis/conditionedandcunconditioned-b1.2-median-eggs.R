#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditionedunconditioned_b1.2_median_egg <- read_excel("data/ConditionedandUnconditioned_Treatment_Egg_B1.2.xlsx")



## Making the data long 
conditionedunconditioned_b1.2_median_egg_long  <- conditionedunconditioned_b1.2_median_egg %>% 
  pivot_longer(cols = ("1:4 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## Median boxplot --
conditionedunconditioned_b1.2_median_egg_plot <- conditionedunconditioned_b1.2_median_egg_long  %>% 
  ggplot(aes(x = diet, y = egg_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  labs(x = "Diet Condition",
       y = "Median number of eggs per diet patch", 
       title = "Male Unconditioned and Conditioned Egg Treatment")+
  theme(legend.position="none")+
  ylim(0,200)+
  geom_jitter(data =  conditionedunconditioned_b1.2_median_egg_long , 
              aes(x = diet,
                  y = egg_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)


