#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditioned_b1.2_median <- read_excel("data/male_block1.2_conditioned.xlsx")

## Making the data long 
conditioned_b1.2_median_long <- conditioned_b1.2_median %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Conditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
conditioned_b1.2_median_plot <- conditioned_b1.2_median_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=c("#1A85FF", "#D41159"))+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "")+
  theme(legend.position="none")+ 
  ylim(0.01,6)+
  geom_jitter(data =  conditioned_b1.2_median_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
