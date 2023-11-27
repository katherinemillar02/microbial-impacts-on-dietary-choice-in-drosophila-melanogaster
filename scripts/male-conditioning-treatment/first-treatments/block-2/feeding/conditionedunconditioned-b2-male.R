#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
conditionedunconditioned_b2_median <- read_excel("data/c-uc-males-b2.xlsx")

## Making the data long 
conditionedunconditioned_b2_long <- conditionedunconditioned_b2_median   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
conditionedunconditioned_b2_plot <- conditionedunconditioned_b2_long %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male Conditioned/ Unconditioned Treatment Block 2")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  conditionedunconditioned_b2_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
