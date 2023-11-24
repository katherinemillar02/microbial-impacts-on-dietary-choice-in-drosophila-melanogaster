#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
unconditioned_b2_median <- read_excel("data/uc-males-b2.xlsx")

## Making the data long 
unconditioned_b2_median_long  <- unconditioned_b2_median    %>% 
  pivot_longer(cols = ("4:1 Unconditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Median boxplot --
unconditioned_b2_median_plot <- unconditioned_b2_median_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = " Male Unconditioned Treatment Block 2")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  unconditioned_b2_median_long ,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)

conditioned_b2_median_plot + unconditioned_b2_median_plot
