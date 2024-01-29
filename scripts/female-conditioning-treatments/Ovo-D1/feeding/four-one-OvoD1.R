####### OvoD1 Conditioning - 4:1 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
four_to_one_OvoD1 <- read_excel("data/OvoD1-4_1-median.xlsx")

## Making the data long 
four_to_one_OvoD1_long <- four_to_one_OvoD1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Visualising the data 
four_to_one_OvoD1_plot <- four_to_one_OvoD1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "1:4 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,4)+
  geom_jitter(data =  four_to_one_OvoD1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



