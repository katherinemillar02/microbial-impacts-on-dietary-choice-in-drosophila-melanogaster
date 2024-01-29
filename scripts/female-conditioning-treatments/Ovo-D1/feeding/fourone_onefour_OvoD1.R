####### OvoD1 Conditioning - 4:1 + 1:4 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
fourone_onefour_OvoD1 <- read_excel("data/OvoD1_4-1_1-4_median.xlsx")

## Making the data long 
fourone_onefour_OvoD1_long <- fourone_onefour_OvoD1   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Visualising the data 
fourone_onefour_OvoD1_plot <- fourone_onefour_OvoD1_long %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 + 1:4 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,6)+
  geom_jitter(data =  fourone_onefour_OvoD1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
