####### OvoD1 Conditioning - 1:4 


#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)

#### Upload data
one_to_four_OvoD1 <- read_excel("data/OvoD1_1-4_median.xlsx")

## Making the data long 
one_to_four_OvoD1_long <- one_to_four_OvoD1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


## Visualising the data 
one_to_four_OvoD1_plot <- one_to_four_OvoD1_long  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 Diets")+
  theme(legend.position="none")+ 
  ylim(-0.01,2)+
  geom_jitter(data =  one_to_four_OvoD1_long,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)
## might have to look at mean average for this plot. 




