## virgin visualisation 

####### Virgin Conditioning - 4:1 ####### 
#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)
library(ggplot2)
library(ggpattern)

#### Virgin visualisation
#### Upload data for plot - median calculated 
four_to_one_virgin <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin.xlsx")

## Making the median data long 
four_to_one_virgin_long <- four_to_one_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") %>% 
  group_by(plate) %>%   mutate(fly_numbers = median(fly_numbers))

# Calculate median for each plate and diet
four_to_one_virgin_summary <- four_to_one_virgin_long %>%  
  group_by(plate, diet) %>% 
  summarise(fly_numbers = median(fly_numbers))  # Add median calculation

# Visualising the data - median data 
four_to_one_virgin_plot <- four_to_one_virgin_summary  %>% 
  ggplot(aes(x = diet, y = fly_numbers, fill = diet))+ 
  geom_boxplot()+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x = "Diet Condition",
       y = "Median number of flies per diet patch", 
       title = "4:1 OvoD1 Female Conditioned and Unconditioned Feeding")+
  theme(legend.position = "none")+ 
  ylim(-0.01,6)+
  geom_jitter(data = four_to_one_virgin_summary,
              aes(x = diet,
                  y = fly_numbers),
              fill = "skyblue",
              colour = "#3a3c3d",
              width = 0.2,
              shape = 21)



#### Creating a function for a plot which will allow me to run the same code for different datasets 
plot_virgin <- function(summary_data) {
  ggplot(summary_data, aes(x = diet, y = fly_numbers, fill = diet))+ 
    geom_boxplot()+
    theme_classic()+
    scale_fill_brewer(palette = "Set2")+
    labs(x = "Diet Condition",
         y = "Median number of flies per diet patch", 
         title = "4:1 OvoD1 Female Conditioned and Unconditioned Feeding")+
    theme(legend.position = "none")+ 
    ylim(-0.01, 6)+
    geom_jitter(data = summary_data,
                aes(x = diet,
                    y = fly_numbers),
                fill = "skyblue",
                colour = "#3a3c3d",
                width = 0.2,
                shape = 21)
}

## Running different data sets: 
plot_virgin(four_to_one_virgin_summary) # 4:1
plot_virgin(one_to_four_virgin_summary) 
plot_virgin(fourone_onefour_virgin_summary) 




