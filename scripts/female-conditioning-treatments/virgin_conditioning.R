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

one_to_four_virgin <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin.xlsx")

## Making the median data long 
four_to_one_virgin_long <- four_to_one_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
 

one_to_four_virgin_long <-one_to_four_virgin  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## creating a function to pull the median
fly_numbers_summary()<- function(data, group_col) {
  summary <- data %>%
    group_by(plate, {{ group_col }}) %>%
    summarise(median = median(fly_numbers))
  return(summary)
}

four_one_virgin_summary <- fly_numbers_summary(four_to_one_virgin_long, diet)
one_four_virgin_summary <- fly_numbers_summary(one_to_four_virgin_long, diet)


#### Creating a function for a plot which will allow me to run the same code for different datasets 
plot_virgin <- function(summary_data) {
  ggplot(summary_data, aes(x = diet, y = fly_numbers, fill = diet))+ 
    geom_boxplot()+
    theme_classic()+
    scale_fill_brewer(palette = "Set2")+
    labs(x = "Diet Condition",
         y = "Median number of flies per diet patch", 
         title = "")+
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
plot_virgin(four_one_virgin_summary) # 4:1
plot_virgin(one_four_virgin_summary) # 1:4 





