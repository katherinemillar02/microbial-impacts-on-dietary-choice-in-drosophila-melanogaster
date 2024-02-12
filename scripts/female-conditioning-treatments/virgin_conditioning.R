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
fourone_onefour_virgin <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin.xlsx")

## Making the median data long 
four_to_one_virgin_long <- four_to_one_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
 
#´
one_to_four_virgin_long <- one_to_four_virgin  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
#
fourone_onefour_virgin_long <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## creating a function to pull the median
fly_numbers_summary()<- function(data, group_col) {
  summary <- data %>%
    group_by(plate, {{ group_col }}) %>%
    summarise(median = median(fly_numbers))
  return(summary)
}

four_one_virgin_summary <- fly_numbers_summary(four_to_one_virgin_long, diet)
one_four_virgin_summary <- fly_numbers_summary(one_to_four_virgin_long, diet)
fourone_onefour_virgin_summary <- fly_numbers_summary(fourone_onefour_virgin_long, diet)


#### Creating a function for a plot which will allow me to run the same code for different datasets 
plot_virgin <- function(summary_data,boxplot_fill_color ) {
  ggplot(summary_data, aes(x = diet, y = fly_numbers, fill = diet, pattern = diet))+ 
    geom_boxplot(fill = "white", color = "black")+
    geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
                         color = "black",
                         pattern_fill = "white",
                         pattern_angle = 45,
                         pattern_density = 0.1,
                         pattern_spacing = 0.025,
                         pattern_key_scale_factor = 0.6) +
    theme_classic()+
    labs(x = "Diet Condition",
         y = "Median number of flies per diet patch", 
         title = "")+
    scale_fill_manual(values = boxplot_fill_color) +  # Set fill colors for the boxplot
    scale_pattern_manual(values = c("stripe", "none", "stripe", "none")) +
    theme(legend.position = "none") +
    ylim(-0.01, 6) +
    geom_jitter(data = summary_data,
                aes(x = diet,
                    y = fly_numbers,
                    fill = diet),
                width = 0.1,
                shape = 1)
  
}

## Code will allow one to see each of the plots
plot_virgin(one_to_four_virgin_long, boxplot_fill_color = c("lightblue", "lightblue"))
plot_virgin(four_to_one_virgin_long, boxplot_fill_color = c("#FDECCD","#FDECCD")) 
plot_virgin(fourone_onefour_virgin_long, boxplot_fill_color = c("lightblue", "lightblue","#FDECCD","#FDECCD"))



