#### Packages ####
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(viridis)



#### Reading the data in ####


                                        #### 90 mm ####
# 4:1 
fourone_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1.xlsx")
# 1:4 
onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_1-4.xlsx")
# 4:1 and 1:4 
fourone_onefour_90mm <- read_excel("data/density_experiment/densityexperiment_90mm_4-1_1-4.xlsx")

## Pivoting data, making the data longer ## 
# 4:1 
fourone_90mm_long <- fourone_90mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 1:4 
onefour_90mm_long <- onefour_90mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
fourone_onefour_90mm_long <- fourone_onefour_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



 
                                    #### 50 mm ####
# 4:1 
fourone_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1.xlsx")
# 1:4 
onefour_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_1-4.xlsx")
# 4:1 and 1:4 
fourone_onefour_50mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1_1-4.xlsx")

## Pivoting data, making the data longer ##. 
# 4:1 
fourone_50mm_long <- fourone_50mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 1:4 
onefour_50mm_long <- onefour_50mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
fourone_onefour_50mm_long <- fourone_onefour_50mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")





#################################### Feeding Results Function Plot ####################################
feeding_results <- function(summary_data,boxplot_fill_colour ) {
  ggplot(summary_data, aes(x = diet, y = fly_numbers, fill = diet, pattern = diet))+ 
    # geom_jitter(aes(x = diet,
    #                 y = fly_numbers,
    #                 fill = diet),
    #             width = 0.1,
    #             shape = 1) +
    geom_boxplot(outlier.shape = NA)+
    geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                         color = "black",
                         pattern_fill = "white",
                         pattern_angle = 45,
                         pattern_density = 0.1,
                         pattern_spacing = 0.025,
                         pattern_key_scale_factor = 0.6,
                         outlier.shape = NA) +
    geom_point(aes(),
               size = 1,
               shape = 1,
               position = position_jitterdodge()) +
    theme_classic()+
    labs(x = "Diet Condition\nConditioned",
         y = "Flies per diet patch", 
         title = "")+
    scale_fill_manual(values = boxplot_fill_colour) + 
    scale_pattern_manual(values = c("circle", "none", "circle", "none")) +
    theme(legend.position = "none") +
    scale_x_discrete(labels = c(
      "4:1\nConditioned", 
      "4:1\nUnconditioned", 
      "1:4\nConditioned", 
      "1:4\nUnconditioned"))+
    ylim(-0.01, 6) 
}

##############################################################################################

#### Colour pallette 
viridis_colours <- inferno(10)


ninety_1_4 <- feeding_results(onefour_90mm_long , boxplot_fill_colour = viridis_colors[c(9,9)])
ninety_4_1 <- feeding_results(fourone_90mm_long , boxplot_fill_colour = viridis_colors[c(7,7)])
ninety_combined <- feeding_results(fourone_onefour_90mm_long, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])

ninety <- ninety_1_4 + ggtitle("90 mm") + ninety_4_1 


fifty_combined <- fifty_combined + ggtitle("35 mm")
ninety_combined <- ninety_combined + ggtitle("90 mm")


fifty_combined + ninety_combined

ninety_combined /
  fifty_combined

fifty_1_4 <- feeding_results(onefour_50mm_long , boxplot_fill_colour  = viridis_colors[c(9,9)])
fifty_4_1 <- feeding_results(fourone_50mm_long , boxplot_fill_colour = viridis_colors[c(7,7)])
fifty_combined <- feeding_results(fourone_onefour_50mm_long, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])

fifty <- fifty_1_4 + ggtitle("35 mm") + fifty_4_1

ggarrange(fifty_1_4 + ggtitle("35 mm"), fifty_4_1, ninety_1_4 + ggtitle("90 mm"), ninety_4_1, ncol = 4, nrow = 1)

