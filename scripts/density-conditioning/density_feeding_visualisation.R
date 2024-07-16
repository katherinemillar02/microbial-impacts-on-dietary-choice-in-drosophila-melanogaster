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



 
                                    #### 35 mm ####
# 4:1 
fourone_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1.xlsx")
# 1:4 
onefour_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_1-4.xlsx")
# 4:1 and 1:4 
fourone_onefour_35mm <- read_excel("data/density_experiment/densityexperiment_50mm_4-1_1-4.xlsx")

## Pivoting data, making the data longer ##. 
# 4:1 
fourone_35mm_long <- fourone_35mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 1:4 
onefour_35mm_long <- onefour_35mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
fourone_onefour_35mm_long <- fourone_onefour_35mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")





#################################### Feeding Results Function Plot ####################################
########################################################################################################################### --

feeding_results <- function(data, boxplot_fill_colour) {
  # Ensure the data has the necessary columns for faceting
  data$nutrient_composition <- ifelse(grepl("4:1", data$diet), "4:1", "1:4")
  data$condition <- ifelse(grepl("Conditioned", data$diet), "Conditioned", "Unconditioned")
  data$combined_factor <- paste(data$nutrient_composition, data$condition, sep = " ")
  ggplot(data, aes(x = condition, y = fly_numbers, fill = combined_factor, pattern = combined_factor)) + 
    geom_boxplot(outlier.shape = NA) +
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
    theme_classic() +
    labs(x = "",
         y = "Female flies per diet patch", 
         title = "") +
    scale_fill_manual(values = boxplot_fill_colour) + 
    scale_pattern_manual(values = c("circle", "none", "circle", "none")) +
    theme(legend.position = "none") +
    ylim(-0.01, 6) +
    facet_wrap(~ nutrient_composition, scales = "free_x", nrow = 1, strip.position = "bottom") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(size = 12)
    )
}


##############################################################################################

#### Colour pallette 
viridis_colors <- inferno(10)


## Running the boxplots.

## 90 mm
ninety_1_4_feed <- feeding_results(onefour_90mm_long , boxplot_fill_colour = viridis_colors[c(9,9)])
ninety_4_1_feed <- feeding_results(fourone_90mm_long , boxplot_fill_colour = viridis_colors[c(7,7)])
ninety_combined_feed <- feeding_results(fourone_onefour_90mm_long, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])



# 35 mm 
fifty_1_4_feed <- feeding_results(onefour_35mm_long , boxplot_fill_colour  = viridis_colors[c(9,9)])
fifty_4_1_feed <- feeding_results(fourone_35mm_long , boxplot_fill_colour = viridis_colors[c(7,7)])
fifty_combined_feed <- feeding_results(fourone_onefour_35mm_long, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])

## Adding titles to the boxplots.
ninety_feed <- ninety_1_4_feed + ggtitle("90 mm") + ninety_4_1_feed 
fifty_feed <- fifty_1_4_feed + ggtitle("35 mm") + fifty_4_1_feed
## patchwork design
ninety_feed /
  fifty_feed

## Adding titles to the boxplots.
fifty_combined_feed <- fifty_combined_feed + ggtitle("35 mm")
ninety_combined_feed <- ninety_combined_feed + ggtitle("90 mm")
## patchwork design
ninety_combined_feed /
  fifty_combined_feed
