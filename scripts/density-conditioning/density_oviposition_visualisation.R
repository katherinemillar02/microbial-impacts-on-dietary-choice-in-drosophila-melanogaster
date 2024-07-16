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

# 90 mm 
fourone_90mm <- read_excel("data/density_experiment/90mm_4-1_oviposition_2.xlsx")
onefour_90mm <- read_excel("data/density_experiment/90mm_1-4_oviposition_2.xlsx")
fourone_onefour_90mm <- read_excel("data/density_experiment/90mm_combined_oviposition_2.xlsx")
# Adding variables to the data
fourone_90mm_long <- fourone_90mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
onefour_90mm_long <- onefour_90mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
fourone_onefour_90mm_long <- fourone_onefour_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# 50 mm 
fourone_35mm <- read_excel("data/density_experiment/50mm_4-1_oviposition_2.xlsx")
onefour_35mm <- read_excel("data/density_experiment/50mm_1-4_oviposition_2.xlsx")
fourone_onefour_35mm <- read_excel("data/density_experiment/50mm_combined_oviposition_2.xlsx")
# Adding variables to the data
fourone_35mm_long <- fourone_35mm   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
onefour_35mm_long <- onefour_35mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
fourone_onefour_35mm_long <- fourone_onefour_35mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")





#################################### Feeding Results Function Plot ####################################

oviposition_results <- function(data, boxplot_fill_colour) {
  # Ensure the data has the necessary columns for faceting
  data$nutrient_composition <- ifelse(grepl("4:1", data$diet), "4:1", "1:4")
  data$condition <- ifelse(grepl("Conditioned", data$diet), "Conditioned", "Unconditioned")
  data$combined_factor <- paste(data$nutrient_composition, data$condition, sep = " ")
  ggplot(data, aes(x = condition, y = egg_numbers, fill = combined_factor, pattern = combined_factor)) + 
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
         y = "Eggs laid per diet patch", 
         title = "") +
    scale_fill_manual(values = boxplot_fill_colour) + 
    scale_pattern_manual(values = c("circle", "none", "circle", "none")) +
    theme(legend.position = "none") +
    ylim(0, 150) +
    facet_wrap(~ nutrient_composition, scales = "free_x", nrow = 1, strip.position = "bottom") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(size = 12)
    )
}



#####################################################################################



## Choosing a colour palette:
viridis_colours <- inferno(10)


## Relative assays - 90 mm
ninety_1_4_egg <- oviposition_results(onefour_90mm_long , boxplot_fill_colour = viridis_colors[c(9,9)])
ninety_4_1_egg <- oviposition_results(fourone_90mm_long , boxplot_fill_colour = viridis_colors[c(7,7)])
 
## Relative assays - 35 mm
fifty_1_4_egg <- oviposition_results(onefour_35mm_long , boxplot_fill_colour = viridis_colors[c(9,9)])
fifty_4_1_egg <- oviposition_results(fourone_35mm_long , boxplot_fill_colour = viridis_colors[c(7,7)])

## Adding titles 
fifty_1_4_egg <- fifty_1_4 + ggtitle("35 mm")
ninety_1_4_egg <- ninety_1_4 + ggtitle("90 mm")


# Relative assays combined
nine_egg <- ninety_1_4_egg + ninety_4_1_egg
three_egg <- fifty_1_4_egg + fifty_4_1_egg

## Relative assays 
nine_egg / 
  three_egg






## Absolute assays - 90 mm
ninety_combined_egg <- oviposition_results(fourone_onefour_90mm_long, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])

## Absolute assays - 35 mm 
fifty_combined_egg <- oviposition_results(fourone_onefour_35mm_long, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])

## Adding titles 
ninety_combined_egg <- ninety_combined_egg + ggtitle("90 mm")
fifty_combined_egg <- fifty_combined_egg + ggtitle("35 mm")


# Absolute assays
ninety_combined_egg /
  fifty_combined_egg


  



