
############ VISUALISING THE DENSITY DATA ############



fourone_onefour_oviposition_50mm <- read_excel("data/density_experiment/50mm_combined_egg.xlsx")

fourone_oviposition_50mm <- read_excel("data/density_experiment/50mm_4-1_oviposition.xlsx")

onefour_oviposition_50mm <- read_excel("data/density_experiment/50mm_1-4_oviposition.xlsx")

# Making it long 
fourone_onefour_oviposition_50mm_long  <- fourone_onefour_oviposition_50mm  %>% 
  pivot_longer(cols = ("1:4 Unconditioned":"4:1 Conditioned"), names_to = "diet", values_to = "egg_numbers")

fourone_oviposition_50mm_long  <- fourone_oviposition_50mm %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

onefour_oviposition_50mm_long  <- onefour_oviposition_50mm %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

fiftydensity_combined <- oviposition_results(fourone_onefour_oviposition_50mm_long , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])
fiftydensity_fourone <- oviposition_results(fourone_oviposition_50mm_long , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])
fiftydensity_onfour <- oviposition_results(onefour_oviposition_50mm_long  , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])





fourone_onefour_oviposition_90mm <- read_excel("data/density_experiment/90mm_combined_egg.xlsx")

fourone_oviposition_90mm <- read_excel("data/density_experiment/90mm_4-1_oviposition.xlsx")

onefour_oviposition_90mm <- read_excel("data/density_experiment/90mm_1-4_oviposition.xlsx")

# Making it long 
fourone_onefour_oviposition_90mm_long  <- fourone_onefour_oviposition_90mm  %>% 
  pivot_longer(cols = ("1:4 Unconditioned":"4:1 Conditioned"), names_to = "diet", values_to = "egg_numbers")

onefour_oviposition_90mm_long  <- onefour_oviposition_90mm  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


fourone_oviposition_90mm_long  <- fourone_oviposition_90mm  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


ninentydensity_combined <- oviposition_results(fourone_onefour_oviposition_90mm_long , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])
ninentydensity_fourone <- oviposition_results(fourone_oviposition_90mm_long , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])
ninentydensity_onefour <- oviposition_results(onefour_oviposition_90mm_long , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])


fiftydensity_combined <- fiftydensity_combined + ggtitle("50 mm dish")
ninentydensity_combined <- ninentydensity_combined + ggtitle("90 mm dish")

fiftydensity <- fiftydensity_fourone + fiftydensity_onfour + fiftydensity_combined
ninetydensity <- ninentydensity_fourone + ninentydensity_onefour + ninentydensity_combined 

ninetydensity/ 
  fiftydensity

oviposition <- grid.arrange(
  fiftydensity_onfour + ggtitle("50 mm"), fiftydensity_fourone, fiftydensity_combined,
  ninentydensity_onefour + ggtitle("90 mm"), ninentydensity_fourone, ninentydensity_combined,
  ncol = 3,  
  nrow = 2,  
  widths = c(0.5, 0.5, 1), 
  heights = c(1, 1)  
)



oviposition_results <- function(summary_data,boxplot_fill_colour ) {
  ggplot(summary_data, aes(x = diet, y = egg_numbers, fill = diet)) + #, pattern = diet))+ 
    # geom_jitter(aes(x = diet,
    #                 y = fly_numbers,
    #                 fill = diet),
    #             width = 0.1,
    #             shape = 1) +
    geom_boxplot()+
    # geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
    #                     color = "black",
    #                     pattern_fill = "white",
    #                     pattern_angle = 45,
    #                     pattern_density = 0.1,
    #                     pattern_spacing = 0.025,
    #                     pattern_key_scale_factor = 0.6) +
    geom_point(aes(),
               size = 1,
               shape = 1,
               position = position_jitterdodge()) +
    theme_classic()+
    labs(x = "Diet Condition",
         y = "Flies", 
         title = "")+
    scale_fill_manual(values = boxplot_fill_colour) +  # Set fill colors for the boxplot
    # scale_pattern_manual(values = c("stripe", "none", "stripe", "none")) +
    theme(legend.position = "none") +
    ylim(-0.01, 200) 
  
}
