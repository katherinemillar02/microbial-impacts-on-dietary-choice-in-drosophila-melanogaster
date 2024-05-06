
############ VISUALISING THE DENSITY DATA ############



fourone_onefour_oviposition_50mm <- read_excel("data/50mm_combined_egg.xlsx")
# Making it long 
fourone_onefour_oviposition_50mm_long  <- fourone_onefour_oviposition_50mm  %>% 
  pivot_longer(cols = ("1:4 Unconditioned":"4:1 Conditioned"), names_to = "diet", values_to = "egg_numbers")

fiftydensity_combined <- oviposition_results(fourone_onefour_oviposition_50mm_long , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])


fourone_onefour_oviposition_90mm <- read_excel("data/90mm_combined_egg.xlsx")
# Making it long 
fourone_onefour_oviposition_90mm_long  <- fourone_onefour_oviposition_90mm  %>% 
  pivot_longer(cols = ("1:4 Unconditioned":"4:1 Conditioned"), names_to = "diet", values_to = "egg_numbers")

ninentydensity_combined <- oviposition_results(fourone_onefour_oviposition_90mm_long , boxplot_fill_colour = viridis_colors[c(1,2,3,4)])

fiftydensity_combined <- fiftydensity_combined + ggtitle("50 mm dish")
ninentydensity_combined <- ninentydensity_combined + ggtitle("90 mm dish")


ninentydensity_combined + fiftydensity_combined 