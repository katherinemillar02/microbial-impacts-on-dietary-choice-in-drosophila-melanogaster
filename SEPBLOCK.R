
#### Packages ####
source("packages.R")
source("scripts/dietary-choice/dietarychoice.dataread.R")
#### #### #### #### #### --


## Combined Block Data Visualisation ####

  ## Visualising the individual blocks as data shows these to be significant ## 

viridis_colours <- viridis(10)

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
         y = "Female flies per diet patch", 
         title = "") +
    scale_fill_manual(values = boxplot_fill_colour) + 
    scale_pattern_manual(values = c("circle", "none", "circle", "none")) +
    theme(legend.position = "none") +
    ylim(-0.01, 150) +
    facet_wrap(~ nutrient_composition, scales = "free_x", nrow = 1, strip.position = "bottom") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(size = 12)
    )
}


## Oviposition: 

# 1:4 OvoD1

# Block 1 
ovod1_b1_onefour <- oviposition_results(onefour_ovod1_oviposition_b1_long, boxplot_fill_colour = viridis_colours[c(9,9)])

# Block 2
ovod1_b2_onefour <- oviposition_results(onefour_ovod1_oviposition_b2_long, boxplot_fill_colour = viridis_colours[c(9,9)])


# 4:1 OvoD1

# Block 1 
ovod1_b1_fourone <- oviposition_results(fourone_ovod1_oviposition_b1_long, boxplot_fill_colour = viridis_colours[c(7,7)])

# Block 2
ovod1_b2_fourone <- oviposition_results(fourone_ovod1_oviposition_b1_long, boxplot_fill_colour = viridis_colours[c(7,7)])




## Combined 4:1 and 1:4 

# Block 1
ovod1_b1_combined <- oviposition_results(fourone_onefour_oviposition_ovod1_b1_long , boxplot_fill_colour = viridis_colours[c(9,9,7,7)])

# Block 2 
ovod1_b2_combined <- oviposition_results(fourone_onefour_oviposition_ovod1_b2_long ,  boxplot_fill_colour = viridis_colours[c(9,9,7,7)])




## Adding titles to the plots 
ovod1_b1_onefour <- ovod1_b1_onefour + ggtitle("OvoD1 Block 1")
ovod1_b1_fourone <- ovod1_b1_fourone + ggtitle("OvoD1 Block 1")
ovod1_b1_combined <- ovod1_b1_combined + ggtitle("OvoD1 Block 1")

ovod1_b2_onefour <- ovod1_b2_onefour + ggtitle("OvoD1 Block 2")
ovod1_b2_fourone <- ovod1_b2_fourone + ggtitle("OvoD1 Block 2")
ovod1_b2_combined <- ovod1_b2_combined + ggtitle("OvoD1 Block 2")




## OvoD1 Block plot 
ovod1_female_oviposition <- grid.arrange(
  ovod1_b1_onefour, ovod1_b1_fourone, ovod1_b1_combined,
  ovod1_b2_onefour, ovod1_b2_fourone, ovod1_b2_combined,
  ncol = 3,
  nrow = 2,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1)
)





############ --
## Virgin Female ####
############ --

## 4:1 Virgin 

# Block 2 
virgin_b2_fourone <- oviposition_results( onefour_virgin_oviposition_b1_long , boxplot_fill_colour  = viridis_colours[3:4])

# Block 3 
virgin_b3_fourone <- oviposition_results( onefour_virgin_oviposition_b1_long, boxplot_fill_colour = viridis_colours[3:4])

# Block 4 
virgin_b4_fourone <- oviposition_results( fourone_ovi_vf_b4, boxplot_fill_colour = viridis_colours[3:4])



# 1:4 Virgin

# Block 2 
virgin_b2_onefour <- oviposition_results(onefour_ovi_vf_b2  , boxplot_fill_colour = viridis_colours[1:2])

# Block 3
virgin_b3_onefour <- oviposition_results(onefour_ovi_vf_b3 , boxplot_fill_colour  = viridis_colours[1:2])

# Block 4
virgin_b4_onefour <- oviposition_results(onefour_ovi_vf_b4 , boxplot_fill_colour  = viridis_colours[1:2])


## 4:1 + 1:4 Virgin 

# Block 2
virgin_b2_combined <- oviposition_results(fourone_onefour_ovi_vf_b2 , boxplot_fill_colour  = viridis_colours[c(1:4)])

# Block 3
virgin_b3_combined <- oviposition_results(fourone_onefour_ovi_vf_b3 , boxplot_fill_colour  = viridis_colours[c(1:4)])

# Block 4
virgin_b4_combined <- oviposition_results(fourone_onefour_ovi_vf_b4, boxplot_fill_colour  = viridis_colours[c(1:4)])



## virgin


v_b2_onefour <- virgin_b2_onefour + ggtitle("Virgin Block 2")
v_b2_fourone <- virgin_b2_fourone + ggtitle("Virgin Block 2")
v_b2_combined <- virgin_b2_combined + ggtitle("Virgin Block 2")

v_b3_onefour <- virgin_b3_onefour + ggtitle("Virgin Block 3")
v_b3_fourone <- virgin_b3_fourone + ggtitle("Virgin Block 3")
v_b3_combined <- virgin_b3_combined + ggtitle("Virgin Block 3")

v_b4_onefour <- virgin_b4_onefour + ggtitle("Virgin Block 4")
v_b4_fourone <- virgin_b4_fourone + ggtitle("Virgin Block 4")
v_b4_combined <- virgin_b4_combined + ggtitle("Virgin Block 4")


## this code works



virgin_female_oviposition <- grid.arrange(
  v_b2_onefour, v_b2_fourone, v_b2_combined,
  v_b3_onefour, v_b3_fourone, v_b3_combined,
  v_b4_onefour, v_b4_fourone, v_b4_combined,
  ncol = 3,
  nrow = 3,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1, 1)
)

############ --
## Male ####
############ -- 

## 1:4 

# Block 1
male_b1_onefour <- oviposition_results(onefour_ovi_m_b1, boxplot_fill_colour = viridis_colours[1:2])

# Block 2
male_b2_onefour <- oviposition_results(onefour_ovi_m_b2, boxplot_fill_colour = viridis_colours[1:2])

## 4:1

# Block 1 
male_b1_fourone <- oviposition_results(fourone_ovi_m_b1, boxplot_fill_colour = viridis_colours[3:4])

# Block 2 
male_b2_fourone <- oviposition_results(fourone_ovi_m_b2, boxplot_fill_colour = viridis_colours[3:4])


## 1:4 and 4:1

# Block 1 
male_b1_fourone_onefour <- oviposition_results(fourone_onefour_ovi_m_b1, boxplot_fill_colour = viridis_colours[1:4])

# Block 2
male_b2_fourone_onefour <- oviposition_results(fourone_onefour_ovi_m_b2, boxplot_fill_colour = viridis_colours[1:4])


## Adding titles to blocks 
male_b1_fourone <- male_b1_fourone + ggtitle("Male Block 1")
male_b2_fourone <- male_b2_fourone + ggtitle("Male Block 2")
male_b1_onefour <- male_b1_onefour + ggtitle("Male Block 1")
male_b2_onefour <- male_b2_onefour + ggtitle("Male Block 2")
male_b1_fourone_onefour <- male_b1_fourone_onefour + ggtitle("Male Block 1")
male_b2_fourone_onefour <- male_b2_fourone_onefour + ggtitle("Male Block 2")



# male 
male_oviposition <- grid.arrange(
  male_b1_fourone, male_b1_onefour, male_b1_fourone_onefour,
  male_b2_fourone, male_b2_onefour, male_b2_fourone_onefour,
  ncol = 3,
  nrow = 2,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1)
)













