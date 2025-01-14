#### Packages ####
source("packages.R")
source("scripts/dietary-choice/dietarychoice.dataread.R")
#### #### #### #### #### --



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  #####  #####  ##### 



###### ###### ###### ###### FEEDING DATA VISUALISATION ###### ###### ###### ###### 
###### 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊📊📊📊###### 


# Creating a function for a boxplot, 
# this will allow one to use the feeding data for different data sets...

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


############################################################################################

#### FEEDING ####
#### RELATIVE (FOUR-CHOICE) ####

#### MALE 

male_relative_feeding <- feeding_results(fourone_onefour_male_long,  boxplot_fill_colour = viridis_colors[c(7,7,9,9)])

#### VIRGIN FEMALE 
virginfemale_relative_feeding <- feeding_results(combined_vf,  boxplot_fill_colour = viridis_colors[c(7,7,9,9)])

#### OVOD1 FEMAlE
ovod1female_relative_feeding <- feeding_results(combined_of,  boxplot_fill_colour = viridis_colors[c(7,7,9,9)])





# Absolute Feeding #

## 1:4 

#### MALE 
male_1_4_feeding <- feeding_results(onefour_male_feeding_long,  boxplot_fill_colour = viridis_colors[c(9,9)])
#### VIRGIN FEMALE 
virginfemale_1_4_feeding <- oviposition_results(onefour_virgin_feeding_long,  boxplot_fill_colour = viridis_colors[c(9,9)])
#### OVOD1 FEMALE 
ovod1female_1_4_feeding <- oviposition_results(onefour_ovod1_feeding_long,  boxplot_fill_colour = viridis_colors[c(9,9)])


## 4:1 

#### MALE 
male_4_1_feeding <- oviposition_results(fourone_male_feeding_long,  boxplot_fill_colour = viridis_colors[c(7,7)])
#### VIRGIN FEMALE 
virginfemale_4_1_feeding <- oviposition_results(fourone_virgin_feeding_long,  boxplot_fill_colour = viridis_colors[c(7,7)])
#### OVOD1 FEMALE 
ovod1female_4_1_feeding <- oviposition_results(fourone_ovod1_feeding_long,  boxplot_fill_colour = viridis_colors[c(7,7)])





###### ###### ###### ###### OVIPOSITION DATA VISUALISATION ###### ###### ###### ###### 
###### 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊📊📊📊###### 


# Creating a function for a boxplot, 
# this will allow one to use the feeding data for different data sets...

########################################################################################################################### --

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


############################################################################################

# Relative Oviposition 
#### MALE 
male_relative_oviposition <- oviposition_results(combined_ovi_m,  boxplot_fill_colour = viridis_colors[c(7,7,9,9)])

#### VIRGIN FEMALE 
virginfemale_relative_oviposition <- oviposition_results(combined_ovi_v,  boxplot_fill_colour = viridis_colors[c(7,7,9,9)])

#### OVOD1 FEMAlE
ovod1female_relative_oviposition <- oviposition_results(combined_of_egg,  boxplot_fill_colour = viridis_colors[c(7,7,9,9)])


# Absolute Oviposition #

## 1:4 

#### MALE 
male_1_4_oviposition <- oviposition_results(onefour_male_oviposition_long,  boxplot_fill_colour = viridis_colors[c(9,9)])
#### VIRGIN FEMALE 
virginfemale_1_4_oviposition <- oviposition_results(onefour_virgin_oviposition_long,  boxplot_fill_colour = viridis_colors[c(9,9)])
#### OVOD1 FEMALE 
ovod1female_1_4_oviposition <- oviposition_results(onefour_ovod1_oviposition_long,  boxplot_fill_colour = viridis_colors[c(9,9)])


## 4:1 

#### MALE 
male_4_1_oviposition <- oviposition_results(fourone_male_oviposition_long,  boxplot_fill_colour = viridis_colors[c(7,7)])
#### VIRGIN FEMALE 
virginfemale_4_1_oviposition <- oviposition_results(fourone_virgin_oviposition_long,  boxplot_fill_colour = viridis_colors[c(7,7)])
#### OVOD1 FEMALE 
ovod1female_4_1_oviposition <- oviposition_results(fourone_ovod1_oviposition_long,  boxplot_fill_colour = viridis_colors[c(7,7)])
