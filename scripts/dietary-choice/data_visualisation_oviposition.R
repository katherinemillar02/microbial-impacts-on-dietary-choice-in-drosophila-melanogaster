#### INSTALL PACKAGES #### 📦📦📦📦
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness) 
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(viridis)
######################## 📦📦📦📦 


########################### -- 
# OVIPOSITION DATA 🥚🥚🥚🥚
########################### -- 




################### --
## OvoD1 FEMALES #### 
################### --

## Reading the data in ####


########### 4:1  ####

## Reading B1 in 
four_to_one_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_oviposition_ovod1_b1.xlsx")
## Adding DF names to B1 
four_one_b1_egg <- four_to_one_oviposition_ovod1_b1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


## Reading B2 in 
four_to_one_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_oviposition_ovod1_b2.xlsx")
## Adding DF names to B2
four_one_b2_egg <- four_to_one_oviposition_ovod1_b2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



# Binding the separate 4:1 data's
fourone_oviposition_of <- rbind(four_to_one_oviposition_ovod1_b1, four_to_one_oviposition_ovod1_b2)

####################################################### --





############ 1:4  ####

## Reading B1 in:
one_to_four_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/1-4_oviposition_ovod1_b1.xlsx")
## Adding DF names to B1
one_four_b1_egg <- one_to_four_oviposition_ovod1_b1 %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

## Reading B2 in:
one_to_four_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/1-4_oviposition_ovod1_b2.xlsx")
## Adding DF names to B2
one_four_b2_egg <- one_to_four_oviposition_ovod1_b2 %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



# Binding the separate 1:4 data 
onefour_oviposition_of <- rbind(one_to_four_oviposition_ovod1_b1, one_to_four_oviposition_ovod1_b2)




###### 4:1 + 1:4 ####

# 4:1./1:4

# reading the data in - block 1
fourone_onefour_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
# Making it long
fourone_onefour_ovo_b1 <- fourone_onefour_oviposition_ovod1_b1  %>%
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

## reading the data in - Block 2 
fourone_onefour_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")
# Making it long - Block 2
fourone_onefour_ovo_b2  <- fourone_onefour_oviposition_ovod1_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")






# Binding the 4:1/1:4 data
fourone_onefour_oviposition_of <- rbind(fourone_onefour_oviposition_ovod1_b1, fourone_onefour_oviposition_ovod1_b2)





## USING PIVOT LONGER TO ADD A VARIABLE TO A DATA FILE ## THE COMBINED DATA 
## OvoD1
# 4:1
fourone_ovi_of <- fourone_oviposition_of %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 1:4 
onefour_ovi_of <- onefour_oviposition_of %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
combined_ovi_of  <- fourone_onefour_oviposition_of   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")









############################### --
## VIRGIN WILD TYPE FEMALES ####
############################## --
# Virgin Conditioning Oviposition data is not included as was done differently 

## 4:1 ####


# 4:1 Virgin - Block 2 
four_to_one_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b2.xlsx")

# 4:1 - oviposition  - Block 2 - adding variable names
fourone_ovi_vf_b2 <- four_to_one_oviposition_virgin_2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

# 4:1 Virgin - Block 3
four_to_one_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b3.xlsx")

# 4:1 - oviposition  - Block 3 - adding variable names
fourone_ovi_vf_b3 <- four_to_one_oviposition_virgin_3 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

# 4:1 Virgin - Block 4
four_to_one_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b4.xlsx")

# 4:1 - oviposition  - Block 4 - adding variable names
fourone_ovi_vf_b4 <- four_to_one_oviposition_virgin_4 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 





# Binding the 4:1 data - where the blocks are combined
four_to_one_oviposition_virgin <- rbind(four_to_one_oviposition_virgin_2, four_to_one_oviposition_virgin_3, four_to_one_oviposition_virgin_4)





# 1:4  ####

## Reading the data in - Block 2
one_to_four_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b2.xlsx")

# 1:4 - oviposition  - Block 2 - adding variable names
onefour_ovi_vf_b2 <- one_to_four_oviposition_virgin_2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



## Reading the data in - Block 3
one_to_four_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b3.xlsx")

# 1:4 - oviposition  - Block 3 - adding variable names
onefour_ovi_vf_b3 <- one_to_four_oviposition_virgin_3  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 




## Reading the data in - Block 4
one_to_four_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b4.xlsx")

# 1:4 - oviposition  - Block 4 - adding variable names
onefour_ovi_vf_b4 <- one_to_four_oviposition_virgin_4  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 




# Binding the 1:4 data - for the combined scripts 
one_to_four_oviposition_virgin <- rbind(one_to_four_oviposition_virgin_2,one_to_four_oviposition_virgin_3, one_to_four_oviposition_virgin_4)





# 4:1 and 1:4  ####

## Reading the data in - Block 2 
fourone_onefour_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx")

# Adding variable names to the data - Block 2
fourone_onefour_ovi_vf_b2 <- fourone_onefour_oviposition_virgin_2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

## Reading the data in - Block 3
fourone_onefour_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx")

# Adding variable names to the data - Block 3
fourone_onefour_ovi_vf_b3 <- fourone_onefour_oviposition_virgin_3 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


## Reading the data in - Block 4 
fourone_onefour_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx")

# Adding variable names to the data - Block 4
fourone_onefour_ovi_vf_b4 <- fourone_onefour_oviposition_virgin_4 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



# Binding the 4:1/1:4 data
fourone_onefour_oviposition_virgin <- rbind( fourone_onefour_oviposition_virgin_2, fourone_onefour_oviposition_virgin_3, fourone_onefour_oviposition_virgin_4)


####
## Adding a data variable using pivotlonger() - redesigning the data frame 
## Virgin
# 4:1
fourone_ovi_vf <- four_to_one_oviposition_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 1:4 
onefour_ovi_vf <- one_to_four_oviposition_virgin %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
combined_ovi_vf <- fourone_onefour_oviposition_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")
####--- 







####################### ####################### ####################### ####################### ####################### --





####################### ####################### ####################### ####################### #######################  --



####################### ---
## WILD TYPE MALES ######
###################### ---

# 4:1 ####

## Reading the data in - Block 1
four_to_one_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_b1_oviposition.xlsx")

## Adding variable names to the data - Block 1
fourone_ovi_m_b1 <- four_to_one_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")



## Reading the data in - Block 2
four_to_one_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_b2_oviposition.xlsx")

## Adding variable names to the data - Block 2
fourone_ovi_m_b2 <- four_to_one_male_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Binding the 4:1 data - separate data sets 
four_to_one_male_oviposition <- rbind(four_to_one_male_oviposition_b1, four_to_one_male_oviposition_b2)






# 1:4  ####

## Reading the data in - Block 1
one_to_four_male_oviposition_b1 <- read_excel("data/male_conditioning/m_1-4_b1_oviposition.xlsx")

## Adding variable names to the data - Block 1
onefour_ovi_m_b1 <- one_to_four_male_oviposition_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



## Reading the data in - Block 2
one_to_four_male_oviposition_b2 <- read_excel("data/male_conditioning/m_1-4_b2_oviposition.xlsx")

## Adding variable names to the data - Block 2
onefour_ovi_m_b2 <- one_to_four_male_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



# Binding the 1:4 data - separate data sets 
one_to_four_male_oviposition <- rbind(one_to_four_male_oviposition_b1, one_to_four_male_oviposition_b2)




# 4:1 + 1:4  ####

## Reading the data in - block 1 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/m_4-1_1-4_b1_oviposition.xlsx")

## Adding variable names to the data - Block 1
combined_ovi_m_b1 <- fourone_onefour_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Reading the data in - Block 2
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/m_4-1_1-4_b2_oviposition.xlsx")

## Adding variable names to the data - Block 2
combined_ovi_m_b2 <- fourone_onefour_male_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")




# Binding the data for 4:1/1:4 - separate data sets
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)




## Pivoting data frames for combined data sets
# 4:1
fourone_ovi_m <- four_to_one_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")
# 1:4 
onefour_ovi_m <- one_to_four_male_oviposition  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")




####  DATA VISUALISATION #### 




######################################################################################################### --
#### Creating a function for a plot which will allow me to run the same code for different datasets 



# Creating a function for a boxplot, 
# this will allow one to use the feeding data for different data sets 


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





## Combined Block Data Visualisation ####

  
## Code will allow one to see each of the plots
################## --
## OvoD1 Female ####


# 1:4 
ov1_egg_1_4 <- oviposition_results(onefour_ovi_of, boxplot_fill_colour = viridis_colors[c(9,9)])

# 4:1 
ov1_egg_4_1 <- oviposition_results(fourone_ovi_of, boxplot_fill_colour = viridis_colors[c(7,7)])


ov1_egg_combined <- oviposition_results(combined_ovi_of, boxplot_fill_colour = viridis_colors[c(9, 9, 7, 7)])
################ --

viridis_colors <- inferno(10)


## Using grid.arrange to put the plots together
ovod1_female_oviposition <- grid.arrange(ov1_egg_1_4, ov1_egg_4_1,
                                         nrow = 1,
                                         widths = c(0.5,0.5),
                                         heights = c(1))


ggsave(filename = "ovod1_female_oviposition.png", 
       plot = ovod1_female_oviposition, 
       width = 10, 
       height = 6, 
       dpi = 300)

ggsave(filename = "ov1_egg_combined.png", 
       plot = ov1_egg_combined, 
       width = 10, 
       height = 6, 
       dpi = 300)




############################# --

## Wild Type Virgin Female ####

# 1:4 
v1_egg_1_4  <- oviposition_results(onefour_ovi_vf, boxplot_fill_colour = viridis_colors[c(9,9)])

# 4:1 
v1_egg_4_1 <- oviposition_results(fourone_ovi_vf, boxplot_fill_colour = viridis_colors[c(7,7)])

# 4:1 and 1:4 
v1_egg_combined <- oviposition_results(combined_ovi_vf, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])






## Using grid arrange to put the plots together
virgin_female_oviposition <- grid.arrange(v1_egg_1_4, v1_egg_4_1 ,
                                          nrow = 1,
                                          widths = c(0.5,0.5),
                                          heights = c(1))




ggsave(filename = "v1_egg_combined.png", 
       plot = v1_egg_combined, 
       width = 10, 
       height = 6, 
       dpi = 300)

ggsave(filename = "virgin_female_oviposition.png", 
       plot = virgin_female_oviposition, 
       width = 10, 
       height = 6, 
       dpi = 300)




#################### --
## Wild type Male ####

# 1:4 
m_egg_1_4 <- oviposition_results(onefour_ovi_m, boxplot_fill_colour  = viridis_colors[c(9,9)])

# 4:1 
m_egg_4_1 <- oviposition_results(fourone_ovi_m, boxplot_fill_colour  = viridis_colors[c(7,7)])

# 1:4 and 4:1 
m_egg_combined <- oviposition_results(combined_ovi_m, boxplot_fill_colour  = viridis_colors[c(9,9,7,7)])





## Using grid arrange to put the plots together
male_oviposition <- grid.arrange(m_egg_1_4, m_egg_4_1,
                                 nrow = 1,
                                 widths = c(0.5,0.5),
                                 heights = c(1))



ggsave(filename = "male_oviposition.png", 
       plot = male_oviposition, 
       width = 10, 
       height = 6, 
       dpi = 300)



ggsave(filename = "m_egg_combined.png", 
       plot = m_egg_combined, 
       width = 10, 
       height = 6, 
       dpi = 300)




##### ALL THE PLOTS TOGETHER 
overall_oviposition <- grid.arrange(m_egg_1_4 + ggtitle("Male Conditioning"), m_egg_4_1 + ggtitle("Male Conditioning"), m_egg_combined + ggtitle("Male Conditioning"),
                                    ov1_egg_1_4 + ggtitle("OvoD1 Female Conditioning"), ov1_egg_4_1 + ggtitle("OvoD1 Female Conditioning"), ov1_egg_combined + ggtitle("OvoD1 Female Conditioning"),
                                    v1_egg_1_4 + ggtitle("Virgin Female Conditioning"), v1_egg_4_1 + ggtitle("Virgin Female Conditioning"), v1_egg_combined + ggtitle("Virgin Female Conditioning"),
                                    nrow = 3,
                                    widths = c(0.5,0.5,1),
                                    heights = c(1,1,1))









###### ###### Individual Block Data Visualisation ####
viridis_colours <- viridis(10)
## - 
############ --
## OvoD1 Female ####
############ --


## Visualising the individual blocks as data shows these to be significant ## 




# For 1:4 OvoD1

# Block 1 
ovod1_b1_onefour <- oviposition_results(one_four_b1_egg, boxplot_fill_colour = viridis_colours[1:2])

# Block 2
ovod1_b2_onefour <- oviposition_results(one_four_b2_egg, boxplot_fill_colour = viridis_colours[1:2])


# For 4:1 OvoD1

# Block 1 
ovod1_b1_fourone <- oviposition_results(four_one_b1_egg, boxplot_fill_colour = viridis_colours[3:4])

# Block 2
ovod1_b2_fourone <- oviposition_results(four_one_b2_egg, boxplot_fill_colour = viridis_colours[3:4])


## Combined 4:1 and 1:4 

# Block 1
ovod1_b1_combined <- oviposition_results(fourone_onefour_ovi_od1_b1 , boxplot_fill_colour = viridis_colours[c(1:4)])

# Block 2 
ovod1_b2_combined <- oviposition_results(fourone_onefour_ovi_od1_b2 ,  boxplot_fill_colour = viridis_colours[c(1:4)])



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
virgin_b2_fourone <- oviposition_results( fourone_ovi_vf_b2 , boxplot_fill_colour  = viridis_colours[3:4])

# Block 3 
virgin_b3_fourone <- oviposition_results( fourone_ovi_vf_b3, boxplot_fill_colour = viridis_colours[3:4])

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








