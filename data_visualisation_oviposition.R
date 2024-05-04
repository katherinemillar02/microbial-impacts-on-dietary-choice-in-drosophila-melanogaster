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


## UPLOADING THE DATA IN ####

################### --
## OvoD1 FEMALES ####
################### --

########### 4:1 OvoD1 ########### 

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





############ 1:4 OvoD1 ###########

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




###### 4:1 + 1:4 OvoD1 ######

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





###### ###### ###### 




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





# 1:4 Virgin ####

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





# 4:1/1:4 Virgin ####

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







####################### ####################### ####################### ####################### ####################### 





####################### ####################### ####################### ####################### #######################  
####################### ---
## WILD TYPE MALES ######
###################### ---

# 4:1 Male ####

## Reading the data in - Block 1
four_to_one_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/m_4-1_t2b1_oviposition.xlsx")

## Adding variable names to the data - Block 1
fourone_ovi_m_b1 <- four_to_one_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")



## Reading the data in - Block 2
four_to_one_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_4-1_t2b2_oviposition.xlsx")

## Adding variable names to the data - Block 2
fourone_ovi_m_b2 <- four_to_one_male_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Binding the 4:1 data - separate data sets 
four_to_one_male_oviposition <- rbind(four_to_one_male_oviposition_b1, four_to_one_male_oviposition_b2)






# 1:4 Male ####

## Reading the data in - Block 1
one_to_four_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2//m_1-4_t2b1_oviposition.xlsx")

## Adding variable names to the data - Block 1
onefour_ovi_m_b1 <- one_to_four_male_oviposition_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



## Reading the data in - Block 2
one_to_four_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_1-4_t2b2_oviposition.xlsx")

## Adding variable names to the data - Block 2
onefour_ovi_m_b2 <- one_to_four_male_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



# Binding the 1:4 data - separate data sets 
one_to_four_male_oviposition <- rbind(one_to_four_male_oviposition_b1, one_to_four_male_oviposition_b2)




# 4:1 + 1:4 Male ####

## Reading the data in - block 1 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b1_oviposition.xlsx")

## Adding variable names to the data - Block 1
combined_ovi_m_b1 <- fourone_onefour_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Reading the data in - Block 2
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b2_oviposition.xlsx")

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




#### #### #### ####  DATA VISUALISATION #### #### #### #### 




######################################################################################################### --
#### Creating a function for a plot which will allow me to run the same code for different datasets 



# Creating a function for a boxplot, 
# this will allow one to use the feeding data for different data sets 


#################################### Feeding Results Function Plot ####################################

oviposition_results <- function(summary_data2,boxplot_fill_colour ) {
  ggplot(summary_data2, aes(x = diet, y = egg_numbers, fill = diet, pattern = diet))+ 
    # geom_jitter(aes(x = diet,
    #                 y = fly_numbers,
    #                 fill = diet),
    #             width = 0.1,
    #             shape = 1) +
    geom_boxplot()+
    # geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
    #                      color = "black",
    #                      pattern_fill = "white",
    #                      pattern_angle = 45,
    #                      pattern_density = 0.1,
    #                      pattern_spacing = 0.025,
    #                      pattern_key_scale_factor = 0.6) +
    # 
    geom_point(aes(),
               size = 1,
               shape = 1,
               position = position_dodge(width=0.75)) +
    
    
    theme_classic()+
    labs(x = "Diet Condition",
         y = "Number of eggs per diet patch", 
         title = "")+
    scale_fill_manual(values = boxplot_fill_colour) +  # Set fill colors for the boxplot
    scale_pattern_manual(values = c("stripe", "none", "stripe", "none")) +
    theme(legend.position = "none") +
    ylim(-0.01, 150) 
  
}




## Combined data sets ####
## Code will allow one to see each of the plots
################## --
## OvoD1 Female ####

# 1:4 
ov1_egg_1_4 <- oviposition_results(onefour_ovi_of, boxplot_fill_colour = viridis_colors[1:2])

# 4:1 
ov1_egg_4_1 <- oviposition_results(fourone_ovi_of, boxplot_fill_colour = viridis_colors[3:4])


ov1_egg_combined <- oviposition_results(combined_ovi_of,boxplot_fill_colour = viridis_colors[1:4])
################ --

## Using grid.arrange to put the plots together
ovod1_female_oviposition <- grid.arrange(ov1_egg_1_4, ov1_egg_4_1, ov1_egg_combined,
                                         nrow = 1,
                                         widths = c(0.5,0.5,1),
                                         heights = c(1))



############################# --

## Wild Type Virgin Female ####

# 1:4 
v1_egg_1_4  <- oviposition_results(onefour_ovi_vf, boxplot_fill_colour = viridis_colors[1:2])

# 4:1 
v1_egg_4_1 <- oviposition_results(fourone_ovi_vf, boxplot_fill_colour = viridis_colors[3:4])

# 4:1 and 1:4 
v1_egg_combined <- oviposition_results(combined_ovi_vf, boxplot_fill_colour = viridis_colors[1:4])






## Using grid arrange to put the plots together
virgin_female_oviposition <- grid.arrange(v1_egg_1_4, v1_egg_4_1, v1_egg_combined ,
                                          nrow = 1,
                                          widths = c(0.5,0.5,1),
                                          heights = c(1))







#################### --
## Wild type Male ####

# 1:4 
m_egg_1_4 <- oviposition_results(onefour_ovi_m, boxplot_fill_colour = viridis_colors[1:2])

# 4:1 
m_egg_4_1 <- oviposition_results(fourone_ovi_m, boxplot_fill_colour = viridis_colors[3:4])

# 1:4 and 4:1 
m_egg_combined <- oviposition_results(combined_ovi_m, boxplot_fill_colour = viridis_colors[1:4])




## Using grid arrange to put the plots together
male_oviposition <- grid.arrange(m_egg_1_4, m_egg_4_1, m_egg_combined,
                                 nrow = 1,
                                 widths = c(0.5,0.5,1),
                                 heights = c(1))







##### ALL THE PLOTS TOGETHER 
overall_oviposition <- grid.arrange(m_egg_1_4 + ggtitle("Male Conditioning"), m_egg_4_1 + ggtitle("Male Conditioning"), m_egg_combined + ggtitle("Male Conditioning"),
                                    ov1_egg_1_4 + ggtitle("OvoD1 Female Conditioning"), ov1_egg_4_1 + ggtitle("OvoD1 Female Conditioning"), ov1_egg_combined + ggtitle("OvoD1 Female Conditioning"),
                                    v1_egg_1_4 + ggtitle("Virgin Female Conditioning"), v1_egg_4_1 + ggtitle("Virgin Female Conditioning"), v1_egg_combined + ggtitle("Virgin Female Conditioning"),
                                    nrow = 3,
                                    widths = c(0.5,0.5,1),
                                    heights = c(1,1,1))









###### ###### INDIVIDUAL BLOCK CODE NEEDS A BIT OF CLEANING ###### ###### 
# Indiviual Block ----

#### MORE PLOTS 



## - 
############ --
## OVOD1 Female ####
############ --


## Visualising the individual blocks as data shows these to be significant ## 

viridis_colours <- viridis(10)


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
## VIRGIN Female ####
############ --

## 4:1 Virgin ####

# Block 2 
virgin_b2_fourone <- oviposition_results( fourone_ovi_vf_b2 , boxplot_fill_colour  = viridis_colours[3:4])

# Block 3 
virgin_b3_fourone <- oviposition_results( fourone_ovi_vf_b3, boxplot_fill_colour = viridis_colours[3:4])

# Block 4 
virgin_b4_fourone <- oviposition_results( fourone_ovi_vf_b4, boxplot_fill_colour = viridis_colours[3:4])



# 1:4 Virgin ####

# Block 2 
virgin_b2_onefour <- oviposition_results(onefour_ovi_vf_b2  , boxplot_fill_colour = viridis_colours[1:2])

# Block 3
virgin_b3_onefour <- oviposition_results(onefour_ovi_vf_b3 , boxplot_fill_colour  = viridis_colours[1:2])

# Block 4
virgin_b4_onefour <- oviposition_results(onefour_ovi_vf_b4 , boxplot_fill_colour  = viridis_colours[1:2])


## 4:1 + 1:4 Virgin ####

# Block 2
virgin_b2_combined <- oviposition_results(fourone_onefour_ovi_vf_b2 , boxplot_fill_colour  = viridis_colours[c(1:4)])

# Block 3
virgin_b3_combined <- oviposition_results(fourone_onefour_ovi_vf_b3 , boxplot_fill_colour  = viridis_colours[c(1:4)])

# Block 4
virgin_b4_combined <- oviposition_results(fourone_onefour_ovi_vf_b4, boxplot_fill_colour  = viridis_colours[c(1:4)])




############ --
## MALE ####
############ -- 

male_b1_fourone <- oviposition_results(fourone_ovi_m_b1, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
male_b2_fourone <- oviposition_results(fourone_ovi_m_b2, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))

male_b1_fourone <- oviposition_results(fourone_ovi_m_b1, boxplot_fill_colour = viridis_colors[1:2])
male_b2_fourone <- oviposition_results(fourone_ovi_m_b2, boxplot_fill_colour = viridis_colors[5:6])


male_b1_fourone + male_b2_fourone


male_b1_onefour <- oviposition_results(onefour_ovi_m_b1, boxplot_fill_colour = viridis_colors[5:6])
male_b2_onefour <- oviposition_results(onefour_ovi_m_b2, boxplot_fill_colour = viridis_colors[5:6])

male_b1_onefour <- oviposition_results(onefour_ovi_m_b1, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
male_b2_onefour <- oviposition_results(onefour_ovi_m_b2, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))

male_b1_fourone_onefour <- oviposition_results(fourone_onefour_ovi_m_b1, boxplot_fill_colour = viridis_colors[3:6])
male_b2_fourone_onefour <- oviposition_results(boxplot_fill_colour = viridis_colors[3:6])

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


male_b1_onefour + male_b2_onefour





## PUTTING THE PLOTS TOGETHER 
## 

ovod1_b1_onefour + ggtitle("OvoD1 B1") + ovod1_b2_onefour + ggtitle("OvoD1 B2") /
  ovod1_b1_fourone + ggtitle("OvoD1 B1") + ovod1_b2_fourone + ggtitle("OvoD1 B2") /
  virgin_b2_fourone + ggtitle("Virgin B2") + virgin_b3_fourone + ggtitle("Virgin B3")/
  virgin_b2_onefour + ggtitle("Virgin B2")+ virgin_b3_onefour + ggtitle("Virgin B3") + virgin_b4_onefour +  ggtitle("Virgin B4")/
  male_b1_fourone + ggtitle("Male B1")+ male_b2_fourone + ggtitle("Male B2") /
  male_b1_onefour + ggtitle("Male B1") + male_b2_onefour + ggtitle("Male B1")



## works 
(ovod1_b1_onefour + ggtitle("OvoD1 Block 1")) + 
  (ovod1_b1_fourone + ggtitle("OvoD1 Block 1")) +
  (ovod1_b1_combined + ggtitle("OvoD1 Block 1")) 





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









ovod1_female_oviposition_b1 + ovod1_female_oviposition_b2 



(ovod1_b2_onefour + ggtitle("OvoD1 Block 2")) +
  (ovod1_b2_fourone + ggtitle("OvoD1 Block 2")) + 
  (ovod1_b2_combined + ggtitle("OvoD1 Block 2"))



(virgin_b2_fourone + ggtitle("Virgin B2")) +
  (virgin_b3_fourone + ggtitle("Virgin B3")) +
  (virgin_b2_onefour + ggtitle("Virgin B2")) +
  (virgin_b3_onefour + ggtitle("Virgin B3")) +
  (virgin_b4_onefour + ggtitle("Virgin B4")) +
  (male_b1_fourone + ggtitle("Male B1")) +
  (male_b2_fourone + ggtitle("Male B2")) +
  (male_b1_onefour + ggtitle("Male B1")) +
  (male_b2_onefour + ggtitle("Male B1"))





#### 4:1 and 1:4 Assays ----
## Block is significant for OvoD1
## Block is not significant for Males 
## Block is significant for Virgins 


# 4:1 + 1:4 Male ####
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b2_oviposition.xlsx")

fourone_onefour_ovi_m_b1 <- fourone_onefour_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


fourone_onefour_ovi_m_b2 <- fourone_onefour_male_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")




fourone_onefour_female_oviposition_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
fourone_onefour_female_oviposition_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")

fourone_onefour_ovi_od1_b1 <- fourone_onefour_female_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


fourone_onefour_ovi_od1_b2 <- fourone_onefour_female_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")



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

