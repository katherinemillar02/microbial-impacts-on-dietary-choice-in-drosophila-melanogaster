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

# Binding the 1:4 data 
onefour_oviposition_of <- rbind(one_to_four_oviposition_ovod1_b1, one_to_four_oviposition_ovod1_b2)




###### 4:1 + 1:4 OvoD1 ######

# 4:1./1:4

fourone_onefour_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
fourone_onefour_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")


fourone_onefour_oviposition_ovod1_b1  <- fourone_onefour_oviposition_ovod1_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

fourone_onefour_oviposition_ovod1_b2  <- fourone_onefour_oviposition_ovod1_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


# Binding the 4:1/1:4 data
fourone_onefour_oviposition_of <- rbind(fourone_onefour_oviposition_ovod1_b1, fourone_onefour_oviposition_ovod1_b2)








## USING PIVOT LONGER TO ADD A VARIABLE TO A DATA FILE ##
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

# 4:1 Virgin ####
four_to_one_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b2.xlsx")

# 4:1 - oviposition 
fourone_ovi_vf_b2 <- four_to_one_oviposition_virgin_2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


four_to_one_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b3.xlsx")

fourone_ovi_vf_b3 <- four_to_one_oviposition_virgin_3 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


four_to_one_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_b4.xlsx")

fourone_ovi_vf_b4 <- four_to_one_oviposition_virgin_4 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

# Binding the 4:1 data 
four_to_one_oviposition_virgin <- rbind(four_to_one_oviposition_virgin_2, four_to_one_oviposition_virgin_3, four_to_one_oviposition_virgin_4)

# 1:4 Virgin ####

##
one_to_four_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b2.xlsx")

onefour_ovi_vf_b2 <- one_to_four_oviposition_virgin_2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 




one_to_four_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b3.xlsx")

onefour_ovi_vf_b3 <- one_to_four_oviposition_virgin_3  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


one_to_four_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_b4.xlsx")

onefour_ovi_vf_b4 <- one_to_four_oviposition_virgin_4  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 



# Binding the 1:4 data 
one_to_four_oviposition_virgin <- rbind(one_to_four_oviposition_virgin_2,one_to_four_oviposition_virgin_3, one_to_four_oviposition_virgin_4)

# 4:1/1:4 Virgin ####
fourone_onefour_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b2.xlsx")
fourone_onefour_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b3.xlsx")
fourone_onefour_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_b4.xlsx")
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










####################### ---
## WILD TYPE MALES ######
###################### ---

# 4:1 Male ####
four_to_one_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/m_4-1_t2b1_oviposition.xlsx")


fourone_ovi_m_b1 <- four_to_one_male_oviposition_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

four_to_one_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_4-1_t2b2_oviposition.xlsx")

fourone_ovi_m_b2 <- four_to_one_male_oviposition_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")

# Binding the 4:1 data
four_to_one_male_oviposition <- rbind(four_to_one_male_oviposition_b1, four_to_one_male_oviposition_b2)





# 1:4 Male ####
one_to_four_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2//m_1-4_t2b1_oviposition.xlsx")


onefour_ovi_m_b1 <- one_to_four_male_oviposition_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 

one_to_four_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_1-4_t2b2_oviposition.xlsx")

onefour_ovi_m_b2 <- one_to_four_male_oviposition_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 


# Binding the 1:4 data
one_to_four_male_oviposition <- rbind(one_to_four_male_oviposition_b1, one_to_four_male_oviposition_b2)


# 4:1 + 1:4 Male ####
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/m_4-1_1-4_t2b2_oviposition.xlsx")
# Binding the data for 4:1/1:4 
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)



## Pivoting data frames 
# 4:1
fourone_ovi_m <- four_to_one_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")
# 1:4 
onefour_ovi_m <- one_to_four_male_oviposition  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
combined_ovi_m <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")






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
    ylim(-0.01, 250) 
  
}




## Combined data sets ####
## Code will allow one to see each of the plots
################## --
## OvoD1 Female ####
ov1_egg1 <- oviposition_results(onefour_ovi_of, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
ov1_egg1 <- oviposition_results(onefour_ovi_of, boxplot_fill_colour = viridis_colors[3:4])

ov1_egg2 <- oviposition_results(fourone_ovi_of, boxplot_fill_colour = c("#FF7F50","#FF7F50")) 
ov1_egg2 <- oviposition_results(onefour_ovi_of, boxplot_fill_colour = viridis_colors[5:6])

ov1_egg3 <- oviposition_results(combined_ovi_of, boxplot_fill_colour = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))
ov1_egg3 <- oviposition_results(combined_ovi_of, boxplot_fill_colour = viridis_colors[3:6])

################ --

## Using grid.arrange to put the plots together
ovod1_female_oviposition <- grid.arrange(ov1_egg1, ov1_egg2, ov1_egg3,
                                     nrow = 1,
                                     widths = c(0.5,0.5,1),
                                     heights = c(1))



############################# --

## Wild Type Virgin Female ####
v1_egg1 <- oviposition_results(onefour_ovi_vf, boxplot_fill_colour = c("#9FE2BF","#9FE2BF"))

v1_egg1 <- oviposition_results(onefour_ovi_vf, boxplot_fill_colour = viridis_colors[3:4])

v1_egg2 <- oviposition_results(fourone_ovi_vf, boxplot_fill_colour = c("#FF7F50","#FF7F50")) 

v1_egg2 <- oviposition_results(fourone_ovi_vf, boxplot_fill_colour = viridis_colors[5:6])

v1_egg3 <- oviposition_results(combined_ovi_vf, boxplot_fill_colour = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))
v1_egg3 <- oviposition_results(combined_ovi_vf, boxplot_fill_colour = viridis_colors[3:6])






## Using grid arrange to put the plots together
virgin_female_oviposition <- grid.arrange(v1_egg1, v1_egg2, v1_egg3,
                                         nrow = 1,
                                         widths = c(0.5,0.5,1),
                                         heights = c(1))


#################### --
## Wild type Male ####
m_egg1 <- oviposition_results(onefour_ovi_m, boxplot_fill_colour = viridis_colors[3:4])
m_egg2 <- oviposition_results(fourone_ovi_m, boxplot_fill_colour = viridis_colors[5:6]) 
m_egg3 <- oviposition_results(combined_ovi_m, boxplot_fill_colour = viridis_colors[3:6])


m_egg1 <- m_egg1
m_egg2 <- m_egg2
m_egg3 <- m_egg3


## Using grid arrange to put the plots together
male_oviposition <- grid.arrange(m_egg1, m_egg2, m_egg3,
                                 nrow = 1,
                                 widths = c(0.5,0.5,1),
                                 heights = c(1))



##### ALL THE PLOTS TOGETHER 
overall_oviposition <- grid.arrange(m_egg1 + ggtitle("Male Conditioning"), m_egg2 + ggtitle("Male Conditioning"), m_egg3 + ggtitle("Male Conditioning"),
                                    ov1_egg1 + ggtitle("OvoD1 Female Conditioning"), ov1_egg2 + ggtitle("OvoD1 Female Conditioning"), ov1_egg3 + ggtitle("OvoD1 Female Conditioning"),
                                 v1_egg1 + ggtitle("Virgin Female Conditioning"), v1_egg2 + ggtitle("Virgin Female Conditioning"), v1_egg3 + ggtitle("Virgin Female Conditioning"),
                                 nrow = 3,
                                 widths = c(0.5,0.5,1),
                                 heights = c(1,1,1))




##### -- 







###### ###### INDIVIDUAL BLOCK CODE NEEDS A BIT OF CLEANING ###### ###### 
# Indiviual Block ----

#### MORE PLOTS 



## - 
############ --
## OVOD1 Female ####
############ --


## Visualising the individual blocks as data shows these to be significant ## 

## 1:4 OvoD1 ####
ovod1_b1_onefour <- oviposition_results(one_four_b1_egg, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
ovod1_b2_onefour <- oviposition_results(one_four_b2_egg, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))



fourone_onefour_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
fourone_onefour_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")

ovod1_b1_onefour + ovod1_b2_onefour


## 4:1 OvoD1 ####
ovod1_b1_fourone <- oviposition_results(four_one_b1_egg, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
ovod1_b2_fourone <- oviposition_results(four_one_b2_egg, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))







 ovod1_b1_fourone + ovod1_b2_fourone
 library(viridis)
 
 # Get Viridis palette starting from the third color
 viridis_colors <- viridis(6) 
 
 
 # For 1:4 OvoD1
 ovod1_b1_onefour <- oviposition_results(one_four_b1_egg, boxplot_fill_colour = viridis_colors[5:6])
 ovod1_b2_onefour <- oviposition_results(one_four_b2_egg, boxplot_fill_colour = viridis_colors[5:6])
 
 # For 4:1 OvoD1
 ovod1_b1_fourone <- oviposition_results(four_one_b1_egg, boxplot_fill_colour = viridis_colors[3:4])
 ovod1_b2_fourone <- oviposition_results(four_one_b2_egg, boxplot_fill_colour = viridis_colors[3:4])
 
 
 ## Combined 4:1 and 1:4 
 ovod1_b1_combined <- oviposition_results(fourone_onefour_oviposition_ovod1_b1 , boxplot_fill_colour = viridis_colors[3:6] )
 ovod1_b2_combined <- oviposition_results(fourone_onefour_oviposition_ovod1_b2 ,  boxplot_fill_colour = viridis_colors[3:6] )
 
 # Plotting 1:4 OvoD1
 plot_1_4 <- ovod1_b1_onefour + ovod1_b2_onefour
 
 # Plotting 4:1 OvoD1
 plot_4_1 <- ovod1_b1_fourone + ovod1_b2_fourone
 
 # Displaying plots
 plot_1_4
 plot_4_1
 
 plot_1_4 +  plot_4_1
############ --
## VIRGIN Female ####
############ --





## 4:1 Virgin ####
virgin_b1_fourone <- oviposition_results(four_one_b1_egg, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
virgin_b2_fourone <- oviposition_results(four_one_b2_egg, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
virgin_b3_fourone <- oviposition_results(four_one_b2_egg, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))


virgin_b1_fourone + virgin_b2_fourone + virgin_b3_fourone 


# 1:4 Virgin ####
virgin_b2_onefour <- oviposition_results(onefour_ovi_vf_b2, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
virgin_b3_onefour <- oviposition_results(onefour_ovi_vf_b3, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
virgin_b4_onefour <- oviposition_results(onefour_ovi_vf_b4, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))


virgin_b2_onefour + virgin_b3_onefour + virgin_b4_onefour 





############ --
## MALE ####
############ -- 

male_b1_fourone <- oviposition_results(fourone_ovi_m_b1, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))
male_b2_fourone <- oviposition_results(fourone_ovi_m_b2, boxplot_fill_colour = c("#9FE2BF", "#9FE2BF"))

male_b1_fourone <- oviposition_results(fourone_ovi_m_b1, boxplot_fill_colour = viridis_colors[5:6])
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


o_b1_onefour <- ovod1_b1_onefour + ggtitle("OvoD1 Block 1")
o_b1_fourone <- ovod1_b1_fourone + ggtitle("OvoD1 Block 1")
o_b1_combined <- ovod1_b1_combined + ggtitle("OvoD1 Block 1")

o_b2_onefour <- ovod1_b2_onefour + ggtitle("OvoD1 Block 2")
o_b2_fourone <- ovod1_b2_fourone + ggtitle("OvoD1 Block 2")
o_b2_combined <- ovod1_b2_combined + ggtitle("OvoD1 Block 2")


## this code works
ovod1_female_oviposition <- grid.arrange(
  o_b1_onefour, o_b1_fourone, o_b1_combined,
  o_b2_onefour, o_b2_fourone, o_b2_combined,
  ncol = 3,
  nrow = 2,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1)
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
