#### INSTALL PACKAGES #### 📦📦📦📦
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness) 
library(ggplot2)
library(ggpattern)
######################## 📦📦📦📦 


###########################
# OVIPOSITION DATA 🥚🥚🥚🥚
###########################




## UPLOADING THE DATA IN -- 

###################
## OvoD1 FEMALES ##
###################

# 4:1 
four_to_one_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_oviposition_ovod1_b1.xlsx")
four_to_one_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_oviposition_ovod1_b2.xlsx")

# Binding the 4:1 data 
fourone_oviposition_of <- rbind(four_to_one_oviposition_ovod1_b1, four_to_one_oviposition_ovod1_b2)

# 1:4 
one_to_four_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/1-4_oviposition_ovod1_b1.xlsx")
one_to_four_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/1-4_oviposition_ovod1_b2.xlsx")

# Binding the 1:4 data 
onefour_oviposition_of <- rbind(one_to_four_oviposition_ovod1_b1, one_to_four_oviposition_ovod1_b2)

# 4:1./1:4
fourone_onefour_oviposition_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b1.xlsx")
fourone_onefour_oviposition_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/4-1_1-4_oviposition_ovod1_b2.xlsx")

# Binding the 4:1/1:4 data
fourone_onefour_oviposition_of <- rbind(fourone_onefour_oviposition_ovod1_b1, fourone_onefour_oviposition_ovod1_b2)



## USING PIVOT LONGER TO ADD A VARIABLE TO A DATA FILE 
## OvoD1
# 4:1
fourone_ovi_of <- fourone_oviposition_of %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 1:4 
onefour_ovi_of <- onefour_oviposition_of %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
combined_ovi_of  <- fourone_onefour_oviposition_of   %>% 
  pivot_longer(cols = ("1:4 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")






###############################
## VIRGIN WILD TYPE FEMALES ##
##############################
# Virgin Conditioning Oviposition data is not included as was done differently 

# 4:1 
four_to_one_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_2.xlsx")
four_to_one_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_3.xlsx")
four_to_one_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin_4.xlsx")

# Binding the 4:1 data 
four_to_one_oviposition_virgin <- rbind(four_to_one_oviposition_virgin_2, four_to_one_oviposition_virgin_3, four_to_one_oviposition_virgin_4)

# 1:4 
one_to_four_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_2.xlsx")
one_to_four_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_3.xlsx")
one_to_four_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin_4.xlsx")

# Binding the 1:4 data 
one_to_four_oviposition_virgin <- rbind(one_to_four_oviposition_virgin_2,one_to_four_oviposition_virgin_3, one_to_four_oviposition_virgin_4)

# 4:1/1:4 
fourone_onefour_oviposition_virgin_2 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_2.xlsx")
fourone_onefour_oviposition_virgin_3 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_3.xlsx")
fourone_onefour_oviposition_virgin_4 <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin_4.xlsx")

# Binding the 4:1/1:4 data
fourone_onefour_oviposition_virgin <- rbind(fourone_onefour_oviposition_virgin_1, fourone_onefour_oviposition_virgin_2, fourone_onefour_oviposition_virgin_3, fourone_onefour_oviposition_virgin_4 )


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










#######################
## WILD TYPE MALES ##
######################

# 4:1 
four_to_one_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/block_1/m_4-1_t2b1_oviposition.xlsx")
four_to_one_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/block_2/m_4-1_t2b2_oviposition.xlsx")

# Binding the 4:1 data
four_to_one_male_oviposition <- rbind(four_to_one_male_oviposition_b1, four_to_one_male_oviposition_b1)


# 1:4 
one_to_four_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/block_1/m_1-4_t2b1_oviposition.xlsx")
one_to_four_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/block_2/m_1-4_t2b2_oviposition.xlsx")

# Binding the 1:4 data
one_to_four_male_oviposition <- rbind(one_to_four_male_oviposition_b1, one_to_four_male_oviposition_b2)


# 4:1 + 1:4 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/block_1/m_4-1_1-4_t2b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/block_2/m_4-1_1-4_t2b2_oviposition.xlsx")

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





#### CREATING A FUNCTION FOR AN OVIPOSITION PLOT #### 
#########################################################################################################
#### Creating a function for a plot which will allow me to run the same code for different datasets 
oviposition_results <- function(summary_data,boxplot_fill_color ) {
  ggplot(summary_data, aes(x = diet, y = egg_numbers, fill = diet, pattern = diet))+ 
    geom_boxplot(fill = "white", color = "black")+
    geom_boxplot_pattern(position = position_dodge(preserve = "single"), 
                         color = "black",
                         pattern_fill = "white",
                         pattern_angle = 45,
                         pattern_density = 0.1,
                         pattern_spacing = 0.025,
                         pattern_key_scale_factor = 0.6) +
    theme_classic()+
    labs(x = "Diet Condition",
         y = "Median number of eggs per diet patch", 
         title = "")+
    scale_fill_manual(values = boxplot_fill_color) +  # Set fill colors for the boxplot
    scale_pattern_manual(values = c("stripe", "none", "stripe", "none")) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 6)) +
    ylim(0.1, 250) +
    geom_jitter(data = summary_data,
                aes(x = diet,
                    y = egg_numbers,
                    fill = diet),
                width = 0.1,
                shape = 1)
  
}




## Code will allow one to see each of the plots
##################
## OvoD1 Female ##
ov1_egg1 <- oviposition_results(onefour_ovi_of, boxplot_fill_color = c("#9FE2BF","#9FE2BF"))
ov1_egg2 <- oviposition_results(fourone_ovi_of, boxplot_fill_color = c("#FF7F50","#FF7F50")) 
ov1_egg3 <- oviposition_results(combined_ovi_of, boxplot_fill_color = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))
################

## Using patchwork() to combine the plots: 
ov1_egg1 + ov1_egg2 + ov1_egg3


#############################
## Wild Type Virgin Female ##
v1_egg1 <- oviposition_results(onefour_ovi_vf, boxplot_fill_color = c("#9FE2BF","#9FE2BF"))
v1_egg2 <- oviposition_results(fourone_ovi_vf, boxplot_fill_color = c("#FF7F50","#FF7F50")) 
v1_egg3 <- oviposition_results(combined_ovi_vf, boxplot_fill_color = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))

## Using patchwork() to combine the plots: 
v1_egg1 + v1_egg2 + v1_egg3


####################
## Wild type Male ##
m_egg1 <- oviposition_results(onefour_ovi_m, boxplot_fill_color = c("#9FE2BF","#9FE2BF"))
m_egg2 <- oviposition_results(fourone_ovi_m, boxplot_fill_color = c("#FF7F50","#FF7F50")) 
m_egg3 <- oviposition_results(combined_ovi_m, boxplot_fill_color = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))

## Using patchwork() to combine the plots: 
m_egg1 + m_egg2 + m_egg3
