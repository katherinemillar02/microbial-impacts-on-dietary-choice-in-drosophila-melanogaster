#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)
library(ggplot2)
library(ggpattern)

## OVIPOSITION SCRIPT 
#### Uploading raw data

## OvoD1 Females 
four_to_one_oviposition_ovod1 <- read_excel("data/female_conditioning/ovod1/block_1/4-1_oviposition_ovod1.xlsx")
one_to_four_oviposition_ovod1 <- read_excel("data/female_conditioning/ovod1/block_1/1_4_oviposition_ovod1.xlsx") # need to change excel file name
fourone_onefour_oviposition_ovod1 <- read_excel("data/female_conditioning/ovod1/block_1/4-1_1-4_oviposition_ovod1.xlsx")

## Making the  data long 
## OvoD1
# 4:1
four_to_one_oviposition_ovod1_long <- four_to_one_oviposition_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 1:4 
one_to_four_oviposition_ovod1_long <- one_to_four_oviposition_ovod1 %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
fourone_onefour_oviposition_ovod1_long  <- fourone_onefour_oviposition_ovod1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


## Virgin females ## NEED OVIPOSITION DATA ## 
## OvoD1 Females 
four_to_one_oviposition_virgin <- read_excel("data/female_conditioning/virgin/4-1_oviposition_virgin.xlsx")
one_to_four_oviposition_virgin <- read_excel("data/female_conditioning/virgin/1-4_oviposition_virgin.xlsx") # need to change excel file name
fourone_onefour_oviposition_virgin <- read_excel("data/female_conditioning/virgin/4-1_1-4_oviposition_virgin.xlsx")
####
## Making the  data long 
## Virgin
# 4:1
four_to_one_oviposition_virgin_long <- four_to_one_oviposition_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 1:4 
one_to_four_oviposition_virgin_long <- one_to_four_oviposition_virgin %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
fourone_onefour_oviposition_virgin_long  <- fourone_onefour_oviposition_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")
####--- 

## Males 
# 4:1 
four_to_one_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/block_1/m_4-1_t2b1_oviposition.xlsx")
four_to_one_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/block_2/m_4-1_t2b2_oviposition.xlsx")
# binding the data
four_to_one_male_oviposition <- rbind(four_to_one_male_oviposition_b1, four_to_one_male_oviposition_b1)

# 1:4 
one_to_four_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/block_1/m_1-4_t2b1_oviposition.xlsx")
one_to_four_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/block_2/m_1-4_t2b2_oviposition.xlsx")
# binding the data
one_to_four_male_oviposition <- rbind(one_to_four_male_oviposition_b1, one_to_four_male_oviposition_b2)

# 4:1 + 1:4 
fourone_onefour_male_oviposition_b1 <- read_excel("data/male_conditioning/treatment_2/block_1/m_4-1_1-4_t2b1_oviposition.xlsx")
fourone_onefour_male_oviposition_b2 <- read_excel("data/male_conditioning/treatment_2/block_2/m_4-1_1-4_t2b2_oviposition.xlsx")
# binding the data
fourone_onefour_male_oviposition <- rbind(fourone_onefour_male_oviposition_b1, fourone_onefour_male_oviposition_b2)

## Making the  data long 
# 4:1
four_to_one_male_oviposition_long <- four_to_one_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "egg_numbers")
# 1:4 
one_to_four_male_oviposition_long <- one_to_four_male_oviposition  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers") 
# 4:1 and 1:4 
fourone_onefour_male_oviposition_long <- fourone_onefour_male_oviposition  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "egg_numbers")


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
## OvoD1 Female
ov1_egg1 <- oviposition_results(one_to_four_oviposition_ovod1_long, boxplot_fill_color = c("#9FE2BF","#9FE2BF"))
ov1_egg2 <- oviposition_results(four_to_one_oviposition_ovod1_long, boxplot_fill_color = c("#FF7F50","#FF7F50")) 
ov1_egg3 <- oviposition_results(fourone_onefour_oviposition_ovod1_long, boxplot_fill_color = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))

ov1_egg1 + ov1_egg2 + ov1_egg3

## Virgin Female
v1_egg1 <- oviposition_results(one_to_four_oviposition_virgin_long, boxplot_fill_color = c("#9FE2BF","#9FE2BF"))
v1_egg2 <- oviposition_results(four_to_one_oviposition_virgin_long, boxplot_fill_color = c("#FF7F50","#FF7F50")) 
v1_egg3 <- oviposition_results(fourone_onefour_oviposition_virgin_long, boxplot_fill_color = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))

v1_egg1 + v1_egg2 + v1_egg3

## Male
m_egg1 <- oviposition_results(one_to_four_male_oviposition_long, boxplot_fill_color = c("#9FE2BF","#9FE2BF"))
m_egg2 <- oviposition_results(four_to_one_male_oviposition_long , boxplot_fill_color = c("#FF7F50","#FF7F50")) 
m_egg3 <- oviposition_results(fourone_onefour_male_oviposition_long, boxplot_fill_color = c("#9FE2BF","#9FE2BF","#FF7F50","#FF7F50"))

m_egg1 + m_egg2 + m_egg3
