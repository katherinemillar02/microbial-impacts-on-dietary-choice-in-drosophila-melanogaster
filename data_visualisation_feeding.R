#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)
library(ggplot2)
library(ggpattern)


#### Uploading raw data

## OvoD1 Females 
four_to_one_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b1.xlsx")
one_to_four_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b1.xlsx")
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")

## OvoD1 Females 
four_to_one_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b2.xlsx")
one_to_four_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b2.xlsx")
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")


four_to_one_ovod1 <- rbind(four_to_one_ovod1_b1, four_to_one_ovod1_b2)
one_to_four_ovod1 <- rbind(one_to_four_ovod1_b1, one_to_four_ovod1_b2)
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)





## Making the  data long 

# 4:1
fourone_of <- four_to_one_ovod1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 1:4 
onefour_of <- one_to_four_ovod1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
combined_of <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

####--- 



## Virgin females
four_to_one_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin.xlsx")
one_to_four_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin.xlsx")
fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin.xlsx")





four_to_one_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b2.xlsx")
one_to_four_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b2.xlsx")
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")


four_to_one_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b3.xlsx")
one_to_four_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b3.xlsx")
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")



## Binding the data 
four_to_one_virgin <- rbind(four_to_one_virgin_b1, four_to_one_virgin_b2, four_to_one_virgin_b3)
one_to_four_virgin <- rbind(one_to_four_virgin_b1, one_to_four_virgin_b2, one_to_four_virgin_b3 )
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2, fourone_onefour_virgin_b3)

## Making the  data long 
## Virgin
# 4:1
fourone_v <- four_to_one_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 1:4 
onefour_v <- one_to_four_virgin  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
combined_v<- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

####--- 

## Males 
# 4:1 
four_to_one_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_t2b1.xlsx")
four_to_one_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_t2b2.xlsx")
# binding the data
four_to_one_male <- rbind(four_to_one_male_b1, four_to_one_male_b2)

# 1:4 
one_to_four_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m1-4_t2b1.xlsx")
one_to_four_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m1-4_t2b2.xlsx")
# binding the data
one_to_four_male <- rbind(one_to_four_male_b1, one_to_four_male_b2)

# 4:1 + 1:4 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b2.xlsx")
# binding the data
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)




## Making the  data long 
# 4:1
fourone_m <- four_to_one_male  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")
# 1:4 
onefour_m <- one_to_four_male  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
combined_m <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


#### Creating a function for a plot which will allow me to run the same code for different datasets 
feeding_results <- function(summary_data,boxplot_fill_color ) {
  ggplot(summary_data, aes(x = diet, y = fly_numbers, fill = diet, pattern = diet))+ 
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
         y = "Median number of flies per diet patch", 
         title = "")+
    scale_fill_manual(values = boxplot_fill_color) +  # Set fill colors for the boxplot
    scale_pattern_manual(values = c("stripe", "none", "stripe", "none")) +
    theme(legend.position = "none") +
    ylim(-0.01, 6) +
    geom_jitter(data = summary_data,
                aes(x = diet,
                    y = fly_numbers,
                    fill = diet),
                width = 0.1,
                shape = 1)
  
}

## Code will allow one to see each of the plots
## Virgin Female
of1 <- feeding_results(onefour_of, boxplot_fill_color = c("lightblue", "lightblue"))
of2 <- feeding_results(fourone_of, boxplot_fill_color = c("#FDECCD","#FDECCD")) 
of3 <- feeding_results(combined_of, boxplot_fill_color = c("lightblue", "lightblue","#FDECCD","#FDECCD"))

of1 + of2 + of3

## Virgin Female
vf1 <- feeding_results(onefour_v, boxplot_fill_color = c("lightblue", "lightblue"))
vf2 <- feeding_results(fourone_v, boxplot_fill_color = c("#FDECCD","#FDECCD")) 
vf3 <- feeding_results(combined_v, boxplot_fill_color = c("lightblue", "lightblue","#FDECCD","#FDECCD"))

vf1 + vf2 + vf3

## Male
mf1 <- feeding_results(onefour_m, boxplot_fill_color = c("lightblue", "lightblue"))
mf2 <- feeding_results(fourone_m, boxplot_fill_color = c("#FDECCD","#FDECCD")) 
mf3 <- feeding_results(combined_m, boxplot_fill_color = c("lightblue", "lightblue","#FDECCD","#FDECCD"))

mf1 + mf2 + mf3




#### A violin plot 
feeding_results_violin <- function(summary_data, violin_fill_color) {
  ggplot(summary_data, aes(x = diet, y = fly_numbers, fill = diet)) + 
    geom_violin(trim = FALSE, width = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
theme_classic() +
    labs(x = "Diet Condition",
         y = "Median number of flies per diet patch",
         title = "") +
    scale_fill_manual(values = violin_fill_color) +
    ylim(-0.01, 6) +
    theme(legend.position = "none")
}


# male 
feeding_results_violin(fourone_onefour_male_long, violin_fill_color = c("lightblue", "lightblue","#FDECCD","#FDECCD"))



