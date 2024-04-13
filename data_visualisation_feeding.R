#### Packages ####
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(viridis)
#### #### #### #### #### --



#### Uploading raw data ####  --


##################### --
## OvoD1 FEMALES ####
##################### --
# 4:1 
four_to_one_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b1.xlsx")
four_to_one_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b2.xlsx")
# Binding the 4:1 data blocks 
four_to_one_ovod1 <- rbind(four_to_one_ovod1_b1, four_to_one_ovod1_b2)

# 1:4 
one_to_four_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b1.xlsx")
one_to_four_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b2.xlsx")
# Binding the 1:4 data blocks
one_to_four_ovod1 <- rbind(one_to_four_ovod1_b1, one_to_four_ovod1_b2)

# 4:1 and 1:4 
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")
# Binding the 4:1/1:4 data blocks
fourone_onefour_ovod1 <- rbind(fourone_onefour_ovod1_b1, fourone_onefour_ovod1_b2)


## CHANGING THE VARIABLE NAMES OF THE DATA - PIVOTING A DATA FRAME -- 
# 4:1
fourone_of <- four_to_one_ovod1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 1:4 
onefour_of <- one_to_four_ovod1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
combined_of <- fourone_onefour_ovod1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")









################################################### --
#👰👰👰👰VIRGIN WILD TYPE FEMALES 👰👰👰👰####
################################################### --


# 4:1 
four_to_one_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b1.xlsx")
four_to_one_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b2.xlsx")
four_to_one_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b3.xlsx")
four_to_one_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b4.xlsx")
# Binding the 4:1 data blocks 
four_to_one_virgin <- rbind(four_to_one_virgin_b1, four_to_one_virgin_b2, four_to_one_virgin_b3, four_to_one_virgin_b4)


# 1:4 
one_to_four_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b1.xlsx")
one_to_four_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b2.xlsx")
one_to_four_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b3.xlsx")
one_to_four_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b4.xlsx")

# Binding the 1:4 data blocks 
one_to_four_virgin <- rbind(one_to_four_virgin_b1, one_to_four_virgin_b2, one_to_four_virgin_b3,  one_to_four_virgin_b4 )


# 4:1 and 1:4 
fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx")
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")
fourone_onefour_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx")

# Binding the 4:1/1:4 data blocks 
fourone_onefour_virgin <- rbind (fourone_onefour_virgin_b1, fourone_onefour_virgin_b2, fourone_onefour_virgin_b3, fourone_onefour_virgin_b4)


## CHANGING THE VARIABLE NAMEAS OF THE DATA - PIVOTING A DATA FRAME -- 
#  4:1 
fourone_v <- four_to_one_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 1:4 
onefour_v <- one_to_four_virgin  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
combined_v<- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

####--






########################### --
#### ♂️ MALE CONDITIONING ♂️ #### 
########################### --


# 4:1 
four_to_one_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_t2b1.xlsx")
four_to_one_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_t2b2.xlsx")
# Binding the 4:1 data
four_to_one_male <- rbind(four_to_one_male_b1, four_to_one_male_b2)

# 1:4 
one_to_four_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m1-4_t2b1.xlsx")
one_to_four_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m1-4_t2b2.xlsx")
# Binding the 1:4 data
one_to_four_male <- rbind(one_to_four_male_b1, one_to_four_male_b2)

# 4:1 + 1:4 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b1.xlsx")
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/treatment_2/rawdata_m4-1_1-4_t2b2.xlsx")

# Binding the 4:1/1:4 data
fourone_onefour_male <- rbind(fourone_onefour_male_b1, fourone_onefour_male_b2)



## CHANGING THE VARIABLE NAMEAS OF THE DATA - PIVOTING A DATA FRAME -- 
# 4:1
fourone_m <- four_to_one_male  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")
# 1:4 
onefour_m <- one_to_four_male  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
# 4:1 and 1:4 
combined_m <- fourone_onefour_male %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")







###### ###### ###### ###### DATA VISUALISATION ###### ###### ###### ###### 
###### 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊 📊📊📊📊📊📊


# Creating a function for a boxplot, 
# this will allow one to use the feeding data for different data sets 


#################################### Feeding Results Function Plot ####################################

feeding_results <- function(summary_data,boxplot_fill_colour ) {
  ggplot(summary_data, aes(x = diet, y = fly_numbers, fill = diet)) + #, pattern = diet))+ 
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
         y = "Number of flies per diet patch", 
         title = "")+
    scale_fill_manual(values = boxplot_fill_colour) +  # Set fill colors for the boxplot
    # scale_pattern_manual(values = c("stripe", "none", "stripe", "none")) +
    theme(legend.position = "none") +
    ylim(-0.01, 6) 
  
}

########################################################################################################################### --


################  CODING FOR EACH OF THE DIFFERENT CONDITIONS #################### 


# Plot function included, followed by the specific data set, 
# followed by the appropriate colours for the plot depending on the assay 


##################### --
## OvoD1 FEMALES ####
##################### --
of_1_4 <- feeding_results(onefour_of, boxplot_fill_colour = c("lightblue", "lightblue"))
of_4_1  <- feeding_results(fourone_of, boxplot_fill_colour = c("#FDECCD","#FDECCD")) 
of_combined  <- feeding_results(combined_of, boxplot_fill_colour = c("lightblue", "lightblue","#FDECCD","#FDECCD"))

of_1_4 <- feeding_results(onefour_of, boxplot_fill_colour = viridis(5))
of_4_1  <- feeding_results(fourone_of, boxplot_fill_colour = viridis(3))
of_combined  <- feeding_results(combined_of, boxplot_fill_colour = viridis(4))



library(viridis)

# Generate colors using viridis palette for the third plot
colors_of_combined <- viridis(5)

# Extract colors for first plot (matching first two variables of the third plot)
colors_first_plot <- colors_of_combined[1:2]

# Extract colors for second plot (matching last two variables of the third plot)
colors_second_plot <- colors_of_combined[3:4]

# Apply colors to the plots
of_1_4 <- feeding_results(onefour_of, boxplot_fill_colour = viridis_colors[5:6])
of_4_1 <- feeding_results(fourone_of, boxplot_fill_colour = viridis_colors[7:8])
of_combined <- feeding_results(combined_of, boxplot_fill_colour = viridis_colors[5:8])



## Using grid.arrange to put the plots together
ovod1_female_feeding <- grid.arrange(of_1_4, of_4_1, of_combined,
                                     nrow = 1,
                                     widths = c(0.5,0.5,1),
                                     heights = c(1))


############################## --
## VIRGIN WILD TYPE FEMALES ####
############################## --
vf_1_4  <- feeding_results(onefour_v, boxplot_fill_colour = c("lightblue", "lightblue"))
vf_4_1 <- feeding_results(fourone_v, boxplot_fill_colour = c("#FDECCD","#FDECCD")) 
vf_combined <- feeding_results(combined_v, boxplot_fill_colour = c("lightblue", "lightblue","#FDECCD","#FDECCD"))


vf_1_4 <- feeding_results(onefour_v, boxplot_fill_colour = colors_first_plot)
vf_4_1 <- feeding_results(fourone_v, boxplot_fill_colour = colors_second_plot)
vf_combined <- feeding_results(combined_v, boxplot_fill_colour = colors_of_combined)


## Using grid.arrange to put the plots together
virgin_female_feeding <- grid.arrange(vf_1_4, vf_4_1, vf_combined,
                                       nrow = 1,
                                       widths = c(0.5,0.5,1),
                                       heights = c(1))


###################### --
## WILD TYPE MALES ####
##################### --
m_1_4  <- feeding_results(onefour_m, boxplot_fill_colour = viridis_colors[5:6])
m_4_1 <- feeding_results(fourone_m, boxplot_fill_colour = viridis_colors[7:8])
m_combined <- feeding_results(combined_m, boxplot_fill_colour = viridis_colors[5:8])


## Using grid.arrange to put the plots together
male_feeding <- grid.arrange(m_1_4, m_4_1, m_combined,
                                      nrow = 1,
                                      widths = c(0.5,0.5,1),
                                      heights = c(1))

m_1_4 + labs(tag = "a")



m_1_4 <- feeding_results(onefour_m, boxplot_fill_colour = colors_first_plot)
m_4_1 <- feeding_results(fourone_m, boxplot_fill_colour = colors_second_plot)
m_combined  <- feeding_results(combined_m, boxplot_fill_colour = colors_of_combined)
 m_1_4 + m_4_1

 onefour_m

 library(ggplot2)
 

 




## OVERALL GRID 
overall_feeding <- grid.arrange(m_1_4 + ggtitle("Male Conditioning"), m_4_1 + ggtitle("Male Conditioning"), m_combined + ggtitle("Male Conditioning"),
                                of_1_4 + ggtitle("OvoD1 Female Conditioning"), of_4_1 + ggtitle("OvoD1 Female Conditioning"), of_combined + ggtitle("OvoD1 Female Conditioning"),
                                vf_1_4 + ggtitle("Virgin Female Conditioning"), vf_4_1+ ggtitle("Virgin Female Conditioning"), vf_combined+ ggtitle("Virgin Female Conditioning"),  
                                nrow = 3,
                                widths = c(0.5,0.5,1),
                                heights = c(1,1,1))















#################################################################################################################################### --
## Additional code ####

#### A Violin Plot ####
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


# Male code for example 
feeding_results_violin(fourone_onefour_male_long, violin_fill_color = c("lightblue", "lightblue","#FDECCD","#FDECCD"))



