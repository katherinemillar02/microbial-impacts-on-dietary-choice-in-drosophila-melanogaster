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

#### COMBINING THE BLOCKS 

# The Dietary Assays 

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







##### FUNCTION FOR ADDING // CHANGING VARIABLE NAMES #####  #####  #####  ##### 

variable_data <- function(data, cols, names_to = "diet", values_to = "fly_numbers") {
  data %>% 
    pivot_longer(cols = all_of(cols), names_to = names_to, values_to = values_to)
}

fourone_of <- variable_data(four_to_one_ovod1, cols = c("4:1 Conditioned", "4:1 Unconditioned"))
onefour_of <- variable_data(one_to_four_ovod1, cols = c("1:4 Conditioned", "1:4 Unconditioned"))
combined_of <- variable_data(fourone_onefour_ovod1, cols = c("4:1 Conditioned", "1:4 Conditioned", "4:1 Unconditioned", "1:4 Unconditioned"))

fourone_m <- variable_data(four_to_one_male, cols = c("4:1 Conditioned", "4:1 Unconditioned"))
onefour_m <- variable_data(one_to_four_male, cols = c("1:4 Conditioned", "1:4 Unconditioned"))
combined_m <- variable_data(fourone_onefour_male, cols = c("4:1 Conditioned", "1:4 Conditioned", "4:1 Unconditioned", "1:4 Unconditioned"))

fourone_v <- variable_data(four_to_one_virgin, cols = c("4:1 Conditioned", "4:1 Unconditioned"))
onefour_v <- variable_data(one_to_four_virgin, cols = c("1:4 Conditioned", "1:4 Unconditioned"))
combined_v <- variable_data(fourone_onefour_virgin, cols = c("4:1 Conditioned", "1:4 Conditioned", "4:1 Unconditioned", "1:4 Unconditioned"))

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  #####  #####  ##### 



###### ###### ###### ###### DATA VISUALISATION ###### ###### ###### ###### 
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

# Example of how to call the function with data
# feeding_results(my_data, c("#FF5733", "#33FF57"))







################  CODING FOR EACH OF THE DIFFERENT CONDITIONS #################### 


# Plot function included, followed by the specific data set, 
# followed by the appropriate colours for the plot depending on the assay 

## Setting colours 

viridis_colors <- inferno(10)
##################### --
## OvoD1 FEMALES ####
##################### --
of_1_4 <- feeding_results(onefour_of, boxplot_fill_colour = viridis_colors[c(9,9)])



of_4_1 <- feeding_results(fourone_of, boxplot_fill_colour = viridis_colors[c(7,7)])

of_combined <- feeding_results(combined_of, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])

of_1_4 + of_4_1


## Using grid.arrange to put the plots together
ovod1_female_feeding <- grid.arrange(of_1_4, of_4_1, of_combined,
                                     nrow = 1,
                                     widths = c(0.5,0.5,1),
                                     heights = c(1))


############################## --
## VIRGIN WILD TYPE FEMALES ####
############################## --
vf_1_4 <- feeding_results(onefour_v, boxplot_fill_colour = viridis_colors[c(9,9)])
vf_4_1 <- feeding_results(fourone_v, boxplot_fill_colour  = viridis_colors[c(7,7)])
vf_combined <- feeding_results(combined_v, boxplot_fill_colour = viridis_colors[c(9,9,7,7)])



## Using grid.arrange to put the plots together
virgin_female_feeding <- grid.arrange(vf_1_4, vf_4_1, vf_combined,
                                       nrow = 1,
                                       widths = c(0.5,0.5,1),
                                       heights = c(1))


hi + vf_combined

virgin_female_feeding <- grid.arrange(hi, vf_combined,
                                      nrow = 1,
                                      widths = c(0.5,0.5,1),
                                      heights = c(1))


library(webshot)
library(htmltools)

# Save the table as an HTML file
save_html(hi, file = "table.html")

# Convert HTML to an image
webshot("table.html", "table.png")
img <- readPNG("table.png")
table_grob <- rasterGrob(img, interpolate = TRUE)

hey <- grid.arrange(
  hi, vf_combined,
  nrow = 1,
  widths = c(0.5, 0.5),
  heights = c(1)
)

###################### --
## WILD TYPE MALES ####
##################### --
m_1_4  <- feeding_results(onefour_m, boxplot_fill_colour  = viridis_colors[c(9,9)])
m_4_1 <- feeding_results(fourone_m, boxplot_fill_colour  = viridis_colors[c(7,7)])
m_combined <- feeding_results(combined_m, boxplot_fill_colour  = viridis_colors[c(9,9,7,7)])

combined_plot <- grid.arrange(m_1_4, m_4_1, nrow = 1, top = "Model: cbind(Conditioned, Unconditioned) ~ ratio + (1|plate) + (1|observation)")




## Using grid.arrange to put the plots together
male_feeding <- grid.arrange(m_1_4, m_4_1, m_combined,
                                      nrow = 1,
                                      widths = c(0.5,0.5,1),
                                      heights = c(1))



 

## OVERALL GRID 
overall_feeding <- grid.arrange(m_1_4 + ggtitle("Male Conditioning"), m_4_1 + ggtitle("Male Conditioning"), m_combined + ggtitle("Male Conditioning"),
                                of_1_4 + ggtitle("OvoD1 Female Conditioning"), of_4_1 + ggtitle("OvoD1 Female Conditioning"), of_combined + ggtitle("OvoD1 Female Conditioning"),
                                vf_1_4 + ggtitle("Virgin Female Conditioning"), vf_4_1 + ggtitle("Virgin Female Conditioning"), vf_combined+ ggtitle("Virgin Female Conditioning"),  
                                nrow = 3,
                                widths = c(0.5,0.5,1),
                                heights = c(1,1,1))





#### MAKING PLOTS FOR SEPARATE BLOCKS  ####



#### MALES ####

## Reading in the block data

## 4:1 

# Block 1
four_to_one_male_b1 <- read_excel("data/male_conditioning/rawdata_m4-1_t2b1.xlsx")

## Changing the variable names of Block 1 
fourone_of_b1 <- four_to_one_male_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

# Block 2 
four_to_one_male_b2 <- read_excel("data/male_conditioning/rawdata_m4-1_t2b2.xlsx")

## Changing the variable names of Block 2
fourone_of_b2 <- four_to_one_male_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 






## 1:4 

# Block 1
one_to_four_male_b1 <- read_excel("data/male_conditioning/rawdata_m1-4_t2b1.xlsx")

# Changing the variable names of Block 1
onefour_of_b1 <- one_to_four_male_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 


# Block 2
one_to_four_male_b2 <- read_excel("data/male_conditioning/rawdata_m1-4_t2b2.xlsx")

# Changing the variable names of Block 2 
onefour_of_b2 <- one_to_four_male_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 





# 4:1 + 1:4 

# Block 1 
fourone_onefour_male_b1 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_t2b1.xlsx")

# Changing the variable names of Block 1 
fouroneonefour_male_b1 <- fourone_onefour_male_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

# Block 2
fourone_onefour_male_b2 <- read_excel("data/male_conditioning/rawdata_m4-1_1-4_t2b2.xlsx")

# Changing the variable names of Block 2
fouroneonefour_male_b2 <- fourone_onefour_male_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 



#### DATA VISUALISATION OF BLOCKS 

## MALE CONDITIONED DIETS

## 1:4 
m_1_4_b1  <- feeding_results(onefour_of_b1 , boxplot_fill_colour = viridis_colors[5:6])
m_1_4_b2  <- feeding_results(onefour_of_b2, boxplot_fill_colour = viridis_colors[5:6])




## 4:1 
m_4_1_b1 <- feeding_results(fourone_of_b1 , boxplot_fill_colour = viridis_colors[7:8])
m_4_1_b2 <- feeding_results(fourone_of_b2, boxplot_fill_colour = viridis_colors[7:8])



## 4:1 and 1:4 
m_combined_b1 <- feeding_results(fouroneonefour_male_b1, boxplot_fill_colour = viridis_colors[5:8])
m_combined_b2 <- feeding_results(fouroneonefour_male_b2, boxplot_fill_colour = viridis_colors[5:8])



## Adding titles to the separate blocks,
## keeping the names the same 
m_4_1_b1 <- m_4_1_b1 + ggtitle("Male Block 1")
m_4_1_b2 <- m_4_1_b2 + ggtitle("Male Block 2")
m_1_4_b1 <- m_1_4_b1 + ggtitle("Male Block 1")
m_1_4_b2 <- m_1_4_b2 + ggtitle("Male Block 2")
m_combined_b1  <- m_combined_b1  + ggtitle("Male Block 1")
m_combined_b2 <- m_combined_b2+ ggtitle("Male Block 2")



## Generating a combined plot of the different blocks 
male_oviposition <- grid.arrange(
  m_4_1_b1, m_1_4_b1, m_combined_b1 ,
  m_4_1_b2, m_1_4_b2, m_combined_b2,
  ncol = 3,
  nrow = 2,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1)
)

male_feeding_absolute <- grid.arrange(
   m_combined_b1 ,
  m_combined_b2,
  ncol = 1,
  nrow = 2,
  widths = c(0.5),
  heights = c(1, 1)
)





#### VIRGIN FEMALE CONDITIONED DIETS ######## 

######## 4:1 ######## 



# Reading the Data in: 

## Block 1
four_to_one_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b1.xlsx")

## Changing the variable names of Block 1
fourone_v_1 <- four_to_one_virgin_b1   %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## Block 2
four_to_one_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b2.xlsx")

## Changing the variable names of Block 2
fourone_v_2 <- four_to_one_virgin_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## Block 3
four_to_one_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b3.xlsx")

## Changing the variable names of Block 3
fourone_v_3 <- four_to_one_virgin_b3 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 


## Block 4
four_to_one_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin_b4.xlsx")

## Changing the variable names of Block 4 
fourone_v_4 <- four_to_one_virgin_b4 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")








 

###### 1:4 ###### 

# Block 1
one_to_four_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b1.xlsx")

## Changing the variable names in block 1
onefour_v_1 <- one_to_four_virgin_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")



# Block 2
one_to_four_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b2.xlsx")

## Changing the variable names in block 2
onefour_v_2 <- one_to_four_virgin_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")




# Block 3
one_to_four_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b3.xlsx")

## Changing the variable names in block 3
onefour_v_3 <- one_to_four_virgin_b3  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 





# Block 4
one_to_four_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin_b4.xlsx")

## Changing the variable names in block 3
onefour_v_4 <- one_to_four_virgin_b4  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 





############# 4:1 and 1:4 ##########

# Block 1
fourone_onefour_virgin_b1 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b1.xlsx")

# Changing the variable names in block 1
combined_v_1 <- fourone_onefour_virgin_b1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 2 
fourone_onefour_virgin_b2 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b2.xlsx")


# Changing the variable names in block 2
combined_v_2 <- fourone_onefour_virgin_b2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 3
fourone_onefour_virgin_b3 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b3.xlsx")

# Changing the variable names in block 3
combined_v_3 <- fourone_onefour_virgin_b3 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# Block 4 
fourone_onefour_virgin_b4 <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin_b4.xlsx")


# Changing the variable names in block 4
combined_v_4 <- fourone_onefour_virgin_b4%>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")




### DATA VISUALISATION FOR VIRGIN CONDITIONING 

##  1:4 
v_1_4_b1  <- feeding_results(onefour_v_1 , boxplot_fill_colour = viridis_colors[5:6])
v_1_4_b2  <- feeding_results(onefour_v_2, boxplot_fill_colour = viridis_colors[5:6])
v_1_4_b3  <- feeding_results(onefour_v_3 , boxplot_fill_colour = viridis_colors[5:6])
v_1_4_b4  <- feeding_results(onefour_v_4, boxplot_fill_colour = viridis_colors[5:6])




## 4:1 
v_4_1_b1 <- feeding_results(fourone_v_1 , boxplot_fill_colour = viridis_colors[7:8])
v_4_1_b2 <- feeding_results(fourone_v_2, boxplot_fill_colour = viridis_colors[7:8])
v_4_1_b3 <- feeding_results(fourone_v_3 , boxplot_fill_colour = viridis_colors[7:8])
v_4_1_b4 <- feeding_results(fourone_v_4, boxplot_fill_colour = viridis_colors[7:8])



## 4:1 and 1:4 
v_combined_b1 <- feeding_results(combined_v_1, boxplot_fill_colour = viridis_colors[5:8])
v_combined_b2 <- feeding_results(combined_v_2, boxplot_fill_colour = viridis_colors[5:8])
v_combined_b3 <- feeding_results(combined_v_3, boxplot_fill_colour = viridis_colors[5:8])
v_combined_b4 <- feeding_results(combined_v_4, boxplot_fill_colour = viridis_colors[5:8])



## Setting the titles 

## 4:1 
v_4_1_b1 <- v_4_1_b1 + ggtitle("Virgin Female Block 1")
v_4_1_b2 <- v_4_1_b2 + ggtitle("Virgin Female  Block 2")
v_4_1_b3 <- v_4_1_b3 + ggtitle("Virgin Female  Block 3")
v_4_1_b4 <- v_4_1_b4 + ggtitle("Virgin Female  Block 4")


## 1:4 
v_1_4_b1 <- v_1_4_b1 + ggtitle("Virgin Female Block 1")
v_1_4_b2 <- v_1_4_b2 + ggtitle("Virgin Female Block 2")
v_1_4_b3 <- v_1_4_b3 + ggtitle("Virgin Female Block 3")
v_1_4_b4 <- v_1_4_b4 + ggtitle("Virgin Female Block 4")


## 4:1 and 1:4 
v_combined_b1  <- v_combined_b1  + ggtitle("Virgin Female Block 1")
v_combined_b2 <- v_combined_b2 + ggtitle("Virgin Female Block 2")
v_combined_b3  <- v_combined_b3  + ggtitle("Virgin Female Block 3")
v_combined_b4 <- v_combined_b4 + ggtitle("Virgin Female Block 4")


## Creating a combined plots for virgin separate blocks 
virgin_oviposition <- grid.arrange(
  v_4_1_b1, v_1_4_b1, v_combined_b1 ,
  v_4_1_b2, v_1_4_b2, v_combined_b2,
  v_4_1_b3, v_1_4_b3, v_combined_b3 ,
  v_4_1_b4, v_1_4_b4, v_combined_b4,
  ncol = 3,
  nrow = 4,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1, 1,1)
)





##################### --
## OvoD1 FEMALES ####
##################### --


# 4:1 

## Block 1 
four_to_one_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b1.xlsx")

# Adding variable names to Block 1
fourone_of_1 <- four_to_one_ovod1_b1  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 


## Block 2 
four_to_one_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_ovod1_b2.xlsx")

# Adding variable names to Block 2
fourone_of_2 <- four_to_one_ovod1_b2  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


# 1:4 

## Block 1
one_to_four_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b1.xlsx")

## Adding variable names to Block 1
onefour_of_1 <- one_to_four_ovod1_b1  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## Block 2
one_to_four_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_1-4_ovod1_b2.xlsx")

## Adding variable names to Block 2
onefour_of_2 <- one_to_four_ovod1_b2  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 


# 4:1 and 1:4 

## Block 1
fourone_onefour_ovod1_b1 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b1.xlsx")

# Adding variable names to block 1
combined_of_1 <- fourone_onefour_ovod1_b1 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")

## Block 2
fourone_onefour_ovod1_b2 <- read_excel("data/female_conditioning/ovod1/rawresults_4-1_1-4_ovod1_b2.xlsx")

## Adding variable names to Block 2 
combined_of_2 <- fourone_onefour_ovod1_b2 %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers")


#### VISUALISING THE DATA FOR OVOD1 FEMALES 

## 1:4 
of_1_4_b1  <- feeding_results(onefour_of_1 , boxplot_fill_colour = viridis_colors[5:6])
of_1_4_b2  <- feeding_results(onefour_of_2, boxplot_fill_colour = viridis_colors[5:6])


## 4:1 
of_4_1_b1 <- feeding_results(fourone_of_1 , boxplot_fill_colour = viridis_colors[7:8])
of_4_1_b2 <- feeding_results(fourone_of_2, boxplot_fill_colour = viridis_colors[7:8])


## 4:1 and 1:4 
of_combined_b1 <- feeding_results(combined_of_1, boxplot_fill_colour = viridis_colors[5:8])
of_combined_b2 <- feeding_results(combined_of_2, boxplot_fill_colour = viridis_colors[5:8])



## Adding titles to OvoD1 plots 
of_4_1_b1 <- of_4_1_b1 + ggtitle("OvoD1 Female Block 1")
of_4_1_b2 <- of_4_1_b2 + ggtitle("OvoD1 Female  Block 2")
of_1_4_b1 <- of_1_4_b1 + ggtitle("OvoD1 Female Block 1")
of_1_4_b2 <- of_1_4_b2 + ggtitle("OvoD1 Female Block 2")
of_combined_b1  <- of_combined_b1  + ggtitle("OvoD1 Female Block 1")
of_combined_b2 <- of_combined_b2 + ggtitle("OvoD1 Female Block 2")


ovod1_oviposition <- grid.arrange(
  of_4_1_b1, of_1_4_b1, of_combined_b1 ,
  of_4_1_b2, of_1_4_b2, of_combined_b2,
  ncol = 3,
  nrow = 2,
  widths = c(0.5, 0.5, 1),
  heights = c(1, 1)
)




