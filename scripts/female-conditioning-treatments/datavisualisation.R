## virgin visualisation 

####### Virgin Conditioning - 4:1 ####### 
#### INSTALL PACKAGES 
library(tidyverse)
library(readxl)
library(patchwork)
library(colorBlindness)
library(ggplot2)
library(ggpattern)

# Packages
library(tidyverse)
library(lmerTest)
library(readxl)

# Raw data
## data on treatment 2 block 1 male feeding

## Creating a path to the scripts within block 1 and block 2 paths
## use for when you get both blocks
pathvirgin <- "data/female_conditioning/virgin"

## This creates  function
## Path is interchangeable with path 2 
read_raw_virgin <-function(path = pathvirgin, pattern_to_exclude = "4-1_1-4"){
  list_of_files <- list.files(path = pathvirgin,
                              pattern = "rawresults", full.names = T)
  list_of_files %>%
    map_dfr(~read_excel(.x) %>% mutate(id = .x, .before = "observation")) %>% #.x is the vector you're inputting? so adding a section of id is the name of the data and the folder its in 
    pivot_longer(cols = c(4:7), ## this will put all the data files into long data, easy to read
                 names_to = "diet",
                 values_to = "fly_numbers") %>%
    drop_na(fly_numbers) ## because the data files are being combined, dropping na where certain data scripts should not be included
}

## read_raw is the function created, and path shows the path made, so the list of files
read_raw_virgin(pathvirgin) 

## creating an actual data set that will read the paths
# first data frame - purr package 
df_virgin <- pathvirgin%>% map_df(~read_raw_virgin(.x)) #.x is a string or NULL - only applies to dfr apparently


#### Virgin visualisation 
#### Upload data for plot - median calculated 
four_to_one_virgin <- read_excel("data/female_conditioning/virgin/rawresults_4-1_virgin.xlsx")
one_to_four_virgin <- read_excel("data/female_conditioning/virgin/rawresults_1-4_virgin.xlsx")
fourone_onefour_virgin <- read_excel("data/female_conditioning/virgin/rawresults_4-1_1-4_virgin.xlsx")




## Making the median data long 
four_to_one_virgin_long <- four_to_one_virgin  %>% 
  pivot_longer(cols = ("4:1 Conditioned":"4:1 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
 
#´
one_to_four_virgin_long <- one_to_four_virgin  %>% 
  pivot_longer(cols = ("1:4 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 
#
fourone_onefour_virgin_long <- fourone_onefour_virgin %>% 
  pivot_longer(cols = ("4:1 Conditioned":"1:4 Unconditioned"), names_to = "diet", values_to = "fly_numbers") 

## creating a function to pull the median
fly_numbers_summary()<- function(data, group_col) {
  summary <- data %>%
    group_by(plate, {{ group_col }}) %>%
    summarise(median = median(fly_numbers))
  return(summary)
}


four_one_virgin_summary <- fly_numbers_summary(four_to_one_virgin_long, diet)
one_four_virgin_summary <- fly_numbers_summary(one_to_four_virgin_long, diet)
fourone_onefour_virgin_summary <- fly_numbers_summary(fourone_onefour_virgin_long, diet)





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
feeding_results(one_to_four_virgin_long, boxplot_fill_color = c("lightblue", "lightblue"))
feeding_results(four_to_one_virgin_long, boxplot_fill_color = c("#FDECCD","#FDECCD")) 
feeding_results(fourone_onefour_virgin_long, boxplot_fill_color = c("lightblue", "lightblue","#FDECCD","#FDECCD"))



