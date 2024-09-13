## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)




#### The second dataset 
## Where the data has been re-organised to only show one data per day

## Reading the dataframe in 
fly_fitness_UMFE_2 <- read_excel("data/fitness_development/fly_data_2.xlsx")




## Subsetting the data for the second dataset 
females_data_UMFE_2 <- subset(fly_fitness_UMFE_2, select = c(time_hours, females, treatment))

## Subsetting the data for the second male data set 
males_data_UMFE_2 <- subset(fly_fitness_UMFE_2, select = c(time_hours, males, treatment))



## Coding required bits for the plot 
viridis_colors <- viridis(10)



                                        ## Boxplot - Females ## 
females_data_UMFE_2_boxplot <- ggplot(females_data_UMFE_2, aes(x = time_hours, y = females, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha =.4, position = position_dodge (width = 20)) + #width =10 ) +
  geom_point(aes(fill = treatment),
             size = 1, shape = 21,
             position = position_jitterdodge(jitter.width = 6, dodge.width = 20)) +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  scale_x_continuous(breaks = unique(females_data_UMFE_2$time_hours), labels = unique(females_data_UMFE_2$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank())+
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged",
       fill = "Treatment") +
  facet_grid(~time_hours, scales = "free_x")





                                          ## Boxplot - Males ## 
males_data_UMFE_2_boxplot <- ggplot(males_data_UMFE_2, aes(x = time_hours, y = males, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha =.4, position = position_dodge (width = 20)) + #width =10 ) +
  geom_point(aes(fill = treatment),
             size = 1, shape = 21,
             position = position_jitterdodge(jitter.width = 6, dodge.width = 20)) +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  scale_x_continuous(breaks = unique(males_data_UMFE_2$time_hours), labels = unique(males_data_UMFE_2$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank())+
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Males Emerged",
       fill = "Treatment") +
  facet_grid(~time_hours, scales = "free_x")



## The Male and Female Plots with the newly arranged datasets

UMFE_fly_plot <- 
  females_data_UMFE_2_boxplot /
  males_data_UMFE_2_boxplot





## Saving a plot
ggsave(filename = "UMFE_fly_plot.png", 
       plot = UMFE_fly_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)




