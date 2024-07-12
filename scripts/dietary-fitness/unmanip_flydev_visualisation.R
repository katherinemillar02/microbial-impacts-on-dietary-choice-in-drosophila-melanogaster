## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)

## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)





################################################ FLY DATA VISUALISATION ####

## Read data in
fly_fitness <- read_excel("data//fitness_development/fly_data.xlsx")

############

## Separating the data into female and male columns 
fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 


fly_fitness_tidy_2 <- fly_fitness %>%
  pivot_longer(cols = c(females, males), names_to = "sex", values_to = "count") %>%
  mutate(sex_treatment = paste(sex, treatment, sep = "_"))




## Subsetting the data into female and male plots

## Just Female data
females_data <- subset(fly_fitness, select = c(time_hours, females, treatment))


## Just a female plot
fly_females_plot <- ggplot(females_data, aes(x = time_hours, y = females, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged") +
  labs(fill = "Treatment")





## Just Male data
males_data <- subset(fly_fitness, select = c(time_hours, males, treatment))


## Just a Male plot
fly_males_plot <- ggplot(males_data, aes(x = time_hours, y = males, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Males Emerged") +
  labs(fill = "Treatment")



## The Male and Female Plots 

fly_females_plot /
  fly_males_plot



#### The second dataset 
## Where the data has been re-organised to only show one data per day
fly_fitness_2 <- read_excel("data/fitness_development/fly_data_2.xlsx")

## Subsetting the data for the second dataset 
females_data_2 <- subset(fly_fitness_2, select = c(time_hours, females, treatment))


## Visualising the data for the second dataset 
fly_females_plot_2 <- ggplot(females_data, aes(x = time_hours, y = females, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged") +
  labs(fill = "Treatment")

fly_females_boxplot_2 <- ggplot(females_data, aes(x = time_hours, y = females, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha =.4, position = position_dodge (width = 20)) + #width =10 ) +
  geom_point(aes(fill = treatment),
             size = 1, shape = 21,
             position = position_jitterdodge(jitter.width = 6, dodge.width = 20)) +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  scale_x_continuous(breaks = unique(females_data_2$time_hours), labels = unique(females_data_2$time_hours)) +
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


## Subsetting the data for the second male data set 
males_data_2 <- subset(fly_fitness_2, select = c(time_hours, males, treatment))


## Visualising the data for the second dataset for males 
fly_males_plot_2 <- ggplot(males_data_2, aes(x = time_hours, y = males, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Males Emerged") +
  labs(fill = "Treatment")

## Boxplot
fly_males_boxplot_2 <- ggplot(males_data_2, aes(x = time_hours, y = males, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha =.4, position = position_dodge (width = 20)) + #width =10 ) +
  geom_point(aes(fill = treatment),
             size = 1, shape = 21,
             position = position_jitterdodge(jitter.width = 6, dodge.width = 20)) +
  scale_fill_manual(values = viridis_colors[c(4,8)]) +
  scale_x_continuous(breaks = unique(males_data_2$time_hours), labels = unique(males_data_2$time_hours)) +
  theme_classic() +
  theme(legend.position = "none",
        strip.placement = "outside",
        legend.direction = "vertical",
        strip.background = element_blank(),  
        strip.text = element_blank()) +  
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Males Emerged",
       fill = "Treatment") +
  facet_grid(~time_hours, scales = "free_x")


## The Male and Female Plots with the newly arranged datasets

fly_females_plot_2 /
  fly_males_plot_2



fly_females_boxplot_2 /
  fly_males_boxplot_2








