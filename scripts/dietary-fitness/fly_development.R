## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)


## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)

################################################ PUPAE ANALYSIS ####


## Reading pupae data in
pupae_fitness <- read_excel("data/puape_data.xlsx")

################################################ PUPAE DATA VISUALISATION ####

## Creating a barplot
## Pupae plot 1 
# This plot uses the raw data set with time and hours set out normally 
## This plot shows two counts per day (usually)
pupae_fitness_plot <- ggplot(pupae_fitness, aes(x = `time (hours)`, y = pupae, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_line(aes(col = treatment), position = position_dodge(width = 1)) +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right")+
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Pupae emerged") +
  labs(fill = "Treatment")





## Reading the second pupae data set in 
# This is the same data, but only shows one collection per day 
# The middle hour point has been found, and the counts of both have been summed 
pupae_fitness_2 <- read_excel("data/pupae_data_2.xlsx")


## The second plot - pupae plot 2
## This plot shows one collection per day, with data merged as described above
pupae_fitness_plot_2 <- ggplot(pupae_fitness_2, aes(x = `time_hours`, y = pupae, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_line(aes(col = treatment), position = position_dodge(width = 1)) +
  scale_fill_manual(values = viridis_colors[c(1,6)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right")+
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Pupae emerged") +
  labs(fill = "Treatment")


################################################ FLY DATA VISUALISATION ####


## Read data in
fly_fitness <- read_excel("data/fly_data.xlsx")

############

## Separating the data into female and male columns 
fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                        cols = c( females, males),
                                        names_to = "sex",
                                        values_to = "count") 




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


## subsetting males data
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
fly_fitness_2 <- read_excel("data/fly_data_2.xlsx")

## Subsetting the data for the second dataset 
females_data_2 <- subset(fly_fitness_2, select = c(time_hours, females, treatment))


## Visualising the data for the second dataset 
fly_females_plot_2 <- ggplot(females_data_2, aes(x = time_hours, y = females, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged") +
  labs(fill = "Treatment")

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


## The Male and Female Plots with the newly arranged datasets

fly_females_plot_2 /
  fly_males_plot_2






##### PUTTING OVERALL EMERGENCE DATA OF FLIES ACROSS VIALS TOGETHER

#### This code shows each vial, for each sex, and for each treatment 
fly_emergence_sex <- fly_fitness_tidy %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, sex, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, sex, sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                   levels = c("conditioned females", "unconditioned females",
                                              "conditioned males", "unconditioned males")))



#### CALCULATIONS 
## Calculating median emergence by vial
## This code combines sex and shows overall emergence
fly_emergence_overall <- fly_fitness_tidy %>%
  filter(sex %in% c("females", "males")) %>%
  group_by(vial, treatment) %>%
  summarise(overall_emergence = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex_treatment = paste(treatment, "overall", sep = " ")) %>%
  mutate(sex_treatment = factor(sex_treatment,
                                   levels = c("conditioned overall", "unconditioned overall")))


## This code shows the median overall emergence (males and females combined) 
vial_overall_emergence_median <- vial_overall_emergence  %>%
  group_by(treatment) %>%
  summarise(median_count = median(overall_emergence, na.rm = TRUE))



## DATA VISUALISA5ION - Visualising overall emergence across vials 
overall_emergence_sex_treatment <- ggplot(fly_emergence_sex, aes(x = sex, y = total_count, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(height=0, width=0.2)) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme(legend.position = "none") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of total flies emerged per vial") +
  labs(fill = "Treatment")+
  ylim(0,80)



## An overall code of emergence per time 
emergence_per_time <- fly_fitness %>%
  group_by(treatment, time_hours) %>%
  summarize(total_females = sum(females, na.rm = TRUE),
            total_males = sum(males, na.rm = TRUE)) %>%
  ungroup()


















