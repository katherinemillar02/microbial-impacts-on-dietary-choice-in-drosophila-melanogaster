## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)

## Read data in
pupae_fitness <- read_excel("data/fitness_experiment/pupae_data.xlsx")

## choosing colours from virids to use
viridis_colors <- viridis(10)


#### PUPAE PLOT ####

## Creating a plot (bar plot for now)
pupae_fitness_plot <- ggplot(pupae_fitness, aes(x = `time_hours`, y = pupae, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_line(aes(col = treatment), position = position_dodge(width = 1)) +
  scale_fill_manual(values = viridis_colors[c(3,6)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right")+
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Pupae from eggs") +
  labs(fill = "Treatment")






#### FLY FITNESS PLOT ####
fly_fitness_plot <- ggplot(fly_fitness_tidy_females, aes(x = `time_hours`, y = count, fill = females)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_line(aes(col = treatment), position = position_dodge(width = 1)) +
  scale_fill_manual(values = viridis_colors[c(1,7,3,8)], labels =  c("Female Conditioned", "Female Unconditioned", "Male Conditioned", 'Male Unconditioned')) +
  theme_classic() + 
  theme(legend.position = "top",
        legend.justification = "right")+
  labs(x = "Time (hours) since eggs laid", 
       y = "Flies (male or female)") +
  labs(fill = "Treatment")



## Read data in
fly_fitness <- read_excel("data/fitness_experiment/fly_data.xlsx")

females_data <- subset(fly_fitness, select = c(time_hours, females, treatment))


fly_females_plot <- ggplot(females_data, aes(x = time_hours, y = females, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(3,6)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged") +
  labs(fill = "Treatment")




males_data <- subset(fly_fitness, select = c(time_hours, males, treatment))


fly_males_plot <- ggplot(males_data, aes(x = time_hours, y = males, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(3,6)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Males Emerged") +
  labs(fill = "Treatment")


## The Male and Female Plots 

fly_females_plot /
  fly_males_plot



# 
# 
# 
# fly_fitness_tidy_females <- tidyr::pivot_longer(data = fly_fitness ,
#                                  cols = c(females),
#                                  names_to = "females",
#                                  values_to = "count")
# 
# fly_fitness_tidy_males <- tidyr::pivot_longer(data = fly_fitness ,
#                                                 cols = c(males),
#                                                 names_to = "males",
#                                                 values_to = "count")
# 
# 
# fly_fitness_tidy_females$females <- paste(fly_fitness_tidy$females, fly_fitness_tidy$treatment, sep = "_")
# 
# 








# # Assuming your time_hours column is in fly_fitness_tidy dataframe
# fly_fitness_tidy$time_category <- cut(fly_fitness_tidy$time_hours, breaks = c(350, 450, 550), labels = c("350-450", "450-550"), include.lowest = TRUE)
# 
# # Assuming your time_hours column is in fly_fitness_tidy dataframe
# # Creating two separate dataframes based on time_category
# 
# # Subset for time category 350-450
# fly_fitness_350_450 <- fly_fitness_tidy[fly_fitness_tidy$time_category == "350-450", ]
# 
# # Subset for time category 450-550
# fly_fitness_450_550 <- fly_fitness_tidy[fly_fitness_tidy$time_category == "450-550", ]
# 
# 
# 
# one <- ggplot(fly_fitness_350_450 , aes(x = time_hours, y = count, fill = sex_treatment)) +
#   geom_bar(position="dodge", stat="identity") +
#   theme_bw() + 
#   theme(legend.position = "none") + 
#   ylim(0,30) +
#   xlim(350,450) 
# 
# two <- ggplot(fly_fitness_450_550 , aes(x = time_hours, y = count, fill = sex_treatment)) +
#   geom_bar(position="dodge", stat="identity") + 
#   theme_bw() +
#   theme(legend.position = "none") + 
#   ylim(0,30)+
#   xlim(450,550) + 
#   ylab("") 
# 
# one + two



