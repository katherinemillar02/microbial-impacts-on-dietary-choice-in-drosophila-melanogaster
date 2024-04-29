## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)

## Read data in
pupae_fitness <- read_excel("data/puape_data.xlsx")

## choosing colours from virids to use
viridis_colors <- viridis(10)


#### PUPAE PLOT ####

## Creating a plot (bar plot for now)
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
fly_fitness <- read_excel("data/fly_data.xlsx")

females_data <- subset(fly_fitness, select = c(time_hours, females, treatment))


fly_females_plot <- ggplot(females_data, aes(x = time_hours, y = females, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged") +
  labs(fill = "Treatment")




males_data <- subset(fly_fitness, select = c(time_hours, males, treatment))


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




total_females <- sum(females_data$females, na.rm = TRUE)
total_males <- sum(df$males, na.rm = TRUE)


total_females <- sum(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females"])

# Calculate the total number of females for conditioned treatment
total_conditioned_females <- sum(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females" & fly_fitness_tidy$treatment == "conditioned"], na.rm = TRUE)

# Calculate the total number of females for unconditioned treatment
total_unconditioned_females <- sum(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females" & fly_fitness_tidy$treatment == "unconditioned"], na.rm = TRUE)


## Does this code right for Females v Males 


total_conditioned_males <- sum(fly_fitness_tidy$count[fly_fitness_tidy$gender == "males" & fly_fitness_tidy$treatment == "conditioned"], na.rm = TRUE)

# Calculate the total number of females for unconditioned treatment
total_unconditioned_males <- sum(fly_fitness_tidy$count[fly_fitness_tidy$gender == "males" & fly_fitness_tidy$treatment == "unconditioned"], na.rm = TRUE)



totals <- data.frame(
  Gender = c("Conditioned Females", "Unconditioned Females", "Conditioned Males", "Unconditioned Males"),
  Total = c(total_conditioned_females, total_unconditioned_females, total_conditioned_males, total_unconditioned_males)
)


totals$Gender <- factor(totals$Gender, levels = c("Conditioned Females", "Unconditioned Females", "Conditioned Males", "Unconditioned Males"))




total_plot <- ggplot(totals, aes(x = Gender, y = Total, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = viridis_colors[c(2,4,6,8)]) +
  labs(title = "",
       y = "Total Flies",
       x = "Sex and Treatment") +
  theme(legend.position = "none") +
  theme_classic()







fly_fitness_tidy <- tidyr::pivot_longer(data = fly_fitness ,
                                 cols = c(females, males),
                                 names_to = "gender",
                                 values_to = "count")




fly_fitness %>% count(females)

total_females <- fly_fitness %>% 
  summarise(total_females = sum(females))




fly_total_plot <- ggplot(fly_fitness_tidy, aes(x = gender, y = count, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_colors[c(3,6)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Females Emerged") +
  labs(fill = "Treatment") + 
  ylim(0,300)







## Finds the sumn
summary_data <- fly_fitness %>%
  group_by(treatment, time_hours) %>%
  summarize(total_females = sum(females, na.rm = TRUE),
            total_males = sum(males, na.rm = TRUE)) %>%
  ungroup()


most_males_by_treatment <- summary_data %>%
  group_by(treatment) %>%
  summarise(max_total_males = max(total_males))

most_females_by_treatment <- summary_data %>%
  group_by(treatment) %>%
  summarise(max_total_males = max(total_males))

data <- summary_data[order(summary_data$total_females, summary_data$total_males), ]


summary_data_2 <- fly_fitness %>%
  group_by(treatment, time_hours) %>%
  summarize(total_females = median(females, na.rm = TRUE),
            total_males = median(males, na.rm = TRUE)) %>%
  ungroup()


most_males_by_treatment_2 <- summary_data_2 %>%
  group_by(treatment) %>%
  summarise(max_total_males = max(total_males))

most_females_by_treatment_2 <- summary_data_2 %>%
  group_by(treatment) %>%
  summarise(max_total_males = max(total_males))

data_2 <- summary_data_2[order(summary_data_2$total_females, summary_data_2$total_males), ]

## same results for median 






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



