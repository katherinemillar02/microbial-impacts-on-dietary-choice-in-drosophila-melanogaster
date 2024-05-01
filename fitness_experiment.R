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
                                        names_to = "gender",
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

#### A TOTAL PLOT 
fly_emergence_overall <- fly_fitness_tidy %>%
  filter(gender %in% c("females", "males")) %>%
  group_by(vial, gender, treatment) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Gender_Treatment = paste(treatment, gender, sep = " ")) %>%
  mutate(Gender_Treatment = factor(Gender_Treatment,
                                   levels = c("conditioned females", "unconditioned females",
                                              "conditioned males", "unconditioned males")))



#### CALCULATIONS 
## Calculating median emergence by vial

vial_overall_emergence <- fly_fitness_tidy %>%
  filter(gender %in% c("females", "males")) %>%
  group_by(vial, treatment) %>%
  summarise(overall_emergence = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gender_treatment = paste(treatment, "overall", sep = " ")) %>%
  mutate(gender_treatment = factor(gender_treatment,
                                   levels = c("conditioned overall", "unconditioned overall")))


vial_overall_emergence_median <- vial_overall_emergence  %>%
  group_by(treatment) %>%
  summarise(median_count = median(overall_emergence, na.rm = TRUE))






total_females <- sum(females_data$females, na.rm = TRUE)
total_males <- sum(df$males, na.rm = TRUE)


total_females <- sum(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females"])

# Calculate the total number of females for conditioned treatment
total_conditioned_females <- sum(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females" & fly_fitness_tidy$treatment == "conditioned" ], na.rm = TRUE)

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
                                        cols = c( females, males),
                                        names_to = "gender",
                                        values_to = "count") 







fly_fitness %>% count(females)

total_females <- fly_fitness %>% 
  summarise(total_females = sum(females))












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


library(dplyr)

median_conditioned_females <- fly_fitness_tidy %>%
  filter(gender == "females", treatment == "conditioned") %>%
  group_by(vial) %>%
  summarise(median_count = median(count, na.rm = TRUE))

median_unconditioned_females <- fly_fitness_tidy %>%
  filter(gender == "females", treatment == "unconditioned") %>%
  group_by(vial) %>%
  summarise(median_count = median(count, na.rm = TRUE))

median_conditioned_males <- fly_fitness_tidy %>%
  filter(gender == "males", treatment == "conditioned") %>%
  group_by(vial) %>%
  summarise(median_count = median(count, na.rm = TRUE))

median_unconditioned_males <- fly_fitness_tidy %>%
  filter(gender == "males", treatment == "unconditioned") %>%
  group_by(vial) %>%
  summarise(median_count = median(count, na.rm = TRUE))


# Creating separate data frames for each condition
medians_conditioned_females <- data.frame(
  Gender = "Conditioned Females",
  Total = median_conditioned_females
)

medians_unconditioned_females <- data.frame(
  Gender = "Unconditioned Females",
  Total = median_unconditioned_females
)

medians_conditioned_males <- data.frame(
  Gender = "Conditioned Males",
  Total = median_conditioned_males
)

medians_unconditioned_males <- data.frame(
  Gender = "Unconditioned Males",
  Total = median_unconditioned_males
)

# Combining the data frames into a single data frame
medians <- bind_rows(medians_conditioned_females, 
                     medians_unconditioned_females, 
                     medians_conditioned_males, 
                     medians_unconditioned_males)


boxplot <- ggplot(medians, aes(x = Gender, y = Total.median_count, fill = Gender)) +
  geom_boxplot() +
  geom_point(aes(),
             size = 1,
             shape = 1,
             position = position_jitterdodge()) +
  # You can customize the appearance of the plot as needed
  labs(title = "Boxplot of Medians by Condition and Gender",
       x = "Group",
       y = "Median Value") +
  theme_classic()
library(ggplot2)

# Assuming your data has columns 'Gender' and 'Total.median_count' where 'Gender' indicates the gender (male/female), and 'Total.median_count' contains the median values

library(ggplot2)

# Assuming your data has columns 'Gender' and 'Total.median_count' where 'Gender' indicates the gender (male/female), and 'Total.median_count' contains the median values



fly_total_plots <- ggplot(fly_fitness_tidy, aes(x = gender, y = count, fill = treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = viridis_colors[c(3,6)], labels =  c("Conditioned", "Unconditioned")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Flies Emerged", 
       fill = "Treatment") +
  geom_point(aes(),
             size = 1,
             shape = 1,
             position = position_dodge(width=0.75)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  ylim(0,25)

fly_fitness_tidy_summary <- fly_fitness %>%  
  group_by(treatment) %>% 
  summarise(median = median(count, na.rm = TRUE),
            mean = mean(count, na.rm = TRUE),
            sd = sd(count, na.rm = TRUE),
            n = sum(!is.na(count)),
            se = sd/sqrt(n))

mean_count_by_vial_and_treatment <- fly_fitness_tidy %>%
  group_by(vial, treatment) %>%
  summarise(mean_count = mean(count, na.rm = TRUE))






ggplot(medians, aes(x = Gender, y = Total.median_count, fill = Gender, pattern = Gender))+ 
  # geom_jitter(aes(x = diet,
  #                 y = fly_numbers,
  #                 fill = diet),
  #             width = 0.1,
  #             shape = 1) +
  geom_boxplot()+
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       color = "black",
                       pattern_fill = "white",
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  geom_point(aes(),
             size = 1,
             shape = 1,
             position = position_jitterdodge()) +
  theme_classic()+
  labs(x = "Diet Condition",
       y = "Flies", 
       title = "")+
  scale_fill_manual(values = "red", "blue", "red", "blue") +  # Set fill colors for the boxplot
  scale_pattern_manual(values = c("stripe", "none", "stripe", "none")) +
  theme(legend.position = "none") +
  ylim(-0.01, 6)





# Calculate the total number of females for conditioned treatment
median_conditioned_females <- median(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females" & fly_fitness_tidy$treatment == "conditioned"], na.rm = TRUE)

# Calculate the total number of females for unconditioned treatment
median_unconditioned_females <- median(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females" & fly_fitness_tidy$treatment == "unconditioned"], na.rm = TRUE)


## Does this code right for Females v Males 


median_conditioned_males <- median(fly_fitness_tidy$count[fly_fitness_tidy$gender == "males" & fly_fitness_tidy$treatment == "conditioned"], na.rm = TRUE)

# Calculate the total number of females for unconditioned treatment
median_unconditioned_males <- median(fly_fitness_tidy$count[fly_fitness_tidy$gender == "males" & fly_fitness_tidy$treatment == "unconditioned"], na.rm = TRUE)



medians <- data.frame(
  Gender = c("Conditioned Females", "Unconditioned Females", "Conditioned Males", "Unconditioned Males"),
  Total = c(median_conditioned_females, median_unconditioned_females, median_conditioned_males, median_unconditioned_males)
)


medians$Gender <- factor(medians$Gender, levels = c("Conditioned Females", "Unconditioned Females", "Conditioned Males", "Unconditioned Males"))




median_plot <- ggplot(medians, aes(x = Gender, y = Total, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = viridis_colors[c(2,4,6,8)]) +
  labs(title = "",
       y = "Total Flies",
       x = "Sex and Treatment") +
  theme(legend.position = "none") +
  theme_classic()












# Calculate the mean number of females for conditioned treatment
mean_conditioned_females <- mean(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females" & fly_fitness_tidy$treatment == "conditioned"], na.rm = TRUE)

# Calculate the mean number of females for unconditioned treatment
mean_unconditioned_females <- mean(fly_fitness_tidy$count[fly_fitness_tidy$gender == "females" & fly_fitness_tidy$treatment == "unconditioned"], na.rm = TRUE)

# Calculate the mean number of males for conditioned treatment
mean_conditioned_males <- mean(fly_fitness_tidy$count[fly_fitness_tidy$gender == "males" & fly_fitness_tidy$treatment == "conditioned"], na.rm = TRUE)

# Calculate the mean number of males for unconditioned treatment
mean_unconditioned_males <- mean(fly_fitness_tidy$count[fly_fitness_tidy$gender == "males" & fly_fitness_tidy$treatment == "unconditioned"], na.rm = TRUE)

means <- data.frame(
  Gender = c("Conditioned Females", "Unconditioned Females", "Conditioned Males", "Unconditioned Males"),
  Total = c(mean_conditioned_females, mean_unconditioned_females, mean_conditioned_males, mean_unconditioned_males)
)

means$Gender <- factor(means$Gender, levels = c("Conditioned Females", "Unconditioned Females", "Conditioned Males", "Unconditioned Males"))

mean_plot <- ggplot(means, aes(x = Gender, y = Total, fill = Gender)) +
  geom_bar(stat = "identity") +
  geom_point() +
  scale_fill_manual(values = viridis_colors[c(2,4,6,8)]) +
  labs(title = "",
       y = "Total Flies",
       x = "Sex and Treatment") +
  theme(legend.position = "none") +
  theme_classic()










# fly_fitness_tidy_males <- tidyr::pivot_longer(data = fly_fitness ,
#                                                 cols = c(females, males),
#                                                 names_to = "gender",
#                                                 values_to = "count",
)
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



