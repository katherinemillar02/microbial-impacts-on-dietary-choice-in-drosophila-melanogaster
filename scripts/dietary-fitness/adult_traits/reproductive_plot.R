## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(patchwork)


## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)


## Reading pupae data in
reproductive_adultstraits_f <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "females")


reproductive_adultstraits_f$day <- as.character(reproductive_adultstraits_f$day)



# Plot
# Updated plot
reproductive_boxplot_adultstraits_f <- ggplot(reproductive_adultstraits_f, 
                                            aes(x = day, y = os, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], 
                    labels = c("Conditioned female", "Conditioned male", 
                               "Unconditioned female", "Unconditioned male")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_text(face = "bold")) +
  labs(x = "Day in experiment when eggs were laid", 
       y = "Offspring",
       fill = "Treatment",
         title = "Female focal") 

reproductive_boxplot_adultstraits_f


## male 
## Reading pupae data in
reproductive_adultstraits_m <- read_excel("data/fitness_development/treatment_reproductive.xlsx", sheet = "males")


reproductive_adultstraits_m$day <- as.character(reproductive_adultstraits_m$day)



# Plot
# Updated plot
reproductive_boxplot_adultstraits_m <- ggplot(reproductive_adultstraits_m, 
                                              aes(x = day, y = os, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], 
                    labels = c("Conditioned female", "Conditioned male", 
                               "Unconditioned female", "Unconditioned male")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_text(face = "bold")) +
  labs(x = "Day in experiment when eggs were laid", 
       y = "Offspring",
       fill = "Treatment",
       title = "Male focal") 

reproductive_boxplot_adultstraits_m
