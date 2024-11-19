## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(patchwork)


## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)


## Reading pupae data in
reproductive_adultstraits <- read_excel("data/fitness_development/treatment_reproductive.xlsx")
reproductive_adultstraits <- as.data.frame(reproductive_adultstraits)

## adding a sex section 
reproductive_adultstraits$Conditioning <- ifelse(grepl("Conditioned", reproductive_adultstraits$treatment), "Conditioned", "Unconditioned")
reproductive_adultstraits$Sex <- ifelse(grepl("female", reproductive_adultstraits$treatment), "Focal female", "Focal male")


# Plot
# Updated plot
reproductive_boxplot_adultstraits <- ggplot(reproductive_adultstraits, 
                                            aes(x = Conditioning, y = offspring, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 4, 8, 8)], 
                    labels = c("Conditioned female", "Conditioned male", 
                               "Unconditioned female", "Unconditioned male")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_text(face = "bold")) +
  labs(x = "Conditioning", 
       y = "Number of pupae emerged",
       fill = "Treatment") +
  facet_wrap(~Sex)



#### SECOND ####

## Reading pupae data in
reproductive_adultstraits.2 <- read_excel("data/fitness_development/reproductive.count.2.xlsx")
reproductive_adultstraits.2 <- as.data.frame(reproductive_adultstraits.2)

## adding a sex section 
reproductive_adultstraits.2$Conditioning <- ifelse(grepl("Conditioned", reproductive_adultstraits.2$treatment), "Conditioned", "Unconditioned")
reproductive_adultstraits.2$Sex <- ifelse(grepl("female", reproductive_adultstraits.2$treatment), "Focal female", "Focal male")


# Plot
# Updated plot
reproductive_boxplot_adultstraits.2 <- ggplot(reproductive_adultstraits.2, 
                                            aes(x = Conditioning, y = offspring, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 4, 8, 8)], 
                    labels = c("Conditioned female", "Conditioned male", 
                               "Unconditioned female", "Unconditioned male")) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_text(face = "bold")) +
  labs(x = "Conditioning", 
       y = "Number of pupae emerged",
       fill = "Treatment") +
  facet_wrap(~Sex)


reproductive_boxplot_adultstraits + reproductive_boxplot_adultstraits.2 


