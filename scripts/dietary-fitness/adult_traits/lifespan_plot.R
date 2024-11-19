## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(survminer)
library(survival)


## Reading the data in
lifespan_adultstraits <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx")
lifespan_adultstraits <- as.data.frame(lifespan_adultstraits)



## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)



## adding a sex section 
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")




# Plot
# Updated plot
lifespan_adultstraits_plot <- ggplot(lifespan_adultstraits, 
                                            aes(x = days_alive, y = treatment, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 4, 8, 8)], 
                    labels = c("Conditioned female", "Conditioned male", 
                               "Unconditioned female", "Unconditioned male")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Conditioning", 
       y = "deaths",
       fill = "Treatment") 

