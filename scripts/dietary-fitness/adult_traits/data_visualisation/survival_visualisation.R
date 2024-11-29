## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(survminer)
library(survival)


## Reading the data in
lifespan_adultstraits <- read_excel("data/fitness_development/adulttraits_lifespan.xlsx")






## adding a sex section 
lifespan_adultstraits$Conditioning <- ifelse(grepl("Conditioned", lifespan_adultstraits$treatment), "Conditioned", "Unconditioned")
lifespan_adultstraits$Sex <- ifelse(grepl("female", lifespan_adultstraits$treatment), "Focal female", "Focal male")



## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)

## messing around
boxplot_adulttraits_survival <- ggplot(lifespan_adultstraits, aes(x = days_alive, y = treatment, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 4, 8, 8)], labels = c("Conditioned", "Unconditioned")) +
  #scale_x_discrete(labels = unique(lifespan_adultstraits$days_alive)) +
  theme_classic() +
  theme(legend.position = "none",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Days alive", 
       y = "Treatment",
       fill = "Treatment")+
  coord_flip()

boxplot_adulttraits_survival
