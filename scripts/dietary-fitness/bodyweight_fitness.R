## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)

viridis_colours <- viridis(10)

## Reading body weight data in
bodyweight <- read_excel("data/fitness_development/bodyweight_flies.xlsx")

##

## Multiplying the data by * 1000 so it can be visualised 
bodyweight$weight_mg <- bodyweight$weight_mg * 1000


## Visualising the data
bodyweight_plot <- ggplot(bodyweight, aes(x = sex, y = weight_mg, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(height=0, width=0.2), size = 0.5, colour = "black") +
  scale_y_continuous(breaks=seq(0,10,2)) +
  theme_classic() +
  scale_fill_manual(values = viridis_colours[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme(legend.position = "right") +
  labs(x = "Sex", 
       y = "Body Weight (μg) of fly") +
  labs(fill = "Treatment")+
  ylim(0,700)






