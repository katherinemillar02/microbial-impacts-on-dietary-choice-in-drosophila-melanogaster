## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)



## Reading body weight data in
bodyweight <- read_excel("data/bodyweight_flies.xlsx")

##

bodyweight$weight_mg <- bodyweight$weight_mg * 10 

bodyweight_plot <- ggplot(bodyweight, aes(x = sex, y = weight_mg, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(height=0, width=0.2)) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme(legend.position = "right") +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of Males Emerged") +
  labs(fill = "Treatment")+
  ylim(0,7)



