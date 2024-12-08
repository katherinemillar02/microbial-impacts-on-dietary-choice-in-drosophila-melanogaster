## Packages
library(ggpubr)
source("packages.R")
source("scripts/dietary-fitness/adult_traits/data_visualisation/reproductive_plot.R")

reproductive_adultstraits_m

# Beginning a basic linear model 
rs_lm_1 <- lm(os ~ treatment * day, data = reproductive_adultstraits_m)



# Checking this data 
summary(rs_lm_1)
