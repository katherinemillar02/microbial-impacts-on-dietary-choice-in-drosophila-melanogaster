## Packages
library(ggpubr)
source("packages.R")
source("scripts/dietary-fitness/adult_traits/data_visualisation/reproductive_plot.R")

reproductive_adultstraits_m

rs_lm_1 <- lm(os ~ treatment * day, data = reproductive_adultstraits_m)

summary(rs_lm_1)
