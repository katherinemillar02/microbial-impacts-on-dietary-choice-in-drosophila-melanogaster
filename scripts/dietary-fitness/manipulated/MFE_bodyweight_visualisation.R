#### Packages ####
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)


# Choosing a colour palette.
viridis_colours <- viridis(10)


# Reading data in 
bodyweight_MFE <- read_excel("data/fitness_development/weighing_body.xlsx")



## Multiplying the data by * 1000 so it can be visualised 
bodyweight_MFE$weight_mg <- bodyweight_MFE$weight_mg * 1000




## Visualising the data
bodyweight_plot_MFE <- ggplot(bodyweight_MFE, aes(x = sex, y = weight_mg, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colours[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical")+
  labs(x = "Sex", 
       y = "Body Weight (μg) of fly") +
  labs(fill = "Treatment")+
  ylim(0,700)


## Displaying plot 
bodyweight_plot_MFE 



## Saving a plot
ggsave(filename = "bodyweight_plot_MFE.png", 
       plot = bodyweight_plot_MFE, 
       width = 10, 
       height = 6, 
       dpi = 300)








## Unused data 

# ## Reading body weight data in
# bodyweight <- read_excel("data/fitness_development/bodyweight_flies.xlsx")
# 
# ## Visualising the data
# bodyweight_plot <- ggplot(bodyweight, aes(x = sex, y = weight_mg, fill = treatment)) +
#   geom_boxplot() +
#   geom_point(aes(),
#              size = 1,
#              shape = 16,
#              position = position_jitter(width = 0.4)) +
#   scale_y_continuous(breaks=seq(0,10,2)) +
#   scale_x_discrete(labels = c("Females", "Males")) + 
#   theme_classic() +
#   scale_fill_manual(values = viridis_colours[c(4,8)], labels =  c("Conditioned", "Unconditioned")) +
#   theme(legend.position = "none") +
#   labs(x = "Sex", 
#        y = "Body Weight (μg) of fly") +
#   labs(fill = "Treatment")+
#   ylim(0,700)

