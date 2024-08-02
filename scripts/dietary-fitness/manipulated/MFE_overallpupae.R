## Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)



## Reading pupae data in
pupae_fitness_MFE <- read_excel("data/fitness_development/MFE_pupae.xlsx")
pupae_fitness_MFE <- as.data.frame(pupae_fitness_MFE)

#### Pupae data check. 
total_pupae <- pupae_fitness_MFE %>% 
  group_by(id, vial, treatment) %>% 
  summarise(total_pupae = sum(pupae, na.rm = TRUE))




# Model 1 - Poisson GLMM
glmm.p.MFE.totalpupae <- glmmTMB(total_pupae ~ 
                              
                              treatment 
                              
                              
                              + (1|vial) + (1|id),
                            
                            family = poisson, data = total_pupae)


# Data analysis
summary(glmm.p.MFE.totalpupae)

# Using a table to view the data
tab_model(glmm.p.MFE.totalpupae)




## DATA VISUALISATION ##
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

overall_emergence_pupae <- ggplot(total_pupae, aes(x = treatment, y = total_pupae, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.75)) + 
  geom_point(aes(fill = treatment), 
             size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  scale_x_discrete(labels = c("Conditioned", "Unconditioned")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Treatment", 
       y = "Number of Pupae Emerged", 
       fill = "Treatment") +
  ylim(0, 60)


## Saving a plot
ggsave(filename = "overall_emergence_treatment_MFE..png", 
       plot = overall_emergence_pupae, 
       width = 10, 
       height = 6, 
       dpi = 300)

