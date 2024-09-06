#### Packages #### 
library(tidyverse)
library(lmerTest)
library(readxl)
library(MASS)
library(performance)
library(pscl)
library(DHARMa)
library(glmmTMB)
library(sjPlot)




pupae_flies <- overallflies_MFE %>%
  left_join(total_pupae, by = c("id", "vial", "treatment"))




survivability_between <- pupae_flies %>%
  mutate(survivability = (total_count / total_pupae) * 100)


pupae_flies <- ggplot(survivability_between, aes(x = treatment, y = survivability, fill = treatment)) +
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
       y = "Survivability (%) of Flies from the Pupal Stage", 
       fill = "Treatment") +
  ylim(0, 100)




## Saving a plot
ggsave(filename = "pupae_flies .png", 
       plot = pupae_flies , 
       width = 10, 
       height = 6, 
       dpi = 300)
