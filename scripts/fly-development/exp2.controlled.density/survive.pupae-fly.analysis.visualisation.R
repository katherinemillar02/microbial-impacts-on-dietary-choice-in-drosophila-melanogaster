source("packages.R")
source("scripts/fly-development/exp2.controlled.density/c.density.dataread.R")
########################


## Poisson Negative Binomial ##
glm.zi.nb.MFE.surviveboth <- glmmTMB(
  survivability ~  treatment + (1 | vial),
  ziformula =  ~ treatment,
  family = nbinom2(),
  data = survivability_between
)




#### Data Analysis ####

# Basic analysis
summary(glm.zi.nb.MFE.surviveboth)

# condidence intervals 
exp(confint(glm.zi.nb.MFE.surviveboth))

# getting values for the write-up
emmeans::emmeans(glm.zi.nb.MFE.surviveboth, specs = ~ treatment, type = "response")

# getting a table for the write up
tab_model(glm.zi.nb.MFE.surviveboth, CSS = list(css.table = '+font-family: Arial;'))



##### Ideas for diff erent analysis: 







############################## 



## Getting colours 
viridis_colors <- viridis(10)


## The plot code: 
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


## Run plot: 
pupae_flies


## Saving a plot: 
ggsave(filename = "pupae_flies .png", 
       plot = pupae_flies , 
       width = 10, 
       height = 6, 
       dpi = 300)

