#### MFE Pupae Analysis ####
source("packages.R")



## Using Negative Binomial GLM (FOR NOW)... 
glm.nb_pupae <- glm.nb(time_hours  ~ 
                         treatment  
                       
                       , data = pupae_fitness_MFE_2)



#### 2. Data analysis with chosen model ####

# Basic analysis 
summary(glm.nb_pupae)

# Confidence intervals
exp(confint(glm.nb_pupae))

# Getting real values for write-up 
emmeans::emmeans(glm.nb_pupae, specs =  ~ treatment, type = "response")


# Generating a table 
tab_model(glm.nb_pupae, CSS = list(css.table = '+font-family: Arial;'))


## Boxplot ##

viridis_colors <- viridis(10)

###

pupae_boxplot_MFE <- ggplot(pupae_fitness_MFE, aes(x = factor(time_hours), y = pupae, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(pupae_fitness_MFE$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since eggs laid", 
       y = "Number of pupae emerged",
       fill = "Treatment")


## Display the plot 
pupae_boxplot_MFE

## Saving a plot
ggsave(filename = "pupae_boxplot_MFE.png", 
       plot = pupae_boxplot_MFE, 
       width = 10, 
       height = 6, 
       dpi = 300)






