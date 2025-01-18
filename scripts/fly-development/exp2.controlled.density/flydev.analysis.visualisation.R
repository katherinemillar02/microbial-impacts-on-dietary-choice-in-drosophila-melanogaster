source("packages.R") 





#### Chosen model: NegBin ####
glm.nb.MFE.fly.2 <- glm.nb(time_hours ~
                           
                           treatment + sex ,
                         
                         data = fly_fitness_tidy_MFE_2)




#### Data Analysis for write-up #### 

# Basic analysis 
summary(glm.nb.MFE.fly.2)

# Real values for write-up
emmeans::emmeans(glm.nb.MFE.fly.2, specs = ~ sex + treatment, type = "response")

# Confidence intervals
exp(confint(glm.nb.MFE.fly.2))

# Table for write-up 
tab_model(glm.nb.MFE.fly.2, CSS = list(css.table = '+font-family: Arial;'))



## Choosing colours from viridis to use: 
viridis_colors <- viridis(10)


# Subsetting a female dataset
females_data <- subset(fly_fitness_MFE, select = c(time_hours, females, treatment))


## Boxplot ## for females
female_boxplot_2 <- ggplot(females_data, aes(x = factor(time_hours), y = females, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(females_data$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since L1 on diets", 
       y = "Number of Females Emerged",
       fill = "Treatment")


## Subsetting a male dataset
males_data <- subset(fly_fitness_MFE, select = c(time_hours, males, treatment))

## Boxplot ## for males 
male_boxplot_2 <- ggplot(males_data, aes(x = factor(time_hours), y = males, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = .4, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = treatment),
             size = 1.5, shape = 21,
             position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9)) +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  scale_x_discrete(labels = unique(males_data$time_hours)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical",
        strip.placement = "outside", 
        strip.background = element_blank(),  
        strip.text = element_blank()) +
  labs(x = "Time (hours) since L1 on diets", 
       y = "Number of Males Emerged",
       fill = "Treatment")


## Using patchwork to combine the female and male plots
MFE_flies <- female_boxplot_2 / 
  male_boxplot_2



## Saving the plot
ggsave(filename = "MFE_flies.png", 
       plot = MFE_flies, 
       width = 10, 
       height = 6, 
       dpi = 300)




