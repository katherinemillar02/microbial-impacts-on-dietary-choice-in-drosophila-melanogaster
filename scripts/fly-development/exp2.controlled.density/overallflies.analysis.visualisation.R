########################
source("packages.R")
########################




#### Final model: Zero Inflated Poisson ####
glm.zi.p.MFE.flies <- glmmTMB(
  total_count ~ sex * treatment + (1 | vial) + (1 | id),  
  ziformula = ~ sex * treatment,               
  family = poisson(),                          
  data = overallflies_MFE
)

# Testing interaction effect
drop1(glm.zi.p.MFE.flies, test = "Chisq")
 ## interaction effect not needed


# Model without interaction effect
glm.zi.p.MFE.flies.2 <- glmmTMB(
  total_count ~ sex + treatment + (1 | vial) + (1 | id),  
  ziformula = ~ sex + treatment,               
  family = poisson(),                          
  data = overallflies_MFE
)



#### DATA ANALYSIS ####

# Basic analysis
summary(glm.zi.p.MFE.flies.2)

# Confidence intervals
exp(confint(glm.zi.p.MFE.flies.2))

# Values for analysis write-up
emmeans::emmeans(glm.zi.p.MFE.flies.2, specs = ~ sex + treatment, type = "response")

# Table for write-up
tab_model(glm.zi.p.MFE.flies.2, CSS = list(css.table = '+font-family: Arial;'))




### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

# Choosing nice colours
viridis_colors <- viridis(10)

# Coding for data visualisation
overall_emergence_treatment <- ggplot(overallflies_MFE, aes(x = sex, y = total_count, fill = treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, position = position_dodge(width = 0.8)) + 
  geom_point(aes(fill = treatment), 
             size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  scale_x_discrete(labels = c("Females", "Males")) + 
  theme_classic() +
  scale_fill_manual(values = viridis_colors[c(4, 8)], labels = c("Conditioned", "Unconditioned")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.direction = "vertical") +
  labs(x = "Treatment", 
       y = "Number of Flies Emerged", 
       fill = "Treatment") +
  ylim(0, 30)


## Saving a plot
ggsave(filename = "overall_emergence_treatment.png", 
       plot = overall_emergence_treatment, 
       width = 10, 
       height = 6, 
       dpi = 300)




