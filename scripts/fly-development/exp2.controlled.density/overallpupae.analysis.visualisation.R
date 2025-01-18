source("packages.R")










glmm.p.MFE.totalpupae <- glmmTMB(total_pupae ~ 
                                   
                                   treatment 
                                 
                                 
                                 + (1|vial) + (1|id),
                                 
                                 family = poisson, data = total_pupae)



#### Final Data analysis ####

# Simple analysus
summary(glmm.p.MFE.totalpupae)

# Confidence intervals 
exp(confint(glmm.p.MFE.totalpupae))


# getting values for the write-up
emmeans::emmeans(glmm.p.MFE.totalpupae, specs = ~ treatment, type = "response")


# Using a table to view the data
tab_model(glmm.p.MFE.totalpupae, CSS = list(css.table = '+font-family: Arial;'))
# looking okay



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
  ylim(0, 75)


## Saving a plot
ggsave(filename = "overall_emergence_treatment_MFE..png", 
       plot = overall_emergence_pupae, 
       width = 10, 
       height = 6, 
       dpi = 300)
